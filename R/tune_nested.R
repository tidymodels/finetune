#' Nested resampling for estimating tuning performance
#'
#' Using the same model tuning results to find optimal settings _and_ to measure
#' optimal performance might lead to optimistic performance estimates.
#' [tune_nested()] resamples the entire tuning process and uses out-of-sample
#' data to compute performance statistics.
#'
#' @inheritParams tune::tune_grid
#' @param resamples An object produced by [rsample::nested_cv()].
#' @param fn A character string for the procedure used to tune the model.
#' Possible values are: `"tune_bayes"`, `"tune_grid"`, `"tune_race_anova"`,
#' `"tune_race_win_loss"`, or `"tune_sim_anneal"`.
#' @param nest_control An object used to modify the tuning process.
#' @param ... Other options to pass to the tuning function (e.g., `metrics` etc).
#' @return An object of class `"nested_results"`.
#' @seealso [tune_bayes](), [tune_grid](), [tune_race_anova](),
#' [tune_race_win_loss](), [tune_sim_anneal]()

#' @export
tune_nested <- function(object, ...) {
  UseMethod("tune_nested")
}

# TODO have resamples use the same `data` arg but with new indicies so that we save
# memory as well as being able to match .row on the inner to the outer
# TODO avoid warnings with validation split
# TODO eval_time args

#' @export
tune_nested.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [tune_nested()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
#' @rdname tune_nested
tune_nested.model_spec <- function(object, preprocessor, resamples,
                                   ..., fn = "tune_grid",
                                   nest_control = control_nested()) {
  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    # TODO or add_variables
    cli::cli_abort("To tune a model spec, you must preprocess with a formula or recipe")
  }

  # nest_control <- parsnip::condense_control(nest_control, control_grid())

  wflow <- add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- add_formula(wflow, preprocessor)
  } # TODO add_variables()

  tune_nested(
    wflow,
    resamples = resamples,
    ..., fn = fn,
    nest_control = nest_control
  )
}

#' @export
#' @rdname tune_nested
tune_nested.workflow <- function(object, resamples, ..., fn = "tune_grid",
                                 nest_control = control_nested()) {

  res <-
    tune_nested_workflow(
      object,
      resamples = resamples,
      fn = fn,
      nest_control = nest_control,
      ...
    )
  .stash_last_result(res)
  res
}

# ------------------------------------------------------------------------------

tune_nested_workflow <- function(workflow, resamples, ..., fn = "tune_grid",
                                 nest_control = control_nested()) {
  opts <- list(...)

  res <- resamples
  B <- nrow(resamples)

  res$result <- purrr::map(resamples$inner_resamples, ~ tune_wrapper(.x, fn, workflow, opts))

  res$.notes <- purrr::map(res$result, tune::collect_notes)

  # ------------------------------------------------------------------------------
  # extract some objects from tuning results

  # TODO find first non-failed results
  ind <- 1
  first_res <- res$result[[ind]]
  mtr_set   <- tune::.get_tune_metrics(first_res)
  mtr_name  <- tune::.get_tune_metric_names(first_res)[1]
  mtr_info  <- tune::metrics_info(mtr_set)
  prm_names <- tune::.get_tune_parameter_names(first_res)
  evl_time  <- tune::.get_tune_eval_time_target(first_res)
  y_name    <- tune::outcome_names(first_res)

  # ------------------------------------------------------------------------------

  # TODO wrappers for everything to deal with failed tuning results
  # TODO how to pass best function? In control? Default to select best with first metric value/eval_time
  res$.selected <- purrr::map(res$result, tune::select_best, metric = mtr_name, eval_time = evl_time)

  if ( !nest_control$save_inner_metrics ) {
    res$.metrics_inner <- purrr::map(res$result, tune::collect_metrics)
  }

  res$wflow <- purrr::map(res$.selected, ~ tune::finalize_workflow(workflow, .x))

  # TODO trim opts to only use fit_resamples arguments
  res$fits <- purrr::map2(res$wflow, resamples$splits, ~ fit(.x, rsample::analysis(.y)))

  res$.predictions <- purrr::map2(res$fits, resamples$splits, ~ nested_predictions(.x, .y, y_name))
  res$.predictions <- purrr::map2(res$.predictions, recipes::names0(B, "nested_"), add_config)

  res$.metrics <-
    purrr::map(
      res$.predictions,
      ~ tune::.estimate_metrics(
        .x,
        metric = mtr_set,
        param_names = NULL,
        outcome_name = y_name,
        event_level = "first",
        metrics_info = mtr_info
      ) %>%
        dplyr::mutate(.config = "selected")
    )

  if ( !is.null(nest_control$extract) ) {
    res$.extracts <- purrr::map(res$fits, nest_control$extract)
  }

  res <- res %>% dplyr::select(-result, -wflow, -fits)

  if ( !nest_control$save_pred ) {
    res <- res %>% dplyr::select(-.predictions)
  }

  # TODO save attributes like other tune_results
  class(res) <- c("nested_results", "tune_results", class(res))
  res
}

# ------------------------------------------------------------------------------

is_result_failure <- function(x) {
  # TODO ?
}

nested_predictions <- function(model, split, y_name) {
  res <-
    generics::augment(model, rsample::assessment(split)) %>%
    dplyr::select(dplyr::all_of(y_name), dplyr::starts_with(".pred"))
  res$.row <- as.integer(split, data = "assessment")
  res
}

add_config <- function(preds, lab) {
  preds$.config <- lab
  preds
}

tune_wrapper <- function(rs, fn, object, opts) {
  # TODO verboseness from nest_control?
  # TODO set the namespace too
  cl <- rlang::call2(.fn = fn, object = expr(object), resamples = expr(rs))
  if ( length(opts) > 0 ) {
    nsm <- names(opts)
    opt_sym <- syms(nsm)
    names(opt_sym) <- nsm
  }
  # TODO this dputs the whole object to the call; find a way to avoid that
  cl <- rlang::call_modify(cl, !!!opt_sym)
  rlang::eval_tidy(cl, data = opts)
}

# ------------------------------------------------------------------------------

# TODO check/ignore/warn about duplicate control options in the inner functions
# such as allow_parallel, save_pred, etc.
# TODO save_results?


#' Control aspects of the nested resampling
#'
#' @inheritParams tune::control_bayes
#' @param save_inner_metrics A logical to save the entire set of resampling
#' results for the inner results (e.g., the results of [tune::collect_metrics()]).
#' @export
control_nested <- function(verbose = FALSE, extract = NULL,
                           save_pred = FALSE,
                           save_workflow = FALSE,
                           save_inner_metrics = FALSE,
                           event_level = "first",
                           select = NULL,
                           pkgs = NULL, allow_par = TRUE, parallel_over = NULL,
                           backend_options = NULL) {

  # Any added arguments should also be added in superset control functions
  # in other packages

  # TODO add options for all other tune_* functions
  # TODO use rlang type checkers

  # val_class_and_single(verbose, "logical", "control_nested()")
  # val_class_and_single(allow_par, "logical", "control_nested()")
  # val_class_and_single(save_pred, "logical", "control_nested()")
  # val_class_and_single(save_workflow, "logical", "control_nested()")
  # val_class_and_single(event_level, "character", "control_nested()")
  # val_class_or_null(pkgs, "character", "control_nested()")
  # val_class_or_null(extract, "function", "control_nested()")
  # val_parallel_over(parallel_over, "control_nested()")
  # check select as function

  res <- list(
    verbose = verbose,
    allow_par = allow_par,
    extract = extract,
    save_pred = save_pred,
    save_inner_metrics = save_inner_metrics,
    select = select,
    pkgs = pkgs,
    save_workflow = save_workflow,
    event_level = event_level,
    parallel_over = parallel_over,
    backend_options = backend_options
  )

  class(res) <- c("control_nested")
  res
}
