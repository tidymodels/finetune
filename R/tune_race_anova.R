#' Model tuning via grid search
#'
#' [tune_race_anova()] computes a set of performance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data.
#'
#' @param object A `parsnip` model specification or a [workflows::workflow()].
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()].
#' @param resamples An `rset()` object.
#' @param param_info A [dials::parameters()] object or `NULL`. If none is given,
#' a parameters set is derived from other arguments. Passing this argument can
#' be useful when parameter ranges need to be customized.
#' @param grid A data frame of tuning combinations or a positive integer. The
#'  data frame should have columns for each parameter being tuned and rows for
#'  tuning parameter candidates. An integer denotes the number of candidate
#'  parameter sets to be created automatically.
#' @param metrics A [yardstick::metric_set()] or `NULL`.
#' @param control An object used to modify the tuning process.
#' @param ... Not currently used.
#' @export
tune_race_anova <- function(object, ...) {
  UseMethod("tune_race_anova")
}

#' @export
tune_race_anova.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [tune_race_anova()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
tune_race_anova.recipe <- function(object, model, resamples, ..., param_info = NULL,
                                   grid = 10, metrics = NULL,
                                   control = control_race_anova()) {

  empty_ellipses(...)

  tune_race_anova(model, preprocessor = object, resamples = resamples,
                  param_info = param_info, grid = grid,
                  metrics = metrics, control = control)
}

#' @export
tune_race_anova.formula <- function(formula, model, resamples, ..., param_info = NULL,
                                    grid = 10, metrics = NULL,
                                    control = control_race_anova()) {
  empty_ellipses(...)

  tune_race_anova(model, preprocessor = formula, resamples = resamples,
                  param_info = param_info, grid = grid,
                  metrics = metrics, control = control)
}

#' @export
#' @rdname tune_race_anova
tune_race_anova.model_spec <- function(object, preprocessor, resamples, ...,
                                       param_info = NULL, grid = 10, metrics = NULL,
                                       control = control_race_anova()) {

  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    rlang::abort(paste("To tune a model spec, you must preprocess",
                       "with a formula or recipe"))
  }

  empty_ellipses(...)

  wflow <- workflows::add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- workflows::add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- workflows::add_formula(wflow, preprocessor)
  }

  tune_race_anova_workflow(
    wflow,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    pset = param_info,
    control = control
  )
}

#' @export
#' @rdname tune_race_anova
tune_race_anova.workflow <- function(object, resamples, ..., param_info = NULL,
                                     grid = 10, metrics = NULL,
                                     control = control_race_anova()) {

  empty_ellipses(...)

  tune_race_anova_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    pset = param_info,
    control = control
  )
}

## -----------------------------------------------------------------------------

tune_grid_race_anova <-
  function(object, resamples, ..., param_info = NULL, grid = 10, metrics = NULL,
           control = control_race()) {

    tune:::empty_ellipses(...)

    B <- nrow(resamples)
    if (control$randomize) {
      resamples <- resamples %>% dplyr::arrange(runif(B))
    }

    min_rs <- control$burn_in
    tmp_resamples <- restore_rset(resamples, 1:min_rs)

    res <-
      object %>%
      tune::tune_grid(
        tmp_resamples,
        ...,
        param_info = param_info,
        grid = grid,
        metrics = metrics,
        control = control
      )

    param_names <- tune::.get_tune_parameter_names(res)
    metrics     <- tune::.get_tune_metrics(res)
    analysis_metric <- names(attr(metrics, "metrics"))[1]
    analysis_max    <- attr(attr(metrics, "metrics")[[1]], "direction") == "maximize"

    filters_results <-
      test_parameters_gls(res, param_names, analysis_metric, analysis_max, control$alpha)
    n_grid <- nrow(filters_results)

    log_final <- TRUE
    for(rs in (min_rs + 1):B) {
      new_grid <-
        filters_results %>%
        dplyr::filter(pass) %>%
        dplyr::select(!!!param_names)

      if (nrow(new_grid) > 1) {
        tmp_resamples <- restore_rset(resamples, rs)
        log_racing(control, filters_results, res$splits, n_grid, analysis_metric)
      } else {
        tmp_resamples <- restore_rset(resamples, rs:B)
        if (log_final) {
          log_racing(control, filters_results, res$splits, n_grid, analysis_metric)
        }
        log_final <- FALSE
      }

      tmp_res <-
        object %>%
        tune::tune_grid(
          tmp_resamples,
          ...,
          param_info = param_info,
          grid = new_grid,
          metrics = metrics,
          control = control
        )
      res <- restore_tune(res, tmp_res)

      if (nrow(new_grid) > 1) {
        filters_results <-
          test_parameters_bt(res, param_names, analysis_metric, analysis_max, control$alpha)
      }
    }

    res
  }


## -----------------------------------------------------------------------------


#' Control aspects of the grid search racing process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#'   this argument, warnings and errors are always shown. If using a dark IDE
#'   theme, some logging messages might be hard to see. If this is the case,
#'   try setting the `tidymodels.dark` option with
#'   `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param no_improve The integer cutoff for the number of iterations without
#'   better results.
#' @param uncertain The number of iterations with no improvement before an
#'  uncertainty sample is created where a sample with high predicted variance is
#'  chosen (i.e., in a region that has not yet been explored). The iteration
#'  counter is reset after each uncertainty sample. For example, if `uncertain =
#'  10`, this condition is triggered every 10 samples with no improvement.
#' @param seed An integer for controlling the random number stream.
#' @param time_limit A number for the minimum number of _minutes_ (elapsed) that
#'   the function should execute. The elapsed time is evaluated at internal
#'   checkpoints and, if over time, the results at that time are returned (with
#'   a warning). This means that the `time_limit` is not an exact limit, but a
#'   minimum time limit.
#' @param extract An optional function with at least one argument (or `NULL`)
#'   that can be used to retain arbitrary objects from the model fit object,
#'   recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#'   be saved for each model _evaluated_.
#' @param pkgs An optional character string of R package names that should be
#'   loaded (by namespace) during parallel processing.
#' @param allow_par A logical to allow parallel processing (if a parallel
#'   backend is registered).
#'
#' @export
control_race <-
  function(verbose = FALSE, allow_par = TRUE, extract = NULL, save_pred = FALSE,
           burn_in = 3, alpha = 0.05, randomize = TRUE, pkgs = NULL) {

    tune:::val_class_and_single(verbose, "logical", "control_grid()")
    tune:::val_class_and_single(allow_par, "logical", "control_grid()")
    tune:::val_class_and_single(save_pred, "logical", "control_grid()")
    tune:::val_class_or_null(pkgs, "character", "control_grid()")
    tune:::val_class_or_null(extract, "function", "control_grid()")

    res <- list(
      verbose = verbose,
      allow_par = allow_par,
      extract = extract,
      save_pred = save_pred,
      alpha = alpha,
      burn_in = burn_in,
      randomize = randomize,
      pkgs = pkgs
    )

    class(res) <- c("control_race")
    res
  }

#' @export
print.control_race <- function(x, ...) {
  cat("Racing method control object\n")
  invisible(x)
}
