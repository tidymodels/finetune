#' Model tuning via grid search
#'
#' [tune_race_compete()] computes a set of performance metrics (e.g. accuracy or RMSE)
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
tune_race_compete <- function(object, ...) {
  UseMethod("tune_race_compete")
}

#' @export
tune_race_compete.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [tune_race_compete()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
tune_race_compete.recipe <- function(object, model, resamples, ..., param_info = NULL,
                                   grid = 10, metrics = NULL, control = control_race()) {

  tune:::empty_ellipses(...)

  tune_race_compete(model, preprocessor = object, resamples = resamples,
                  param_info = param_info, grid = grid,
                  metrics = metrics, control = control)
}

#' @export
tune_race_compete.formula <- function(formula, model, resamples, ..., param_info = NULL,
                                    grid = 10, metrics = NULL, control = control_race()) {
  tune:::empty_ellipses(...)

  tune_race_compete(model, preprocessor = formula, resamples = resamples,
                  param_info = param_info, grid = grid,
                  metrics = metrics, control = control)
}

#' @export
#' @rdname tune_race_compete
tune_race_compete.model_spec <- function(object, preprocessor, resamples, ...,
                                       param_info = NULL, grid = 10, metrics = NULL,
                                       control = control_race()) {

  if (rlang::is_missing(preprocessor) || !is_preprocessor(preprocessor)) {
    rlang::abort(paste("To tune a model spec, you must preprocess",
                       "with a formula or recipe"))
  }

  tune:::empty_ellipses(...)

  wflow <- workflows::add_model(workflow(), object)

  if (is_recipe(preprocessor)) {
    wflow <- workflows::add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- workflows::add_formula(wflow, preprocessor)
  }

  tune_race_compete_workflow(
    wflow,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    param_info = param_info,
    control = control
  )
}

#' @export
#' @rdname tune_race_compete
tune_race_compete.workflow <- function(object, resamples, ..., param_info = NULL,
                                     grid = 10, metrics = NULL,
                                     control = control_race()) {

  tune:::empty_ellipses(...)

  tune_race_compete_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    param_info = param_info,
    control = control
  )
}

## -----------------------------------------------------------------------------

tune_race_compete_workflow <-
  function(object, resamples, param_info = NULL, grid = 10, metrics = NULL,
           control = control_race()) {

    B <- nrow(resamples)
    if (control$randomize) {
      resamples <- resamples %>% dplyr::arrange(runif(B))
    }

    min_rs <- control$burn_in
    tmp_resamples <- restore_rset(resamples, 1:min_rs)

    res <-
      object %>%
      tune::tune_grid(
        resamples = tmp_resamples,
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
      test_parameters_bt(res, param_names, analysis_metric, analysis_max, control$alpha)
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
          resamples = tmp_resamples,
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

