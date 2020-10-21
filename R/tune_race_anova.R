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
#' @references
#' Kuhn, M 2014. "Futility Analysis in the Cross-Validation of Machine Learning
#' Models." \url{http://arxiv.org/abs/1405.6974.}
#' @details
#' The technical details of this method are described in Kuhn (2014).
#'
#'   Racing methods are efficient approaches to grid search. Initially, the
#'  function evaluates all tuning parameters on a small initial set of
#'  resamples. The `burn_in` argument of `control_race()` sets the number of
#'  initial resamples.
#'
#'   The performance statistics from these resamples are analyzed to determine
#'  which tuning parameters are _not_ statistically different from the current
#'  best setting. If a parameter is statistically different, it is excluded from
#'  further resampling.
#'
#'   The next resample is used with the remaining parameter combinations and the
#'  statistical analysis is updated. More candidate parameters may be excluded
#'  with each new resample that is processed.
#'
#'   This function determines statistical significance using a simple ANOVA
#'  model where the performance statistic (e.g., RMSE, accuracy, etc.) is the
#'  outcome data. The `control_race()` function contains are parameter for the
#'  significance cutoff applied to the ANOVA results as well as other relevant
#'  arguments.
#'
#'   There is benefit to using racing methods in conjunction with parallel
#'  processing. The following section shows a benchmark of results for one
#'  dataset and model.
#'
#' @includeRmd  man/rmd/anova-benchmark.md details
#' @examples
#' \donttest{
#' library(parsnip)
#' library(rsample)
#' library(discrim)
#' library(dials)
#'
#' ## -----------------------------------------------------------------------------
#'
#' data(two_class_dat, package = "modeldata")
#'
#' set.seed(6376)
#' rs <- bootstraps(two_class_dat, times = 10)
#'
#' ## -----------------------------------------------------------------------------
#'
#' # optimize an regularized discriminant analysis model
#' rda_spec <-
#'   discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
#'   set_engine("klaR")
#'
#' ## -----------------------------------------------------------------------------
#'
#' set.seed(11)
#' grid_anova <- rda_spec %>% tune_race_anova(Class ~ ., resamples = rs, grid = 10)
#'
#' show_best(grid_anova, metric = "roc_auc", n = 2)
#' }
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
                                   control = control_race()) {

  tune::empty_ellipses(...)

  tune_race_anova(model, preprocessor = object, resamples = resamples,
                  param_info = param_info, grid = grid,
                  metrics = metrics, control = control)
}

#' @export
tune_race_anova.formula <- function(formula, model, resamples, ..., param_info = NULL,
                                    grid = 10, metrics = NULL,
                                    control = control_race()) {
  tune::empty_ellipses(...)

  tune_race_anova(model, preprocessor = formula, resamples = resamples,
                  param_info = param_info, grid = grid,
                  metrics = metrics, control = control)
}

#' @export
#' @rdname tune_race_anova
tune_race_anova.model_spec <- function(object, preprocessor, resamples, ...,
                                       param_info = NULL, grid = 10, metrics = NULL,
                                       control = control_race()) {

  if (rlang::is_missing(preprocessor) || !tune::is_preprocessor(preprocessor)) {
    rlang::abort(paste("To tune a model spec, you must preprocess",
                       "with a formula or recipe"))
  }

  tune::empty_ellipses(...)

  wflow <- workflows::add_model(workflows::workflow(), object)

  if (tune::is_recipe(preprocessor)) {
    wflow <- workflows::add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- workflows::add_formula(wflow, preprocessor)
  }

  tune_race_anova_workflow(
    wflow,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    param_info = param_info,
    control = control
  )
}

#' @export
#' @rdname tune_race_anova
tune_race_anova.workflow <- function(object, resamples, ..., param_info = NULL,
                                     grid = 10, metrics = NULL,
                                     control = control_race()) {

  tune::empty_ellipses(...)

  tune_race_anova_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    param_info = param_info,
    control = control
  )
}

## -----------------------------------------------------------------------------

tune_race_anova_workflow <-
  function(object, resamples, param_info = NULL, grid = 10, metrics = NULL,
           control = control_race()) {

    B <- nrow(resamples)
    if (control$randomize) {
      resamples <-
        resamples %>% dplyr::arrange(runif(B))
    }
    resamples <- dplyr::mutate(resamples, .order = dplyr::row_number())

    min_rs <- control$burn_in
    tmp_resamples <- restore_rset(resamples, 1:min_rs)

    control$pkgs <- c(tune::required_pkgs(object), "workflows", "tidyr", "rlang")

    res <-
      object %>%
      tune::tune_grid(
        tmp_resamples,
        param_info = param_info,
        grid = grid,
        metrics = metrics,
        control = control
      )

    param_names <- tune::.get_tune_parameter_names(res)
    metrics     <- tune::.get_tune_metrics(res)
    analysis_metric <- names(attr(metrics, "metrics"))[1]
    analysis_max    <- attr(attr(metrics, "metrics")[[1]], "direction") == "maximize"

    cols <- tune::get_tune_colors()
    if (control$verbose_elim) {
      msg <-
        paste("Racing will", ifelse(analysis_max, "maximize", "minimize"),
              "the", analysis_metric, "metric.")
      rlang::inform(cols$message$info(paste0(cli::symbol$info, " ", msg)))
      if (control$randomize) {
        msg <- "Resamples are analyzed in a random order."
        rlang::inform(cols$message$info(paste0(cli::symbol$info, " ", msg)))
      }
    }

    filters_results <- test_parameters_gls(res, control$alpha)
    n_grid <- nrow(filters_results)

    log_final <- TRUE
    num_ties <- 0
    for(rs in (min_rs + 1):B) {
      if (sum(filters_results$pass) == 2) {
        num_ties <- num_ties + 1
      }
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
          param_info = param_info,
          grid = new_grid,
          metrics = metrics,
          control = control
        )
      res <- restore_tune(res, tmp_res)


      if (nrow(new_grid) > 1) {
        filters_results <- test_parameters_gls(res, control$alpha)
        if (sum(filters_results$pass) == 2 & num_ties >= control$num_ties) {
          filters_results <- tie_breaker(res, control)
        }
      }
    }

    res
  }
