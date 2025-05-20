#' Efficient grid search via racing with ANOVA models
#'
#' [tune_race_anova()] computes a set of performance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data. After an initial number of
#'  resamples have been evaluated, the process eliminates tuning parameter
#'  combinations that are unlikely to be the best results using a repeated
#'  measure ANOVA model.
#'
#' @param object A `parsnip` model specification or a [workflows::workflow()].
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()]. This is only required when `object` is not a workflow.
#' @param resamples An `rset()` object that has multiple resamples (i.e., is not
#'  a validation set).
#' @param param_info A [dials::parameters()] object or `NULL`. If none is given,
#' a parameters set is derived from other arguments. Passing this argument can
#' be useful when parameter ranges need to be customized.
#' @param grid A data frame of tuning combinations or a positive integer. The
#'  data frame should have columns for each parameter being tuned and rows for
#'  tuning parameter candidates. An integer denotes the number of candidate
#'  parameter sets to be created automatically.
#' @param metrics A [yardstick::metric_set()] or `NULL`.
#' @param eval_time A numeric vector of time points where dynamic event time
#' metrics should be computed (e.g. the time-dependent ROC curve, etc). The
#' values must be non-negative and should probably be no greater than the
#' largest event time in the training set (See Details below).
#' @param control An object used to modify the tuning process. See
#'  [control_race()] for more details.
#' @param ... Not currently used.
#' @references
#' Kuhn, M 2014. "Futility Analysis in the Cross-Validation of Machine Learning
#' Models." \url{https://arxiv.org/abs/1405.6974}.
#' @details
#' The technical details of this method are described in Kuhn (2014).
#'
#' Racing methods are efficient approaches to grid search. Initially, the
#'  function evaluates all tuning parameters on a small initial set of
#'  resamples. The `burn_in` argument of [control_race()] sets the number of
#'  initial resamples.
#'
#' The performance statistics from these resamples are analyzed to determine
#'  which tuning parameters are _not_ statistically different from the current
#'  best setting. If a parameter is statistically different, it is excluded from
#'  further resampling.
#'
#' The next resample is used with the remaining parameter combinations and the
#'  statistical analysis is updated. More candidate parameters may be excluded
#'  with each new resample that is processed.
#'
#' This function determines statistical significance using a repeated measures ANOVA
#'  model where the performance statistic (e.g., RMSE, accuracy, etc.) is the
#'  outcome data and the random effect is due to resamples. The
#'  [control_race()] function contains are parameter for the significance cutoff
#'  applied to the ANOVA results as well as other relevant arguments.
#'
#' There is benefit to using racing methods in conjunction with parallel
#'  processing. The following section shows a benchmark of results for one
#'  dataset and model.
#'
#' ## Censored regression models
#'
#' With dynamic performance metrics (e.g. Brier or ROC curves), performance is
#' calculated for every value of `eval_time` but the _first_ evaluation time
#' given by the user (e.g., `eval_time[1]`) is analyzed during racing.
#'
#' Also, values of `eval_time` should be less than the largest observed event
#' time in the training data. For many non-parametric models, the results beyond
#' the largest time corresponding to an event are constant (or `NA`).
#'
#' @return An object with primary class `tune_race` in the same standard format
#' as objects produced by [tune::tune_grid()].
#' @includeRmd  man/rmd/anova-benchmark.md details
#' @examples
#' \donttest{
#' library(parsnip)
#' library(rsample)
#' library(dials)
#'
#' ## -----------------------------------------------------------------------------
#'
#' if (rlang::is_installed(c("discrim", "lme4", "modeldata"))) {
#'   library(discrim)
#'   data(two_class_dat, package = "modeldata")
#'
#'   set.seed(6376)
#'   rs <- bootstraps(two_class_dat, times = 10)
#'
#'   ## -----------------------------------------------------------------------------
#'
#'   # optimize an regularized discriminant analysis model
#'   rda_spec <-
#'     discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) |>
#'     set_engine("klaR")
#'
#'   ## -----------------------------------------------------------------------------
#'
#'   ctrl <- control_race(verbose_elim = TRUE)
#'   set.seed(11)
#'   grid_anova <-
#'     rda_spec |>
#'     tune_race_anova(Class ~ ., resamples = rs, grid = 10, control = ctrl)
#'
#'   # Shows only the fully resampled parameters
#'   show_best(grid_anova, metric = "roc_auc", n = 2)
#'
#'   plot_race(grid_anova)
#' }
#' }
#' @seealso [tune::tune_grid()], [control_race()], [tune_race_win_loss()]
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
  cli::cli_abort(msg)
}

#' @export
tune_race_anova.recipe <-
  function(
    object,
    model,
    resamples,
    ...,
    param_info = NULL,
    grid = 10,
    metrics = NULL,
    eval_time = NULL,
    control = control_race()
  ) {
    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

    tune_race_anova(
      model,
      preprocessor = object,
      resamples = resamples,
      param_info = param_info,
      grid = grid,
      metrics = metrics,
      eval_time = eval_time,
      control = control
    )
  }

#' @export
tune_race_anova.formula <-
  function(
    formula,
    model,
    resamples,
    ...,
    param_info = NULL,
    grid = 10,
    metrics = NULL,
    eval_time = NULL,
    control = control_race()
  ) {
    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

    tune_race_anova(
      model,
      preprocessor = formula,
      resamples = resamples,
      param_info = param_info,
      grid = grid,
      metrics = metrics,
      eval_time = eval_time,
      control = control
    )
  }

#' @export
#' @rdname tune_race_anova
tune_race_anova.model_spec <-
  function(
    object,
    preprocessor,
    resamples,
    ...,
    param_info = NULL,
    grid = 10,
    metrics = NULL,
    eval_time = NULL,
    control = control_race()
  ) {
    if (
      rlang::is_missing(preprocessor) || !tune::is_preprocessor(preprocessor)
    ) {
      cli::cli_abort(
        "To tune a model spec, you must preprocess with a formula, recipe, \\
        or variable specification."
      )
    }

    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

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
      eval_time = eval_time,
      param_info = param_info,
      control = control
    )
  }

#' @export
#' @rdname tune_race_anova
tune_race_anova.workflow <-
  function(
    object,
    resamples,
    ...,
    param_info = NULL,
    grid = 10,
    metrics = NULL,
    eval_time = NULL,
    control = control_race()
  ) {
    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

    tune_race_anova_workflow(
      object,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      eval_time = eval_time,
      param_info = param_info,
      control = control
    )
  }

## -----------------------------------------------------------------------------

tune_race_anova_workflow <-
  function(
    object,
    resamples,
    param_info = NULL,
    grid = 10,
    metrics = NULL,
    eval_time = NULL,
    control = control_race(),
    call = caller_env()
  ) {
    rlang::check_installed("lme4")

    tune::initialize_catalog(control = control)

    B <- nrow(resamples)
    if (control$randomize) {
      resamples <- randomize_resamples(resamples)
    }
    resamples <- dplyr::mutate(resamples, .order = dplyr::row_number())

    min_rs <- control$burn_in
    check_num_resamples(B, min_rs)
    tmp_resamples <- restore_rset(resamples, 1:min_rs)

    metrics <- tune::check_metrics_arg(metrics, object, call = call)
    eval_time <- tune::check_eval_time_arg(eval_time, metrics, call = call)

    control$pkgs <- c(
      control$pkgs,
      tune::required_pkgs(object),
      "workflows",
      "tidyr",
      "rlang"
    )

    if (control$verbose_elim) {
      tune_cols <- tune::get_tune_colors()
      msg <- tune_cols$message$info(
        paste0(
          cli::symbol$info,
          " Evaluating against the initial {min_rs} burn-in resamples."
        )
      )

      cli::cli_inform(msg)
    }

    grid_control <- parsnip::condense_control(control, tune::control_grid())
    res <-
      object |>
      tune::tune_grid(
        tmp_resamples,
        param_info = param_info,
        grid = grid,
        metrics = metrics,
        eval_time = eval_time,
        control = grid_control
      )

    param_names <- tune::.get_tune_parameter_names(res)

    opt_metric <- tune::first_metric(metrics)
    opt_metric_name <- opt_metric$metric
    maximize <- opt_metric$direction == "maximize"

    opt_metric_time <- tune::first_eval_time(
      metrics,
      metric = opt_metric_name,
      eval_time = eval_time,
      call = call
    )

    racing_obj_log(
      opt_metric_name,
      opt_metric$direction,
      control,
      opt_metric_time
    )

    filters_results <- test_parameters_gls(res, control$alpha, opt_metric_time)
    n_grid <- nrow(filters_results)

    log_final <- TRUE
    num_ties <- 0
    for (rs in (min_rs + 1):B) {
      if (sum(filters_results$pass) == 2) {
        num_ties <- num_ties + 1
      }
      new_grid <-
        filters_results |>
        dplyr::filter(pass) |>
        dplyr::select(!!!param_names)

      if (nrow(new_grid) > 1) {
        tmp_resamples <- restore_rset(resamples, rs)
        log_racing(
          control,
          filters_results,
          res$splits,
          n_grid,
          opt_metric_name
        )
      } else {
        tmp_resamples <- restore_rset(resamples, rs:B)
        if (log_final) {
          log_racing(
            control,
            filters_results,
            res$splits,
            n_grid,
            opt_metric_name
          )
        }
        log_final <- FALSE
      }

      grid_control <- parsnip::condense_control(control, tune::control_grid())
      tmp_res <-
        object |>
        tune::tune_grid(
          tmp_resamples,
          param_info = param_info,
          grid = new_grid,
          metrics = metrics,
          eval_time = eval_time,
          control = grid_control
        )

      res <- restore_tune(res, tmp_res, opt_metric_time)

      if (nrow(new_grid) > 1) {
        filters_results <- test_parameters_gls(
          res,
          control$alpha,
          opt_metric_time
        )
        if (sum(filters_results$pass) == 2 & num_ties >= control$num_ties) {
          filters_results <- tie_breaker(
            res,
            control,
            eval_time = opt_metric_time
          )
        }
      } else {
        # Depending on the value of control$parallel_over we don't need to do
        # the remaining loop to get the rs counter to B
        max_B <- max(tune::collect_metrics(res)$n)
        if (max_B == B) {
          break()
        }
      }
    }

    .stash_last_result(res)

    res
  }

# fmt: skip
check_num_resamples <- function(B, min_rs) {
  if (B <= min_rs) {
    cli::cli_abort(
      paste0("The number of resamples (", B, ") needs to be more than the ",
             "number of burn-in resamples (", min_rs, ") set by the control ",
             "function `control_race()`."),
      call = NULL
    )
  }
  invisible(NULL)
}
