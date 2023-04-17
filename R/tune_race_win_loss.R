#' Efficient grid search via racing with win/loss statistics
#'
#' [tune_race_win_loss()] computes a set of performance metrics (e.g. accuracy or RMSE)
#'  for a pre-defined set of tuning parameters that correspond to a model or
#'  recipe across one or more resamples of the data. After an initial number of
#'  resamples have been evaluated, the process eliminates tuning parameter
#'  combinations that are unlikely to be the best results using a statistical
#'  model. For each pairwise combinations of tuning parameters, win/loss
#'  statistics are calculated and a logistic regression model is used to measure
#'  how likely each combination is to win overall.
#'
#' @param object A `parsnip` model specification or a [workflows::workflow()].
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()].
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
#' @param control An object used to modify the tuning process.
#' @references
#' Kuhn, M 2014. "Futility Analysis in the Cross-Validation of Machine Learning
#' Models." \url{https://arxiv.org/abs/1405.6974}.
#' @param ... Not currently used.
#' The technical details of this method are described in Kuhn (2014).
#'
#' Racing methods are efficient approaches to grid search. Initially, the
#'  function evaluates all tuning parameters on a small initial set of
#'  resamples. The `burn_in` argument of `control_race()` sets the number of
#'  initial resamples.
#'
#' The performance statistics from the current set of resamples are converted
#' to win/loss/tie results. For example, for two parameters (`j` and `k`) in a
#' classification model that have each been resampled three times:
#'
#' \preformatted{
#'             |  area under the ROC curve |
#'             -----------------------------
#'    resample | parameter j | parameter k | winner
#'    ---------------------------------------------
#'        1    |    0.81     |     0.92    |   k
#'        2    |    0.95     |     0.94    |   j
#'        3    |    0.79     |     0.81    |   k
#'    ---------------------------------------------
#' }
#'
#' After the third resample, parameter `k` has a 2:1 win/loss ratio versus `j`.
#' Parameters with equal results are treated as a half-win for each setting.
#' These statistics are determined for all pairwise combinations of the
#' parameters and a Bradley-Terry model is used to model these win/loss/tie
#' statistics. This model can compute the ability of a parameter combination to
#' win overall. A confidence interval for the winning ability is computed and
#' any settings whose interval includes zero are retained for future resamples
#' (since it is not statistically different form the best results).
#'
#'
#' The next resample is used with the remaining parameter combinations and the
#'  statistical analysis is updated. More candidate parameters may be excluded
#'  with each new resample that is processed.
#'
#' The `control_race()` function contains are parameter for the significance cutoff
#'  applied to the Bradley-Terry model results as well as other relevant arguments.
#' @examples
#' \donttest{
#' library(parsnip)
#' library(rsample)
#' library(dials)
#'
#' ## -----------------------------------------------------------------------------
#'
#' if (rlang::is_installed(c("discrim", "modeldata"))) {
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
#'     discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
#'     set_engine("klaR")
#'
#'   ## -----------------------------------------------------------------------------
#'
#'   ctrl <- control_race(verbose_elim = TRUE)
#'
#'   set.seed(11)
#'   grid_wl <-
#'     rda_spec %>%
#'     tune_race_win_loss(Class ~ ., resamples = rs, grid = 10, control = ctrl)
#'
#'   # Shows only the fully resampled parameters
#'   show_best(grid_wl, metric = "roc_auc")
#'
#'   plot_race(grid_wl)
#' }
#' }
#' @return An object with primary class `tune_race` in the same standard format
#' as objects produced by [tune::tune_grid()].
#' @seealso [tune::tune_grid()], [control_race()], [tune_race_anova()]
#' @export
tune_race_win_loss <- function(object, ...) {
  UseMethod("tune_race_win_loss")
}

#' @export
tune_race_win_loss.default <- function(object, ...) {
  msg <- paste0(
    "The first argument to [tune_race_win_loss()] should be either ",
    "a model or workflow."
  )
  rlang::abort(msg)
}

#' @export
tune_race_win_loss.recipe <-
  function(object, model, resamples, ..., param_info = NULL, grid = 10,
           metrics = NULL, control = control_race(), eval_time = NULL) {
    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

    tune_race_win_loss(
      model,
      preprocessor = object, resamples = resamples,
      param_info = param_info, grid = grid,
      metrics = metrics, control = control,
      eval_time = eval_time
    )
  }

#' @export
tune_race_win_loss.formula <-
  function(formula, model, resamples, ..., param_info = NULL, grid = 10,
           metrics = NULL, control = control_race(), eval_time = NULL) {
    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

    tune_race_win_loss(
      model,
      preprocessor = formula, resamples = resamples,
      param_info = param_info, grid = grid,
      metrics = metrics, control = control,
      eval_time = eval_time
    )
  }

#' @export
#' @rdname tune_race_win_loss
tune_race_win_loss.model_spec <-
  function(object, preprocessor, resamples, ..., param_info = NULL, grid = 10,
           metrics = NULL, control = control_race(), eval_time = NULL) {
    if (rlang::is_missing(preprocessor) || !tune::is_preprocessor(preprocessor)) {
      rlang::abort(paste(
        "To tune a model spec, you must preprocess",
        "with a formula, recipe, or variable specification"
      ))
    }

    tune::empty_ellipses(...)

    control <- parsnip::condense_control(control, control_race())

    wflow <- workflows::add_model(workflows::workflow(), object)

    if (tune::is_recipe(preprocessor)) {
      wflow <- workflows::add_recipe(wflow, preprocessor)
    } else if (rlang::is_formula(preprocessor)) {
      wflow <- workflows::add_formula(wflow, preprocessor)
    }

    tune_race_win_loss_workflow(
      wflow,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      param_info = param_info,
      control = control,
      eval_time = eval_time
    )
  }

#' @export
#' @rdname tune_race_win_loss
tune_race_win_loss.workflow <- function(object, resamples, ..., param_info = NULL,
                                        grid = 10, metrics = NULL,
                                        control = control_race()) {
  tune::empty_ellipses(...)

  control <- parsnip::condense_control(control, control_race())

  tune_race_win_loss_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    param_info = param_info,
    control = control,
    eval_time = eval_time
  )
}

## -----------------------------------------------------------------------------

tune_race_win_loss_workflow <-
  function(object, resamples, param_info = NULL, grid = 10, metrics = NULL,
           control = control_race(), eval_time = NULL) {
    rlang::check_installed("BradleyTerry2")

    B <- nrow(resamples)
    if (control$randomize) {
      resamples <- randomize_resamples(resamples)
    }
    resamples <- dplyr::mutate(resamples, .order = dplyr::row_number())

    min_rs <- control$burn_in
    check_num_resamples(B, min_rs)
    tmp_resamples <- restore_rset(resamples, 1:min_rs)

    grid_control <- parsnip::condense_control(control, tune::control_grid())
    res <-
      object %>%
      tune::tune_grid(
        resamples = tmp_resamples,
        param_info = param_info,
        grid = grid,
        metrics = metrics,
        control = grid_control,
        eval_time = eval_time
      )

    param_names <- tune::.get_tune_parameter_names(res)
    metrics <- tune::.get_tune_metrics(res)
    analysis_metric <- names(attr(metrics, "metrics"))[1]
    analysis_max <- attr(attr(metrics, "metrics")[[1]], "direction") == "maximize"
    metrics_time <- eval_time[1]

    cols <- tune::get_tune_colors()
    if (control$verbose_elim) {
      msg <-
        paste(
          "Racing will", ifelse(analysis_max, "maximize", "minimize"),
          "the", analysis_metric, "metric."
        )
      rlang::inform(cols$message$info(paste0(cli::symbol$info, " ", msg)))
      if (control$randomize) {
        msg <- "Resamples are analyzed in a random order."
        rlang::inform(cols$message$info(paste0(cli::symbol$info, " ", msg)))
      }
    }

    filters_results <- test_parameters_bt(res, control$alpha, metrics_time)
    n_grid <- nrow(filters_results)

    log_final <- TRUE
    num_ties <- 0
    for (rs in (min_rs + 1):B) {
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

      grid_control <- parsnip::condense_control(control, tune::control_grid())
      tmp_res <-
        object %>%
        tune::tune_grid(
          resamples = tmp_resamples,
          param_info = param_info,
          grid = new_grid,
          metrics = metrics,
          control = grid_control,
          eval_time = eval_time
        )
      res <- restore_tune(res, tmp_res)

      if (nrow(new_grid) > 1) {
        filters_results <- test_parameters_bt(res, control$alpha, metrics_time)
        if (sum(filters_results$pass) == 2 & num_ties >= control$num_ties) {
          filters_results <- tie_breaker(res, control)
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
