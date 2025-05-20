options(dplyr.summarise.inform = FALSE)

## -----------------------------------------------------------------------------
# Racing via repeated measures ANOVA

mod2tibble <- function(x) {
  x |>
    tibble::as_tibble(rownames = ".config") |>
    dplyr::filter(grepl("\\.config", .config)) |>
    dplyr::mutate(.config = gsub("^\\.config", "", .config))
}

refactor_by_mean <- function(res, maximize = TRUE) {
  configs <-
    res |>
    dplyr::select(dplyr::starts_with("id"), .estimate, .config)

  ## ---------------------------------------------------------------------------
  # regroup .configs by mean and subset on those being analyzed

  best_config <-
    configs |>
    dplyr::group_by(.config) |>
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      B = sum(!is.na(.estimate))
    ) |>
    dplyr::ungroup()

  max_resamples <- max(best_config$B, na.rm = TRUE)
  best_config <- best_config |> dplyr::filter(B == max_resamples)
  configs <- configs |> dplyr::filter(.config %in% best_config$.config)
  if (maximize) {
    config_levels <- best_config$.config[order(-best_config$mean)]
  } else {
    config_levels <- best_config$.config[order(best_config$mean)]
  }
  configs$.config <- factor(configs$.config, levels = config_levels)
  configs
}

test_parameters_gls <- function(x, alpha = 0.05, eval_time = NULL) {
  if (all(purrr::map_lgl(x$.metrics, is.null))) {
    cli::cli_abort("There were no valid metrics for the ANOVA model.")
  }
  param_names <- tune::.get_tune_parameter_names(x)
  metric_data <- metric_tibble(x)
  metric <- metric_data$metric[1]
  maximize <- metric_data$direction[1] == "maximize"

  res <-
    tune::collect_metrics(x, summarize = FALSE) |>
    dplyr::filter(.metric == metric)

  if (!is.null(eval_time) && any(names(res) == ".eval_time")) {
    res <- dplyr::filter(res, .eval_time == eval_time)
  }

  key <-
    res |>
    dplyr::select(!!!param_names, .config) |>
    dplyr::distinct()

  configs <- refactor_by_mean(res, maximize)

  ## ---------------------------------------------------------------------------

  mod_est <- fit_anova(x, configs, alpha)

  # rs_pct <-
  # lme4::VarCorr(mod) |>
  # tibble::as_tibble(rownames = "terms") |>
  # dplyr::mutate(var = sdcor^2, pct = var/sum(var)*100) |>
  # dplyr::filter(grp == "id") |>
  # dplyr::pull(pct) |>
  # round(2)

  # point_est <-
  #   coef(summary(mod)) |>
  #   mer2tibble() |>
  #   dplyr::select(.config, estimate = Estimate)
  # mod_est <-
  #   confint(mod, method = "Wald", level = 1 - alpha, quiet = TRUE) |>
  #   mer2tibble() |>
  #   setNames(c(".config", "lower", "upper")) |>
  #   dplyr::inner_join(point_est, by = ".config")

  if (maximize) {
    mod_est <-
      mod_est |>
      dplyr::mutate(pass = ifelse(upper > 0, TRUE, FALSE))
  } else {
    mod_est <-
      mod_est |>
      dplyr::mutate(pass = ifelse(lower < 0, TRUE, FALSE))
  }

  best_res <-
    tibble::tibble(
      .config = levels(configs$.config)[1],
      lower = 0,
      upper = 0,
      estimate = 0,
      pass = TRUE
    )

  dplyr::bind_rows(best_res, mod_est) |>
    dplyr::inner_join(key, by = ".config")
}


## -----------------------------------------------------------------------------
# Racing via discrete competitions

test_parameters_bt <- function(x, alpha = 0.05, eval_time = NULL) {
  param_names <- tune::.get_tune_parameter_names(x)
  metric_data <- metric_tibble(x)
  metric <- metric_data$metric[1]
  maximize <- metric_data$direction[1] == "maximize"

  res <-
    tune::collect_metrics(x, summarize = FALSE) |>
    dplyr::filter(.metric == metric)

  if (!is.null(eval_time) && any(names(res) == ".eval_time")) {
    res <- dplyr::filter(res, .eval_time == eval_time)
  }

  key <-
    res |>
    dplyr::select(!!!param_names, .config) |>
    dplyr::distinct()

  ## ---------------------------------------------------------------------------
  # Analyze data only on current candidate set

  num_resamples <-
    res |>
    dplyr::count(.config, name = "B")
  max_resamples <- max(num_resamples$B)
  analysis_config <-
    num_resamples |>
    dplyr::filter(B == max_resamples)
  analysis_data <- dplyr::inner_join(res, analysis_config, by = ".config")

  ## -----------------------------------------------------------------------------
  # Split data into all 2-way combinations of parameter configurations

  season_schedule <- make_config_pairs(analysis_data)
  # Eliminate pairs with all ties
  season_data <- score_season(season_schedule, analysis_data, maximize)

  best_team <- levels(season_data$scoring$player_1)[1]

  # Cases, esp with 2 players, where one player doesn't lose any games
  if (nrow(season_data$scoring) == 0) {
    return(mercy_rule(season_data, key))
  }

  best_team <- levels(season_data$scoring$player_1)[1]
  suppressWarnings(
    mod <- BradleyTerry2::BTm(
      cbind(wins_1, wins_2),
      player_1,
      player_2,
      data = season_data$scoring,
      br = TRUE
    )
  )

  q_val <- qt(1 - alpha, 1)
  mod_est <-
    summary(mod) |>
    purrr::pluck("coefficients") |>
    tibble::as_tibble(rownames = ".config") |>
    dplyr::select(.config, value = Estimate, std_err = `Std. Error`) |>
    dplyr::mutate(
      .config = gsub("^\\.\\.", "", .config),
      lower = value - q_val * std_err,
      upper = value + q_val * std_err,
      # The secondary conditions are for cases when a candidate set wins
      # a single competition.
      pass = ifelse(upper > 0 & std_err < 500, TRUE, FALSE)
    ) |>
    dplyr::bind_rows(make_best_results(best_team))

  ## TODO For both racing methods, make this a function with the configs as
  ## arguments.
  if (length(season_data$eliminated) > 0) {
    mod_est <- dplyr::bind_rows(
      mod_est,
      make_elim_results(season_data$eliminated)
    )
  }

  mod_est <-
    mod_est |>
    dplyr::inner_join(key, by = ".config")

  mod_est
}

make_config_pairs <- function(x) {
  ids <- unique(x$.config)
  id_combin <- t(utils::combn(ids, 2))
  colnames(id_combin) <- c("p1", "p2")
  id_combin <- tibble::as_tibble(id_combin)
  id_combin
}

score_match <- function(x, y, maximize) {
  if (maximize) {
    1.0 * (x > y) + sum(x == y) / 2
  } else {
    1.0 * sum(x < y) + sum(x == y) / 2
  }
}

make_best_results <- function(player) {
  tibble::tibble(
    .config = player,
    value = 0,
    std_err = 0,
    lower = 0,
    upper = 0,
    pass = TRUE
  )
}

make_elim_results <- function(players) {
  tibble::tibble(
    .config = players,
    value = NA_real_,
    std_err = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    pass = FALSE
  )
}

mercy_rule <- function(dat, key) {
  elim <- dat$eliminated
  lvls <- levels(dat$scoring$player_1)
  best <- lvls[!(lvls %in% elim)]
  dplyr::bind_rows(
    make_best_results(best),
    make_elim_results(elim)
  ) |>
    dplyr::inner_join(key, by = ".config")
}


score_season <- function(x, dat, maximize = FALSE) {
  # Determine how many matches each team has won. However, if a team loses
  # _all of its games_ (aka getting skunked), do not include it in the BT model
  # since it will have massively large standard errors and _not_ be eliminated
  # from the candidate set.
  # Because of this, we do some computations multiple times.

  ## -----------------------------------------------------------------------------

  # Look for zero-variance results and duplicated columns
  eliminated <- check_results(dat)
  x <- x |> dplyr::filter(!(p1 %in% eliminated) & !(p2 %in% eliminated))
  dat <- dat |> dplyr::filter(!(.config %in% eliminated))
  current_teams <- unique(dat$.config)

  ## -----------------------------------------------------------------------------

  player_1 <-
    dplyr::left_join(
      x |>
        dplyr::mutate(pair = dplyr::row_number()) |>
        dplyr::select(.config = p1, pair),
      dat,
      by = ".config",
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      player_1 = .config,
      metric_1 = .estimate,
      pair,
      dplyr::starts_with("id")
    )

  player_2 <-
    dplyr::left_join(
      x |>
        dplyr::mutate(pair = dplyr::row_number()) |>
        dplyr::select(.config = p2, pair),
      dat,
      by = ".config",
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      player_2 = .config,
      metric_2 = .estimate,
      pair,
      dplyr::starts_with("id")
    )

  game_results <-
    dplyr::full_join(
      player_1,
      player_2,
      by = c("id", "pair"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      wins_1 = purrr::map2_dbl(
        metric_1,
        metric_2,
        score_match,
        maximize = maximize
      ),
      wins_2 = purrr::map2_dbl(
        metric_2,
        metric_1,
        score_match,
        maximize = maximize
      )
    )

  season_results <-
    game_results |>
    dplyr::group_by(player_1, player_2, pair) |>
    dplyr::summarize(
      wins_1 = sum(wins_1, na.rm = TRUE),
      wins_2 = sum(wins_2, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-pair)

  # Find overall rankings
  player_rankings <-
    dplyr::bind_rows(
      season_results |> dplyr::select(player = player_1, wins = wins_1),
      season_results |> dplyr::select(player = player_2, wins = wins_2)
    ) |>
    dplyr::group_by(player) |>
    dplyr::summarize(wins = sum(wins, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(wins))

  if (any(player_rankings$wins == 0)) {
    skunked <- player_rankings$player[player_rankings$wins == 0]
    eliminated <- c(eliminated, skunked)
    season_results <-
      game_results |>
      dplyr::filter(!(player_1 %in% skunked) & !(player_2 %in% skunked)) |>
      dplyr::group_by(player_1, player_2, pair) |>
      dplyr::summarize(
        wins_1 = sum(wins_1, na.rm = TRUE),
        wins_2 = sum(wins_2, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-pair)

    player_rankings <-
      dplyr::bind_rows(
        season_results |> dplyr::select(player = player_1, wins = wins_1),
        season_results |> dplyr::select(player = player_2, wins = wins_2)
      ) |>
      dplyr::group_by(player) |>
      dplyr::summarize(wins = sum(wins, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(wins))
  }

  if (nrow(season_results) == 0) {
    not_elim <- current_teams[!(current_teams %in% eliminated)]
    lvls <- c(not_elim, eliminated)
  } else {
    lvls <- player_rankings$player
  }

  res <-
    season_results |>
    dplyr::mutate(
      player_1 = factor(player_1, levels = lvls),
      player_2 = factor(player_2, levels = lvls)
    )
  list(scoring = res, eliminated = eliminated)
}

## -----------------------------------------------------------------------------
# Other helpers

# Since slicing rset or tune objects drops their attributes, these functions are
# used to reconstruct them temporarily

restore_rset <- function(x, index) {
  att <- attributes(x)
  x <- x |> dplyr::slice(index)
  att$row.names <- att$row.names[index]
  attributes(x) <- att
  x
}

get_configs <- function(x) {
  tune::collect_metrics(x) |>
    dplyr::distinct(!!!rlang::syms(tune::.get_tune_parameter_names(x)), .config)
}
merge_indiv_configs <- function(x, key) {
  orig_names <- names(x)
  param_names <- names(key)[names(key) != ".config"]
  dplyr::inner_join(x |> dplyr::select(-.config), key, by = param_names) |>
    dplyr::select(!!!orig_names)
}
harmonize_configs <- function(x, key) {
  x$.metrics <- purrr::map(x$.metrics, merge_indiv_configs, key = key)
  if (any(names(x) == ".predictions")) {
    x$.predictions <- purrr::map(x$.predictions, merge_indiv_configs, key = key)
  }
  if (any(names(x) == ".extracts")) {
    x$.extracts <- purrr::map(x$.extracts, merge_indiv_configs, key = key)
  }
  x
}

# restore_tune() restores certain attributes (esp class) that are lost during
# racing when rows of the resampling object are filtered.
# `x` has class `tune_results`. `y` has the same structure but different
# attributes.
# About eval_time_target: `x` is from `tune_grid()`, which has no notion of a
# target evaluation time. https://github.com/tidymodels/tune/pull/782 defaults
# `eval_time_target` to NULL for grid tuning, resampling, and last fit objects.
# That's why `eval_time_target` is an argument to this function. It should be
# non-null for the resulting racing object but the value inherited from `x` is
# NULL unless we set it.

restore_tune <- function(x, y, eval_time_target = NULL) {
  # With a smaller number of parameter combinations, the .config values may have
  # changed. We'll use the full set of parameters in `x` to adjust what is in
  # `y`.

  keys <- get_configs(x)
  y <- harmonize_configs(y, keys)

  ## -----------------------------------------------------------------------------

  att <- attributes(x)
  att$row.names <- 1:(nrow(x) + nrow(y))
  att$eval_time_target <- eval_time_target
  att$class <- c("tune_race", "tune_results", class(tibble::tibble()))

  ## -----------------------------------------------------------------------------

  x <- dplyr::bind_rows(tibble::as_tibble(x), tibble::as_tibble(y))
  attributes(x) <- att

  x
}

## -----------------------------------------------------------------------------

log_racing <- function(control, x, splits, grid_size, metric) {
  if (!control$verbose_elim) {
    return(invisible(NULL))
  }

  chr_seq <- format(0:grid_size)

  if (!is.null(splits)) {
    splits <- splits[[length(splits)]]
    labs <- labels(splits)
    labs <- rev(unlist(labs))
    labs <- paste0(labs, collapse = ", ")
    labs <- paste0(labs, ": ")
  }

  remaining <- sum(x$pass, na.rm = TRUE)
  num_elim <- sum(!x$pass, na.rm = TRUE)
  if (remaining > 1) {
    msg <- paste(
      chr_seq[num_elim + 1],
      "eliminated;",
      chr_seq[remaining + 1],
      "candidates remain."
    )
  } else {
    msg <- paste("All but one parameter combination were eliminated.")
  }

  tune_cols <- tune::get_tune_colors()
  msg <-
    tune_cols$message$info(
      paste0(cli::symbol$info, " ", labs, msg)
    )
  cli::cli_bullets(msg)
}

tie_breaker <- function(res, control, eval_time = NULL) {
  param_names <- tune::.get_tune_parameter_names(res)
  metrics <- tune::.get_tune_metrics(res)
  analysis_metric <- names(attr(metrics, "metrics"))[1]
  analysis_max <- attr(attr(metrics, "metrics")[[1]], "direction") == "maximize"
  metrics_time <- eval_time[1]

  x <-
    res |>
    tune::collect_metrics() |>
    dplyr::filter(.metric == analysis_metric)

  if (!is.null(metrics_time)) {
    x <- dplyr::filter(x, .eval_time == metrics_time)
  }

  all_config <- x$.config
  max_rs <- max(x$n)
  finalists <- x[x$n == max_rs, ]
  best <- finalists$.config[order(finalists$mean, decreasing = analysis_max)][1]
  if (control$verbose_elim) {
    tune_cols <- tune::get_tune_colors()
    msg <- tune_cols$symbol$info(paste0(cli::symbol$info, " Tie broken!"))
    if (runif(1) < .05) {
      msg <- paste(msg, "Two models enter, one model leaves.")
    }
    cli::cli_inform(msg)
  }
  res <-
    dplyr::select(x, .config, !!!param_names) |>
    dplyr::mutate(
      lower = 0,
      upper = 0,
      estimate = 0,
      pass = ifelse(.config == best, TRUE, FALSE)
    )
  res
}

check_results <- function(dat, rm_zv = TRUE, rm_dup = FALSE) {
  ids <- grep("^id", names(dat), value = TRUE)

  if (any(names(dat) == ".eval_time")) {
    dat$.eval_time <- NULL
  }

  x <-
    dat |>
    dplyr::select(!!!ids, .estimate, .config) |>
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(ids)),
      names_from = .config,
      values_from = .estimate
    )

  exclude <- character(0)

  if (rm_dup) {
    tmp <- x[, !(names(x) %in% ids)]
    exclude <- c(exclude, names(tmp[duplicated(lapply(tmp, c))]))
    x <- x[, !(names(x) %in% exclude)]
  }

  if (rm_zv) {
    is_zv <- purrr::map_lgl(x, \(x) length(unique(x)) == 1)
    if (any(is_zv)) {
      exclude <- c(exclude, names(x)[is_zv])
      x <- x[, !(names(x) %in% exclude)]
    }
  }

  exclude
}


lmer_formula <- function(x, info) {
  f <- .estimate ~ .config + (1 | id)
  if (any(info$class == "vfold_cv") && info$repeats > 1) {
    ids <- x |> dplyr::select(id2, id)
    unique_res <- dplyr::count(ids, id)
    if (nrow(unique_res) > 1 && all(unique_res$n > 1)) {
      f <- .estimate ~ .config + (1 | id2 / id)
    } else {
      f <- .estimate ~ .config + (1 | .all_id)
    }
  }
  attr(f, ".Environment") <- rlang::base_env()
  f
}

fit_anova <- function(x, dat, alpha) {
  rs_info <- attr(x, "rset_info")$att
  if (any(rs_info$class == "vfold_cv") && rs_info$repeats > 1) {
    dat <- dplyr::mutate(dat, .all_id = paste(id2, id))
  }

  f <- lmer_formula(x, rs_info)

  suppressWarnings(
    suppressMessages(
      mod <- try(lme4::lmer(f, data = dat), silent = TRUE)
    )
  )

  if (inherits(mod, "try-error") || !isTRUE(mod@optinfo$conv$opt == 0)) {
    mod <- lm(.estimate ~ .config, data = dat)
  }
  point_est <-
    coef(summary(mod)) |>
    mod2tibble() |>
    dplyr::select(.config, estimate = Estimate)
  interval_est <-
    confint(mod, method = "Wald", level = 1 - alpha, quiet = TRUE) |>
    mod2tibble() |>
    setNames(c(".config", "lower", "upper"))

  dplyr::inner_join(point_est, interval_est, by = ".config")
}

## -----------------------------------------------------------------------------

metric_tibble <- function(x, ...) {
  metrics <- attributes(x)$metrics
  metrics <- attributes(metrics)$metrics
  directions <- purrr::map_chr(metrics, \(x) attr(x, "direction"))
  tibble::tibble(metric = names(metrics), direction = directions)
}

## -----------------------------------------------------------------------------

check_hidden_arg <- function(x, name, value) {
  if (!any(names(x) == name)) {
    return(FALSE)
  }
  identical(x[[name]], value)
}

# ------------------------------------------------------------------------------

randomize_resamples <- function(x) {
  att <- attributes(x)
  B <- nrow(x)
  x$.rand <- runif(B)
  reps <- attr(x, "repeats")
  if (!is.null(reps) && reps > 1) {
    x <- x |>
      dplyr::group_by(id) |>
      dplyr::arrange(.rand, .by_group = TRUE)
  } else {
    x <- x |> dplyr::arrange(.rand)
  }
  x$.rand <- NULL
  att$row.names <- att$row.names
  attributes(x) <- att
  x
}

# ------------------------------------------------------------------------------
# S3 methods

#' Obtain and format results produced by racing functions
#' @inheritParams tune::collect_predictions
#' @param all_configs A logical: should we return the complete set of model
#' configurations or just those that made it to the end of the race (the
#' default).
#' @export
#' @return A tibble. The column names depend on the results and the mode of the
#' model.
#' @details
#'
#' For [tune::collect_metrics()] and [tune::collect_predictions()], when unsummarized,
#' there are columns for each tuning parameter (using the `id` from [hardhat::tune()],
#' if any).
#' [tune::collect_metrics()] also has columns `.metric`, and `.estimator`.  When the
#' results are summarized, there are columns for `mean`, `n`, and `std_err`.
#' When not summarized, the additional columns for the resampling identifier(s)
#' and `.estimate`.
#'
#' For [tune::collect_predictions()], there are additional columns for the resampling
#' identifier(s), columns for the predicted values (e.g., `.pred`,
#' `.pred_class`, etc.), and a column for the outcome(s) using the original
#' column name(s) in the data.
#'
#' [tune::collect_predictions()] can summarize the various results over
#'  replicate out-of-sample predictions. For example, when using the bootstrap,
#'  each row in the original training set has multiple holdout predictions
#'  (across assessment sets). To convert these results to a format where every
#'  training set same has a single predicted value, the results are averaged
#'  over replicate predictions.
#'
#' For regression cases, the numeric predictions are simply averaged. For
#'  classification models, the problem is more complex. When class probabilities
#'  are used, these are averaged and then re-normalized to make sure that they
#'  add to one. If hard class predictions also exist in the data, then these are
#'  determined from the summarized probability estimates (so that they match).
#'  If only hard class predictions are in the results, then the mode is used to
#'  summarize.
#'
#' For racing results, it is best to only
#' collect model configurations that finished the race (i.e., were completely
#' resampled). Comparing performance metrics for configurations averaged with
#' different resamples is likely to lead to inappropriate results.
#' @name collect_predictions
collect_predictions.tune_race <-
  function(x, ..., summarize = FALSE, parameters = NULL, all_configs = FALSE) {
    rlang::check_dots_empty()
    x <- dplyr::select(x, -.order)
    res <- collect_predictions(
      x,
      summarize = summarize,
      parameters = parameters
    )
    if (!all_configs) {
      final_configs <- subset_finished_race(x)
      res <- dplyr::inner_join(res, final_configs, by = ".config")
    }
    res
  }

#' @inheritParams tune::collect_metrics
#' @export
#' @rdname collect_predictions
collect_metrics.tune_race <- function(
  x,
  ...,
  summarize = TRUE,
  type = c("long", "wide"),
  all_configs = FALSE
) {
  rlang::check_dots_empty()
  x <- dplyr::select(x, -.order)
  final_configs <- subset_finished_race(x)
  res <- collect_metrics(x, summarize = summarize, type = type)
  if (!all_configs) {
    final_configs <- subset_finished_race(x)
    res <- dplyr::inner_join(res, final_configs, by = ".config")
  }
  res
}

#' Investigate best tuning parameters
#'
#' [tune::show_best()] displays the top sub-models and their performance estimates.
#' @inheritParams tune::show_best
#' @rdname show_best
#' @param n An integer for the maximum number of top results/rows to return.
#' @details
#' For racing results (from the \pkg{finetune} package), it is best to only
#' report configurations that finished the race (i.e., were completely
#' resampled). Comparing performance metrics for configurations averaged with
#' different resamples is likely to lead to inappropriate results.
#' @export
show_best.tune_race <- function(
  x,
  ...,
  metric = NULL,
  eval_time = NULL,
  n = 5,
  call = rlang::current_env()
) {
  rlang::check_dots_empty()
  if (!is.null(metric)) {
    # What was used to judge the race and how are they being sorted now?
    metrics <- tune::.get_tune_metrics(x)
    tune::check_metric_in_tune_results(tibble::as_tibble(metrics), metric)
    opt_metric <- tune::first_metric(metrics)
    opt_metric_name <- opt_metric$metric
    if (metric[1] != opt_metric_name) {
      cli::cli_warn(
        "Metric {.val {opt_metric_name}} was used to evaluate model
                   candidates in the race but {.val {metric}} has been chosen
                   to rank the candidates. These results may not agree with the
                   race."
      )
    }
  }

  x <- dplyr::select(x, -.order)
  final_configs <- subset_finished_race(x)

  res <- NextMethod(
    metric = metric,
    eval_time = eval_time,
    n = Inf,
    call = call
  )
  res$.ranked <- 1:nrow(res)
  res <- dplyr::inner_join(res, final_configs, by = ".config")
  res$.ranked <- NULL
  n <- min(n, nrow(res))
  res[1:n, ]
}


# Only return configurations that were completely resampled
subset_finished_race <- function(x) {
  x <- dplyr::select(x, -dplyr::any_of(".order"))
  full_res <- tune::estimate_tune_results(x)
  # At least one configuration was completely resampled
  max_b <- max(full_res$n)
  final_configs <- dplyr::filter(full_res, n == max_b)
  final_configs <- dplyr::distinct(final_configs, .config)
  final_configs <- dplyr::select(final_configs, .config)
  final_configs
}

# ------------------------------------------------------------------------------
# Log the objective function used for racing

racing_obj_log <- function(
  analysis_metric,
  direction,
  control,
  metrics_time = NULL
) {
  cols <- tune::get_tune_colors()
  if (control$verbose_elim) {
    msg <- paste("Racing will", direction, "the", analysis_metric, "metric")

    if (!is.null(metrics_time)) {
      msg <- paste(msg, "at time", format(metrics_time, digits = 3))
    }
    msg <- paste0(msg, ".")

    cli::cli_inform(cols$message$info(paste0(cli::symbol$info, " ", msg)))
    if (control$randomize) {
      msg <- "Resamples are analyzed in a random order."
      cli::cli_inform(cols$message$info(paste0(cli::symbol$info, " ", msg)))
    }
  }
  invisible(NULL)
}
