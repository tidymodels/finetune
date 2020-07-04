options(dplyr.summarise.inform = FALSE)

## -----------------------------------------------------------------------------
# Racing via repeated measures ANOVA


mer2tibble <- function(x) {
  x %>%
    tibble::as_tibble(rownames = ".config") %>%
    dplyr::filter(grepl("\\.config", .config)) %>%
    dplyr::mutate(.config = gsub("^\\.config", "", .config))
}

test_parameters_gls <- function(x, param_names, metric, maximize, alpha =  0.05) {
  require("lme4")

  res <-
    tune::collect_metrics(x, summarize = FALSE) %>%
    dplyr::filter(.metric == metric)

  key <-
    res %>%
    dplyr::select(!!!param_names, .config) %>%
    dplyr::distinct()

  configs <-
    res %>%
    dplyr::select(dplyr::starts_with("id"), .estimate, .config)

  ## ---------------------------------------------------------------------------
  # regroup .configs by mean and subset on those being analyzed

  best_config <-
    configs %>%
    dplyr::group_by(.config) %>%
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      B = sum(!is.na(.estimate))) %>%
    dplyr::ungroup()

  max_resamples <- max(best_config$B, na.rm = TRUE)
  best_config <- best_config %>% dplyr::filter(B == max_resamples)
  configs <- configs %>% dplyr::filter(.config %in% best_config$.config)
  if (maximize) {
    config_levels <- best_config$.config[order(-best_config$mean)]
  } else {
    config_levels <- best_config$.config[order( best_config$mean)]
  }
  configs$.config <- factor(configs$.config, levels = config_levels)

  ## ---------------------------------------------------------------------------

  # TODO Break out model and try/catch if errors
  # function to make formula based on current resamples
  mod <- lme4::lmer(.estimate ~ .config + (1|id), data = configs)

  rs_pct <-
    VarCorr(mod) %>%
    tibble::as_tibble(rownames = "terms") %>%
    dplyr::mutate(var = sdcor^2, pct = var/sum(var)*100) %>%
    dplyr::filter(grp == "id") %>%
    dplyr::pull(pct) %>%
    round(2)

  point_est <-
    coef(summary(mod)) %>%
    mer2tibble() %>%
    dplyr::select(.config, estimate = Estimate)
  mod_est <-
    confint(mod, method = "Wald", level = 1 - alpha, quiet = TRUE) %>%
    mer2tibble() %>%
    setNames(c(".config", "lower", "upper")) %>%
    dplyr::inner_join(point_est, by = ".config")

  if (maximize) {
    mod_est <-
      mod_est %>%
      dplyr::mutate(pass = ifelse(upper > 0, TRUE, FALSE))
  } else {
    mod_est <-
      mod_est %>%
      dplyr::mutate(pass = ifelse(lower < 0, TRUE, FALSE))
  }

  best_res <-
    tibble::tibble(
      .config = config_levels[1],
      lower = 0,
      upper = 0,
      estimate = 0,
      pass = TRUE
    )

  dplyr::bind_rows(best_res, mod_est) %>%
    dplyr::inner_join(key, by = ".config")
}


## -----------------------------------------------------------------------------
# Racing via discrete competitions

test_parameters_bt <- function(x, param_names, metric, maximize, alpha =  0.05) {
  require("BradleyTerry2")

  res <-
    tune::collect_metrics(x, summarize = FALSE) %>%
    dplyr::filter(.metric == metric)

  key <-
    res %>%
    dplyr::select(!!!param_names, .config) %>%
    dplyr::distinct()

  ## ---------------------------------------------------------------------------
  # Analyze data only on current candidate set

  num_resasmples <-
    res %>%
    dplyr::count(.config, name = "B")
  max_resasmples <- max(num_resasmples$B)
  analysis_config <-
    num_resasmples %>%
    dplyr::filter(B == max_resasmples)
  analysis_data <- dplyr::inner_join(res, analysis_config, by = ".config")

  ## -----------------------------------------------------------------------------
  # Split data into all 2-way combinations of parameter configurations

  season_schedule <- make_config_pairs(analysis_data)
  season_data <- score_season(season_schedule, analysis_data, maximize)

  mod <- BradleyTerry2::BTm(cbind(wins_1, wins_2), player_1, player_2, data = season_data$scoring)
  z_val <- qnorm(1 - alpha)
  mod_est <-
    BradleyTerry2::BTabilities(mod) %>%
    tibble::as_tibble(rownames = ".config") %>%
    dplyr::mutate_if(is.numeric, as.numeric) %>%  # <- gets rid of `BTabilts` columns
    dplyr::mutate(
      lower = ability - z_val * s.e.,
      upper = ability + z_val * s.e.,
      # The secondary conditions are for cases when a candidate set wins
      # a single competition.
      pass = ifelse(upper > 0 & s.e. < 500, TRUE, FALSE),
      # keep best condition
      pass = ifelse(ability == 0 & s.e. == 0, TRUE, pass)
    )

  if (length(season_data$eliminated) > 0) {
    elim_results <-
      tibble::tibble(
        .config = season_data$eliminated,
        ability = NA_real_,
        s.e. = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        pass = FALSE
      )
    mod_est <- dplyr::bind_rows(mod_est, elim_results)
  }

  mod_est <-
    mod_est %>%
    dplyr::inner_join(key, by = ".config")

  mod_est
}

make_config_pairs <- function(x) {
  ids <- unique(x$.config)
  id_combin <- t(utils::combn(ids, 2))
  colnames(id_combin) <- c("p1", "p2")
  id_combin <- tibble::as_tibble(id_combin)
}


score_match <- function(x, y, maximize) {
  if (maximize) {
    1.0 * (x > y) + sum(x == y)/2
  } else {
    1.0 * sum(x < y) + sum(x == y)/2
  }
}

score_season <- function(x, dat, maximize = FALSE) {
  # Determine how many matches each team has won. However, if a team loses
  # _all of its games_ (aka getting skunked), do not include it in the BT model
  # since it will have massively large standard errors and _not_ be eliminated
  # from the candidate set.
  # Because of this, we do some computations multiple times.
  player_1 <-
    dplyr::left_join(
      x %>%
        dplyr::mutate(pair = row_number()) %>%
        dplyr::select(.config = p1, pair),
      dat,
      by = ".config") %>%
    dplyr::select(player_1 = .config, metric_1 = .estimate, pair, dplyr::starts_with("id"))

  player_2 <-
    left_join(
      x %>%
        dplyr::mutate(pair = row_number()) %>%
        dplyr::select(.config = p2, pair),
      dat,
      by = ".config") %>%
    dplyr::select(player_2 = .config, metric_2 = .estimate, pair, dplyr::starts_with("id"))

  game_results <-
    dplyr::full_join(player_1, player_2, by = c("id", "pair")) %>%
    dplyr::mutate(
      wins_1 = purrr::map2_dbl(metric_1, metric_2, score_match, maximize = maximize),
      wins_2 = purrr::map2_dbl(metric_2, metric_1, score_match, maximize = maximize)
    )

  season_results <-
    game_results %>%
    dplyr::group_by(player_1, player_2, pair) %>%
    dplyr::summarize(
      wins_1 = sum(wins_1, na.rm = TRUE),
      wins_2 = sum(wins_2, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pair)

  # Find overall rankings

  player_rankings <-
    dplyr::bind_rows(
      season_results %>% dplyr::select(player = player_1, wins = wins_1),
      season_results %>% dplyr::select(player = player_2, wins = wins_2)
    ) %>%
    dplyr::group_by(player) %>%
    dplyr::summarize(wins = sum(wins, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(wins))

  if (any(player_rankings$wins == 0)) {
    eliminated <- player_rankings$player[player_rankings$wins == 0]
    # message("Some players lost all games: ", paste0(eliminated, collapse = ", "))
    season_results <-
      game_results %>%
      dplyr::filter(!(player_1 %in% eliminated) & !(player_2 %in% eliminated)) %>%
      dplyr::group_by(player_1, player_2, pair) %>%
      dplyr::summarize(
        wins_1 = sum(wins_1, na.rm = TRUE),
        wins_2 = sum(wins_2, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-pair)

    player_rankings <-
      dplyr::bind_rows(
        season_results %>% dplyr::select(player = player_1, wins = wins_1),
        season_results %>% dplyr::select(player = player_2, wins = wins_2)
      ) %>%
      dplyr::group_by(player) %>%
      dplyr::summarize(wins = sum(wins, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(wins))
  } else {
    eliminated <- character(0)
  }

  res <-
    season_results %>%
    dplyr::mutate(
      player_1 = factor(player_1, levels = player_rankings$player),
      player_2 = factor(player_2, levels = player_rankings$player)
    )
  list(scoring = res, eliminated = eliminated)
}

## -----------------------------------------------------------------------------
# Other helpers

# Since slicing rset or tune objects drops their attributes, these functions are
# used to reconstruct them temporarily

restore_rset <- function(x, index) {
  att <- attributes(x)
  x <- x %>% dplyr::slice(index)
  attributes(x) <- att
  x
}

get_configs <- function(x) {
  collect_metrics(x) %>%
    distinct(!!!rlang::syms(tune::.get_tune_parameter_names(x)), .config)
}
merge_indiv_configs <- function(x, key) {
  orig_names <- names(x)
  param_names <- names(key)[names(key) != ".config"]
  dplyr::inner_join(x %>% dplyr::select(-.config), key, by = param_names) %>%
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

restore_tune <- function(x, y) {
  # With a smaller number of parameter combinations, the .config values may have
  # changed. We'll use the full set of parameters in `x` to adjust what is in
  # `y`.

  keys <- get_configs(x)
  y <- harmonize_configs(y, keys)

  ## -----------------------------------------------------------------------------

  att <- attributes(x)
  att$row.names <- 1:(nrow(x) + nrow(y))

  ## -----------------------------------------------------------------------------

  x <- dplyr::bind_rows(tibble::as_tibble(x), tibble::as_tibble(y))
  attributes(x) <- att

  x
}

## -----------------------------------------------------------------------------

log_racing <- function(control, x, splits, grid_size, metric) {
  # if (!control$verbose) {
  #   return(invisible(NULL))
  # }


  if (!is.null(splits)) {
    splits <- splits[[length(splits)]]
    labs <- labels(splits)
    labs <- rev(unlist(labs))
    labs <- paste0(labs, collapse = ", ")
    labs <- paste0(labs, ": ")
  }

  remaining <- sum(x$pass, na.rm = TRUE)
  if (remaining > 1) {
    msg <- paste(remaining, "of",  grid_size, "candidate models remain")
  } else {
    msg <- paste(remaining, "of",  grid_size, "candidate model remains")
  }

  msg <-
    tune:::tune_color$symbol$info(
      paste0(cli::symbol$info, " ", labs, msg, " (filtered using ", metric, ").")
    )
  message(msg)
}

