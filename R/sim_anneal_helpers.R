maximize_metric <- function(x, metric) {
  metrics <- tune::.get_tune_metrics(x)
  metrics_data <- tune::metrics_info(metrics)
  x <- metrics_data$.metric[1]
  metrics_data$direction[metrics_data$.metric == metric] == "maximize"
}

# Might not use this function
treat_as_integer <- function(x, num_unique = 10) {
  param_type <- purrr::map_chr(x$object, \(x) x$type)
  is_int <- param_type == "integer"
  x_vals <- purrr::map(x$object, \(x) dials::value_seq(x, n = 200))
  x_vals <- purrr::map_int(x_vals, \(x) length(unique(x)))
  x_vals < num_unique & is_int
}

new_in_neighborhood <- function(
  current,
  hist_values,
  pset,
  radius = c(0.05, 0.15),
  flip = 0.1
) {
  current <- dplyr::select(current, !!!pset$id)
  param_type <- purrr::map_chr(pset$object, \(x) x$type)
  if (any(param_type == "double")) {
    dbl_nms <- pset$id[param_type == "double"]
    new_dbl <-
      random_real_neighbor(
        current |> dplyr::select(dplyr::all_of(dbl_nms)),
        hist_values = hist_values |> dplyr::select(dplyr::all_of(dbl_nms)),
        pset |> dplyr::filter(id %in% dbl_nms),
        r = radius
      )
    current[, dbl_nms] <- new_dbl
  }

  if (any(param_type == "integer")) {
    int_nms <- pset$id[param_type == "integer"]
    flip_one <- all(param_type == "integer")
    new_int <-
      random_integer_neighbor(
        current |> dplyr::select(dplyr::all_of(int_nms)),
        hist_values = hist_values |> dplyr::select(dplyr::all_of(int_nms)),
        pset |> dplyr::filter(id %in% int_nms),
        prob = flip,
        change = flip_one
      )
    current[, int_nms] <- new_int
  }

  if (any(param_type == "character")) {
    chr_nms <- pset$id[param_type == "character"]
    flip_one <- all(param_type == "character")
    new_chr <-
      random_discrete_neighbor(
        current |> dplyr::select(!!!chr_nms),
        pset |> dplyr::filter(id %in% chr_nms),
        prob = flip,
        change = flip_one
      )
    current[, chr_nms] <- new_chr
  }
  current
}

random_discrete_neighbor <- function(current, pset, prob, change) {
  pnames <- pset$id
  change_val <- runif(length(pnames)) <= prob
  if (change & !any(change_val)) {
    change_val[sample(seq_along(change_val), 1)] <- TRUE
  }
  if (any(change_val)) {
    new_vals <- pnames[change_val]
    for (i in new_vals) {
      current_val <- current[[i]]
      parm_obj <- pset$object[[which(pset$id == i)]]
      parm_obj$values <- setdiff(parm_obj$values, current_val)
      current[[i]] <- dials::value_sample(parm_obj, 1)
    }
  }
  current
}


random_integer_neighbor <- function(
  current,
  hist_values,
  pset,
  prob,
  change,
  retain = 1,
  tries = 500
) {
  candidates <-
    purrr::map(
      1:tries,
      \(x) random_integer_neighbor_calc(current, pset, prob, change)
    ) |>
    purrr::list_rbind()

  rnd <- tune::encode_set(candidates, pset, as_matrix = TRUE)
  sample_by_distance(rnd, hist_values, retain = retain, pset = pset)
}

random_integer_neighbor_calc <- function(current, pset, prob, change) {
  change_val <- runif(nrow(pset)) <= prob
  if (change & !any(change_val)) {
    change_val[sample(seq_along(change_val), 1)] <- TRUE
  }
  if (any(change_val)) {
    param_change <- pset$id[change_val]
    for (i in param_change) {
      prm <- pset$object[[which(pset$id == i)]]
      prm_rng <- prm$range$upper - prm$range$lower
      tries <- min(prm_rng + 1, 500)
      pool <- dials::value_seq(prm, n = tries)
      smol_range <- floor(prm_rng / 10) + 1
      val_diff <- abs(current[[i]] - pool)
      pool <- pool[val_diff <= smol_range & val_diff > 0]
      if (length(pool) > 1) {
        current[[i]] <- sample(pool, 1)
      } else if (length(pool) == 1) {
        current[[i]] <- pool
      }
    }
  }
  current
}

random_real_neighbor <- function(
  current,
  hist_values,
  pset,
  retain = 1,
  tries = 500,
  r = c(0.05, 0.15)
) {
  is_quant <- purrr::map_lgl(pset$object, inherits, "quant_param")
  current <- current[, is_quant]
  pset <- pset[is_quant, ]
  encoded <- tune::encode_set(current, pset, as_matrix = TRUE)

  num_param <- ncol(encoded)
  if (num_param > 1) {
    rnd <- rnorm(num_param * tries)
    rnd <- matrix(rnd, ncol = num_param)
    rnd <- t(apply(rnd, 1, function(x) x / sqrt(sum(x^2))))
    rnd <- rnd * runif(tries, min = min(r), max = max(r))
    rnd <- sweep(rnd, 2, as.vector(encoded), "+")
    outside <- apply(rnd, 1, function(x) any(x > 1 | x < 0))
    rnd <- rnd[!outside, , drop = FALSE]
  } else {
    rnd <- runif(tries, min = -max(r), max = max(r)) + encoded[[1]]
    rnd <- ifelse(rnd > 1, 1, rnd)
    rnd <- ifelse(rnd < 0, 0, rnd)
    rnd <- matrix(rnd, ncol = 1)
    rnd <- rnd[!duplicated(rnd), , drop = FALSE]
  }
  colnames(rnd) <- names(current)
  retain <- min(retain, nrow(rnd))

  sample_by_distance(rnd, hist_values, retain = retain, pset = pset)
}

encode_set_backwards <- function(x, pset, ...) {
  pset <- pset[pset$id %in% names(x), ]
  mapply(
    check_backwards_encode,
    pset$object,
    x,
    pset$id,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  new_vals <- purrr::map2(
    pset$object,
    x,
    dials::encode_unit,
    direction = "backward"
  )
  names(new_vals) <- names(x)
  tibble::as_tibble(new_vals)
}

check_backwards_encode <- function(x, value, id) {
  if (!dials::has_unknowns(x)) {
    compl <- value[!is.na(value)]
    if (any(compl < 0) | any(compl > 1)) {
      cli::cli_abort(
        c(
          "!" = "The range for parameter {.val {noquote(id)}} used when \\
               generating initial results isn't compatible with the range \\
               supplied in {.arg param_info}.",
          "i" = "Possible values of parameters in {.arg param_info} should \\
               encompass all values evaluated in the initial grid."
        ),
        call = rlang::call2("tune_sim_anneal()")
      )
    }
  }
}

sample_by_distance <- function(candidates, existing, retain, pset) {
  if (nrow(existing) > 0) {
    existing <- tune::encode_set(existing, pset, as_matrix = TRUE)
    hist_index <- 1:nrow(existing)
    all_values <- rbind(existing, candidates)
    all_values <- stats::dist(all_values)
    all_values <- as.matrix(all_values)
    all_values <- all_values[hist_index, -hist_index, drop = FALSE]
    min_dist <- apply(all_values, 2, min)
    min_dist <- min_dist / max(min_dist)
    prob_wt <- min_dist^2
    prob_wt[is.na(prob_wt)] <- 0.0001

    if (diff(range(prob_wt)) < 0.0001) {
      prob_wt <- rep(1 / nrow(candidates), nrow(candidates))
    }
  } else {
    prob_wt <- rep(1 / nrow(candidates), nrow(candidates))
  }
  retain <- min(retain, nrow(candidates))

  candidates <- tibble::as_tibble(candidates)
  candidates <- encode_set_backwards(candidates, pset)

  selected <- sample(seq_along(prob_wt), size = retain, prob = prob_wt)
  candidates[selected, ]
}

## -----------------------------------------------------------------------------

update_history <- function(history, x, iter, eval_time) {
  analysis_metric <- tune::.get_tune_metric_names(x)[1]
  res <-
    tune::show_best(x, metric = analysis_metric, eval_time = eval_time) |>
    dplyr::mutate(
      .config = paste0("iter", iter),
      .iter = iter,
      random = runif(1),
      accept = NA_real_,
      results = NA_character_
    )
  if (is.null(history)) {
    history <- res
  } else {
    history <- dplyr::bind_rows(history, res)
  }

  if (maximize_metric(x, analysis_metric)) {
    best_res <- which.max(history$mean)
  } else {
    best_res <- which.min(history$mean)
  }

  history$global_best <- FALSE
  history$global_best[best_res] <- TRUE
  history
}

sa_decide <- function(x, parent, metric, maximize, coef) {
  res <- dplyr::filter(x, .metric == metric)
  latest_ind <- which.max(res$.iter)
  prev_ind <- which(res$.config == parent)
  prev_metric <- res$mean[prev_ind]
  latest_metric <- res$mean[latest_ind]
  all_prev <- res$mean[1:prev_ind]

  if (maximize) {
    is_best <- latest_metric > max(all_prev, na.rm = TRUE)
    is_better <- isTRUE(latest_metric > prev_metric)
  } else {
    is_best <- latest_metric < min(all_prev, na.rm = TRUE)
    is_better <- isTRUE(latest_metric < prev_metric)
  }

  m <- nrow(x)

  x$accept[m] <-
    acceptance_prob(
      current = prev_metric,
      new = latest_metric,
      iter = max(x$.iter),
      maximize = maximize,
      coef = coef
    )

  if (is_best) {
    x$results[m] <- "new best"
    x$random[m] <- x$accept[m] <- NA_real_
  } else if (is_better) {
    x$results[m] <- "better suboptimal"
    x$random[m] <- x$accept[m] <- NA_real_
  } else {
    if (x$random[m] <= x$accept[m]) {
      x$results[m] <- "accept suboptimal"
    } else {
      x$results[m] <- "discard suboptimal"
    }
  }
  x
}

initialize_history <- function(x, eval_time = NULL, ...) {
  # check to see if there is existing history
  res <-
    tune::collect_metrics(x) |>
    dplyr::filter(.metric == tune::.get_tune_metric_names(x)[1])
  if (!is.na(eval_time) && any(names(res) == ".eval_time")) {
    res <- res |> dplyr::filter(.eval_time == eval_time)
  }

  if (!any(names(res) == ".iter")) {
    res$.iter <- 0
  }

  res <-
    res |>
    dplyr::mutate(
      random = NA_real_,
      accept = NA_real_,
      results = "initial"
    )
  res
}


percent_diff <- function(current, new, maximize = TRUE) {
  if (isTRUE(all.equal(current, new))) {
    return(0.0)
  }
  if (maximize) {
    pct_diff <- (new - current) / current
  } else {
    pct_diff <- (current - new) / current
  }
  pct_diff * 100
}

acceptance_prob <- function(
  current,
  new,
  iter,
  maximize = TRUE,
  coef = 2 / 100
) {
  pct_diff <- percent_diff(current, new, maximize)
  if (pct_diff > 0) {
    return(1.0)
  }
  exp(pct_diff * coef * iter)
}

log_sa_progress <- function(
  control = list(verbose_iter = TRUE),
  x,
  metric,
  max_iter,
  maximize = TRUE,
  digits = 5
) {
  if (!control$verbose_iter) {
    return(invisible(NULL))
  }
  is_initial <- all(x$results == "initial")
  if (is_initial) {
    m <- max(which(x$global_best))
    new_res <- x$mean[m]
    new_std <- x$std_err[m]
    new_event <- x$results[m]
  } else {
    m <- nrow(x)
    new_res <- x$mean[m]
    new_std <- x$std_err[m]
    new_event <- x$results[m]
  }
  iter <- max(x$.iter)
  if (iter > 0 & !is_initial) {
    is_best <- isTRUE(x$global_best[m])
    prev_res <- x$mean[m - 1]
    pct_diff <- percent_diff(prev_res, new_res, maximize) * 100
    pct_diff <- sprintf("%6.2f", pct_diff)
  } else {
    is_best <- FALSE
    pct_diff <- NA_real_
  }

  chr_iter <- format(1:max_iter)[iter]
  dig <- paste0("%.", digits, "f")

  cols <- tune::get_tune_colors()
  if (iter > 0) {
    msg <- paste0(metric, "=", signif(new_res, digits = digits))
    if (!is.na(new_std) && new_std > 0) {
      msg <- paste0(msg, "\t(+/-", signif(new_std, digits = digits - 1), ")")
    }
    msg <- paste(chr_iter, format_event(new_event), msg)
  } else {
    if (maximize) {
      initial_res <- max(x$mean[x$.iter == 0], na.rm = TRUE)
    } else {
      initial_res <- min(x$mean[x$.iter == 0], na.rm = TRUE)
    }
    msg <- paste0(
      "Initial best: ",
      sprintf(dig, signif(initial_res, digits = digits))
    )
  }

  cli::cli_bullets(cols$message$info(msg))
}

# fmt: skip
format_event <- function(x) {
  result_key <- tibble::tribble(
    ~orig,               ~symb,
    "initial",            cli::symbol$tick,
    "new best",           cli::symbol$heart,
    "better suboptimal",  "+",
    "discard suboptimal", cli::symbol$line,
    "accept suboptimal",  cli::symbol$circle,
    "restart from best",  cli::symbol$cross
  ) |>
    dplyr::mutate(
      new = format(orig, justify = "left"),
      new = gsub(" ", "\u00a0", new, fixed = TRUE),
      result = paste(symb, new)
    )
  color_event(result_key$result[result_key$orig == x])
}

color_event <- function(x) {
  cols <- tune::get_tune_colors()
  dplyr::case_when(
    grepl("initial", x) ~ cols$symbol$info(x),
    grepl("new", x) ~ cols$symbol$success(x),
    grepl("better", x) ~ cols$symbol$success(x),
    grepl("discard", x) ~ cols$message$danger(x),
    grepl("accept", x) ~ cols$message$warning(x),
    grepl("restart", x) ~ cols$message$danger(x),
    TRUE ~ cols$message$info(x)
  )
}

get_outcome_names <- function(x, rs) {
  preproc <- extract_preprocessor(x)
  if (inherits(preproc, "workflow_variables")) {
    if (any(names(preproc) == "outcomes")) {
      dat <- rs$splits[[1]]$data
      res <- tidyselect::eval_select(preproc$outcomes, data = dat)
      res <- names(res)
    } else {
      cli::cli_abort("Cannot obtain the outcome name(s)")
    }
  } else {
    res <- outcome_names(x)
  }
  res
}

update_config <- function(x, prefix = NULL, config = "new", save_pred) {
  if (!is.null(prefix)) {
    x$.metrics <-
      purrr::map(
        x$.metrics,
        \(x) dplyr::mutate(x, .config = paste0(prefix, "_", .config))
      )
    if (save_pred) {
      x$.predictions <-
        purrr::map(
          x$.predictions,
          \(x) dplyr::mutate(x, .config = paste0(prefix, "_", .config))
        )
    }
  } else {
    x$.metrics <-
      purrr::map(
        x$.metrics,
        \(x) dplyr::mutate(x, .config = config)
      )
    if (save_pred) {
      x$.predictions <-
        purrr::map(
          x$.predictions,
          \(x) dplyr::mutate(x, .config = config)
        )
    }
  }
  x
}
