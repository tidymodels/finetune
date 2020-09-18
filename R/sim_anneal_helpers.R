maximize_metric <- function(x, metric) {
  metrics <- tune::.get_tune_metrics(x)
  metrics_data <- tune::metrics_info(metrics)
  x <- metrics_data$.metric[1]
  metrics_data$direction[metrics_data$.metric == metric] == "maximize"
}


new_in_neighborhood <- function(current, pset, radius = 0.025, flip = 0.1) {
  current <- dplyr::select(current, !!!pset$id)
  is_quant <- purrr::map_lgl(pset$object, inherits, "quant_param")
  if (any(is_quant)) {
    quant_nms <- pset$id[is_quant]
    new_quant <- random_neighbor(current, pset, r = radius)
    current[, quant_nms] <- new_quant
  }
  if (any(!is_quant)) {
    qual_nms <- pset$id[!is_quant]
    new_qual <- random_flip(current, pset, prob = flip)
    current[, qual_nms] <- new_qual
  }
  current
}

random_flip <- function(current, pset, prob) {
  is_qual <- purrr::map_lgl(pset$object, inherits, "qual_param")
  qual_nms <- pset$id[is_qual]
  new_val <- runif(length(qual_nms)) <= prob
  if (any(new_val)) {
    new_vals <- qual_nms[new_val]
    for (i in new_vals) {
      current_val <- current[[i]]
      parm_obj <- pset$object[[which(pset$id == i)]]
      parm_obj$values <- setdiff(parm_obj$values, current_val)
      current[[i]] <- dials::value_sample(parm_obj, 1)
    }
  }
  current[, is_qual]
}


random_neighbor <- function(current, pset, retain = 1, tries = 500, r = .025) {
  is_quant <- purrr::map_lgl(pset$object, inherits, "quant_param")
  current <- current[, is_quant]
  pset <- pset[is_quant, ]
  encoded <- tune::encode_set(current, pset, as_matrix = TRUE)

  num_param <- ncol(encoded)
  rnd <- rnorm(num_param * tries)
  rnd <- matrix(rnd, ncol = num_param)
  rnd <- t(apply(rnd, 1, function(x) x/sqrt(sum(x^2))))
  rnd <- rnd * r
  rnd <- sweep(rnd, 2, as.vector(encoded), "+")
  outside <- apply(rnd, 1, function(x) any(x > 1 | x < 0))
  rnd <- rnd[!outside,,drop = FALSE]
  retain <- min(retain, nrow(rnd))
  colnames(rnd) <- colnames(current)
  rnd <- tibble::as_tibble(rnd)
  rnd <- encode_set_backwards(rnd, pset)
  selected <- rnd %>% dplyr::sample_n(retain)
  selected
}

encode_set_backwards <- function(x, pset, ...) {
  pset <- pset[pset$id %in% names(x),]
  new_vals <- purrr::map2(pset$object, x, dials::encode_unit, direction = "backward")
  names(new_vals) <- names(x)
  tibble::as_tibble(new_vals)
}

## -----------------------------------------------------------------------------

update_history <- function(history, x, iter) {
  analysis_metric <- tune::.get_tune_metric_names(x)[1]
  res <-
    tune::show_best(x, metric = analysis_metric) %>%
    # dplyr::select(.metric, mean, n, std_err) %>%
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
  prev_metric   <- res$mean[prev_ind]
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

initialize_history <- function(x, ...) {
  # check to see if there is existing history
  res <-
    tune::collect_metrics(x)%>%
    dplyr::filter(.metric == tune::.get_tune_metric_names(x)[1]) %>%
    dplyr::mutate(
      .iter = 0,
      random = NA_real_,
      accept = NA_real_,
      results = "initial"
    )
  res
}


percent_diff <- function(current, new, maximize = TRUE) {
  if (maximize) {
    pct_diff <- (new - current)/current
  } else {
    pct_diff <- (current - new)/current
  }
  pct_diff * 100
}

acceptance_prob <- function(current, new, iter, maximize = TRUE, coef = 2/100) {
  pct_diff <- percent_diff(current, new, maximize)
  if (pct_diff > 0) {
    return(1.0)
  }
  exp(pct_diff * coef * iter)
}

log_sa_progress <- function(control = list(verbose = TRUE), x, metric, max_iter, maximize = TRUE, digits = 5) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  m <- nrow(x)
  new_res <- x$mean[m]
  new_std <- x$std_err[m]
  new_event <- x$results[m]
  iter <- max(x$.iter)
  if (iter > 0) {
    is_best <- isTRUE(x$global_best[m])
    prev_res <- x$mean[m - 1]
    pct_diff <- percent_diff(prev_res, new_res, maximize) *100
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
      msg <- paste0(msg,  "\t(+/-", signif(new_std, digits = digits - 1), ")")
    }
    msg <- paste(chr_iter, format_event(new_event), msg)
  } else {
    initial_res <- max(x$mean[x$.iter == 0], na.rm = TRUE)
    msg <- paste0("Initial best: ", sprintf(dig, signif(initial_res, digits = digits)))
  }

  rlang::inform(cols$message$info(msg))
}

format_event <- function(x) {
  result_key <- tibble::tribble(
    ~ orig,               ~ symb,
    "new best",           cli::symbol$heart,
    "better suboptimal",  "+",
    "discard suboptimal", cli::symbol$line,
    "accept suboptimal",  cli::symbol$circle,
    "restart from best",  cli::symbol$cross
  ) %>%
    dplyr::mutate(
      new = format(orig, justify = "left"),
      result = paste(symb, new)
    )
  color_event(result_key$result[result_key$orig == x])
}

color_event <- function(x) {
  cols <- tune::get_tune_colors()
  dplyr::case_when(
    grepl("new", x)      ~ cols$symbol$success(x),
    grepl("better", x)   ~ cols$symbol$success(x),
    grepl("discard", x)  ~ cols$message$danger(x),
    grepl("accept", x)   ~ cols$message$warning(x),
    grepl("restart", x)  ~ cols$message$danger(x),
    TRUE                 ~ cols$message$info(x)
  )
}


