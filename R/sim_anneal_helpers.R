maximize_metric <- function(x, metric) {
  metrics <- .get_tune_metrics(x)
  metrics_data <- tune:::metrics_info(metrics)
  x <- metrics_data$.metric[1]
  metrics_data$direction[metrics_data$.metric == metric] == "maximize"
}


new_in_neighborhood <- function(current, pset, radius = 0.025, flip = 0.1) {
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
  encoded <- tune:::encode_set(current, pset, as_matrix = TRUE)

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
  rnd <- as_tibble(rnd)
  rnd <- encode_set_backwards(rnd, pset)
  selected <- rnd %>% sample_n(retain)
  selected
}

encode_set_backwards <- function(x, pset, ...) {
  pset <- pset[pset$id %in% names(x),]
  new_vals <- purrr::map2(pset$object, x, encode_unit, direction = "backward")
  names(new_vals) <- names(x)
  tibble::as_tibble(new_vals)
}

## -----------------------------------------------------------------------------

update_history <- function(history, x, iter) {
  analysis_metric <- tune::.get_tune_metric_names(x)[1]
  res <-
    show_best(x, metric = analysis_metric) %>%
    dplyr::select(.metric, mean, n, std_err) %>%
    dplyr::mutate(
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
    best_res <- history$.iter[which.max(history$mean)]
  } else {
    best_res <- history$.iter[which.min(history$mean)]
  }

  history <-
    history %>%
    mutate(global_best = .iter == best_res)
  history %>%
    dplyr::select(.iter, .metric, mean, n, std_err, random, accept, results, global_best)
}

get_sa_param <- function(x) {
  nms <- .get_tune_parameter_names(x)
  x$.metrics[[1]] %>%
    distinct(!!!syms(nms))

}

sa_decide <- function(x, metric, maximize, ...) {
  latest_iter <- max(x$.iter)
  prev_iter <- latest_iter - 1
  prev_metric   <- x$mean[x$.metric == metric & x$.iter == prev_iter]
  latest_metric <- x$mean[x$.metric == metric & x$.iter == latest_iter]

  if (maximize) {
    prev_metric   <- max(prev_metric, na.rm = TRUE)
    latest_metric <- max(latest_metric, na.rm = TRUE)
    better_result <- isTRUE(latest_metric > prev_metric)
  } else {
    prev_metric   <- min(prev_metric, na.rm = TRUE)
    latest_metric <- min(latest_metric, na.rm = TRUE)
    better_result <- isTRUE(latest_metric < prev_metric)
  }

  m <- nrow(x)

  x$accept[m] <-
    acceptance_prob(
      current = prev_metric,
      new = latest_metric,
      iter = latest_iter,
      maximize = maximize,
      ...
    )

  if (better_result) {
    x$results[m] <- "improvement"
    x$random[m] <- x$accept[m] <- NA_real_
  } else {
    if (x$random[m] <= x$accept[m]) {
      x$results[m] <- "accept"
    } else {
      x$results[m] <- "discard"
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
  pct_diff
}

acceptance_prob <- function(current, new, iter, maximize = TRUE, coef = 2) {
  pct_diff <- percent_diff(current, new, maximize)
  if (pct_diff > 0) {
    return(1.0)
  }
  exp(pct_diff * coef * iter)
}


iter_since_x <- function(x, mset) {
  max_iter <- max(x$.iter)
  best_iter <- x$.iter[x$global_best]
  if (any(x$results == "improvement")) {
    last_imp <- max(x$.iter[x$results == "improvement"], na.rm = TRUE)
  } else {
    last_imp <- Inf
  }
  list(improve = max_iter - last_imp, best = max_iter - best_iter)
}


is_new_best <- function(x, iter, mset) {
  iter == x$.iter[which.max(x$mean)]
}

log_sa_progress <- function(control = list(verbose = TRUE), x, metric, max_iter, maximize = TRUE, digits = 7) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  m <- nrow(x)
  new_res <- x$mean[m]
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

  if (iter > 0) {
    msg <- paste0(" ", metric, ": ", sprintf(dig, signif(new_res, digits = digits)))
    msg <- paste0(msg,  "\t")  # "\t(", pct_diff, "%)  "
    symb <- dplyr::case_when(
      new_event == "improvement" ~ crayon::green(cli::symbol$heart),
      new_event == "discard"     ~ crayon::red(cli::symbol$circle_cross),
      new_event == "accept"      ~ crayon::silver("+"),
      new_event == "restart"     ~ crayon::silver(cli::symbol$radio_on),
      TRUE                       ~ crayon::black(cli::symbol$info)        # accept
    )
    if (is_best) {
      new_event <- paste(new_event, "(new best)")
    }
    msg <- paste0(chr_iter, " ", symb, "\t", msg, " ", new_event)
  } else {
    initial_res <- max(x$mean[x$.iter == 0], na.rm = TRUE)
    msg <- paste0("Initial best: ", sprintf(dig, signif(initial_res, digits = digits)))
  }

  message(msg)
}

