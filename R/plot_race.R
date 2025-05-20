#' Plot racing results
#'
#' Plot the model results over stages of the racing results. A line is given
#' for each submodel that was tested.
#' @param x A object with class `tune_results`
#' @return A ggplot object.
#' @export
plot_race <- function(x) {
  metric <- tune::.get_tune_metric_names(x)[1]
  ex_mtrc <- collect_metrics(x)

  if (any(names(ex_mtrc) == ".eval_time")) {
    eval_time <- min(ex_mtrc$.eval_time, na.rm = TRUE)
  } else {
    eval_time <- NULL
  }

  rs <-
    x |>
    dplyr::select(id, .order, .metrics) |>
    tidyr::unnest(cols = .metrics) |>
    dplyr::filter(.metric == metric)

  if (!is.null(eval_time) && any(names(rs) == ".eval_time")) {
    rs <- dplyr::filter(rs, .eval_time == eval_time)
  }

  .order <- sort(unique(rs$.order))
  purrr::map(.order, \(x) stage_results(x, rs)) |>
    purrr::list_rbind() |>
    ggplot2::ggplot(ggplot2::aes(
      x = stage,
      y = mean,
      group = .config,
      col = .config
    )) +
    ggplot2::geom_line(alpha = .5, show.legend = FALSE) +
    ggplot2::xlab("Analysis Stage") +
    ggplot2::ylab(metric) +
    ggplot2::scale_x_continuous(breaks = integer_breaks)
}

integer_breaks <- function(lims) {
  breaks <- pretty(lims)

  unique(round(breaks))
}

stage_results <- function(ind, x) {
  res <-
    x |>
    dplyr::filter(.order <= ind) |>
    dplyr::group_by(.config) |>
    dplyr::summarize(
      mean = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      .groups = "drop"
    ) |>
    dplyr::mutate(stage = ind) |>
    dplyr::ungroup() |>
    dplyr::filter(n == ind)
}
