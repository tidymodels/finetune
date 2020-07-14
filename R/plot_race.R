#' Plot racing results
#'
#' Plot difference parameter combinations versus the number of times that each
#' was evaluated. The plot colors the results by the performance metric that was
#' being optimized/tested.
#' @param x A object with class `tune_results`
#' @return A ggplot object.
#' @export
plot_race <- function(x) {
  met <- tune::.get_tune_metric_names(grid_anova)[1]
  x %>%
    tune::collect_metrics() %>%
    dplyr::filter(.metric == met) %>%
    dplyr::mutate(
      .config = reorder(.config, n),
      .lower = mean - 1.96 * std_err,
      .upper = mean + 1.96 * std_err
      ) %>%
    ggplot2::ggplot(ggplot2::aes(x = n, y = .config)) +
    ggplot2::geom_point(aes(size = mean)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = 0, xmax = n), alpha = .25) +
    ggplot2::labs(x = "Number of Resamples", y = NULL)
}

