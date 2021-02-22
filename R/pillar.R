
#' @export
#' @keywords internal
#' @rdname pillar-helpers
obj_sum.tune_race <- function(x) {
  null_metrics <- purrr::map_lgl(x$.metrics, is.null)
  if (!all(null_metrics)) {
    res <- "race[+]"
  } else {
    res <- "race[x]"
  }
  res
}

#' Helpers for pillar formatting
#' @param x an object
#' @return A character string.
#' @export
#' @keywords internal
#' @rdname pillar-helpers
size_sum.tune_race <- function(x) {
  ""
}

newer_tibble <- function() {
  utils::packageVersion("tibble") >= "3.0.6"
}
