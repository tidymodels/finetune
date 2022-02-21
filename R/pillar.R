
#' @export
obj_sum.tune_race <- function(x, ...) {
  null_metrics <- purrr::map_lgl(x$.metrics, is.null)
  if (!all(null_metrics)) {
    res <- "race[+]"
  } else {
    res <- "race[x]"
  }
  res
}

#' @export
size_sum.tune_race <- function(x, ...) {
  ""
}

newer_tibble <- function() {
  utils::packageVersion("tibble") >= "3.0.6"
}
