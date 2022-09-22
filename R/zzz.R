.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("tune::show_best", "tune_race")
  vctrs::s3_register("tune::collect_metrics", "tune_race")
  vctrs::s3_register("tune::collect_predictions", "tune_race")
}
