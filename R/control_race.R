#' Control aspects of the grid search racing process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#'   this argument, warnings and errors are always shown. If using a dark IDE
#'   theme, some logging messages might be hard to see. If this is the case,
#'   try setting the `tidymodels.dark` option with
#'   `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param no_improve The integer cutoff for the number of iterations without
#'   better results.
#' @param uncertain The number of iterations with no improvement before an
#'  uncertainty sample is created where a sample with high predicted variance is
#'  chosen (i.e., in a region that has not yet been explored). The iteration
#'  counter is reset after each uncertainty sample. For example, if `uncertain =
#'  10`, this condition is triggered every 10 samples with no improvement.
#' @param seed An integer for controlling the random number stream.
#' @param time_limit A number for the minimum number of _minutes_ (elapsed) that
#'   the function should execute. The elapsed time is evaluated at internal
#'   checkpoints and, if over time, the results at that time are returned (with
#'   a warning). This means that the `time_limit` is not an exact limit, but a
#'   minimum time limit.
#' @param extract An optional function with at least one argument (or `NULL`)
#'   that can be used to retain arbitrary objects from the model fit object,
#'   recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#'   be saved for each model _evaluated_.
#' @param pkgs An optional character string of R package names that should be
#'   loaded (by namespace) during parallel processing.
#' @param allow_par A logical to allow parallel processing (if a parallel
#'   backend is registered).
#'
#' @export
control_race <-
  function(verbose = FALSE, allow_par = TRUE, extract = NULL, save_pred = FALSE,
           burn_in = 3, alpha = 0.05, randomize = TRUE, pkgs = NULL) {

    tune::val_class_and_single(verbose, "logical", "control_grid()")
    tune::val_class_and_single(allow_par, "logical", "control_grid()")
    tune::val_class_and_single(save_pred, "logical", "control_grid()")
    tune::val_class_or_null(pkgs, "character", "control_grid()")
    tune::val_class_or_null(extract, "function", "control_grid()")

    res <- list(
      verbose = verbose,
      allow_par = allow_par,
      extract = extract,
      save_pred = save_pred,
      alpha = alpha,
      burn_in = burn_in,
      randomize = randomize,
      pkgs = pkgs
    )

    class(res) <- c("control_race")
    res
  }

#' @export
print.control_race <- function(x, ...) {
  cat("Racing method control object\n")
  invisible(x)
}
