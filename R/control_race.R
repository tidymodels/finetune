#' Control aspects of the grid search racing process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#'   this argument, warnings and errors are always shown. If using a dark IDE
#'   theme, some logging messages might be hard to see. If this is the case,
#'   try setting the `tidymodels.dark` option with
#'   `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param verbose_elim A logical for whether logging of the elimination of
#'  tuning parameter combinations should occur.
#' @param burn_in An integer for how many resamples should be completed for all
#'  grid combinations before parameter filtering begins.
#' @param num_ties An integer for when tie-breaking should occur. If there are
#'  two final parameter combinations being evaluated, `num_ties` specified how
#'  many more resampling iterations should be evaluated. After `num_ties` more
#'  iterations, the parameter combination with the current best results is
#'  retained.
#' @param alpha The alpha level for a one-sided confidence interval for each
#'  parameter combination.
#' @param randomize Should the resamples be evaluated in a random order?  By
#' default, the resamples are evaluated in a random order so the random number
#' seed should be control prior to calling this method (to be reproducible).
#' @param extract An optional function with at least one argument (or `NULL`)
#'   that can be used to retain arbitrary objects from the model fit object,
#'   recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#'   be saved for each model _evaluated_.
#' @param pkgs An optional character string of R package names that should be
#'   loaded (by namespace) during parallel processing.
#' @param allow_par A logical to allow parallel processing (if a parallel
#'   backend is registered).
#' @param save_workflow A logical for whether the workflow should be appended
#' to the output as an attribute.
#'
#' @export
control_race <-
  function(verbose = FALSE, verbose_elim = TRUE, allow_par = TRUE,
           extract = NULL, save_pred = FALSE,
           burn_in = 3, num_ties = 10, alpha = 0.05, randomize = TRUE,
           pkgs = NULL, save_workflow = FALSE) {

    tune::val_class_and_single(verbose,       "logical",   "control_grid()")
    tune::val_class_and_single(verbose_elim,  "logical",   "control_grid()")
    tune::val_class_and_single(allow_par,     "logical",   "control_grid()")
    tune::val_class_and_single(alpha,         "numeric",   "control_grid()")
    tune::val_class_and_single(burn_in,       "numeric",   "control_grid()")
    tune::val_class_and_single(randomize,     "logical",   "control_grid()")
    tune::val_class_and_single(num_ties,      "numeric",   "control_grid()")
    tune::val_class_and_single(save_pred,     "logical",   "control_grid()")
    tune::val_class_or_null(pkgs,             "character", "control_grid()")
    tune::val_class_or_null(extract,          "function",  "control_grid()")
    tune::val_class_and_single(save_workflow, "logical",   "control_grid()")

    res <- list(
      verbose = verbose,
      verbose_elim = verbose_elim,
      allow_par = allow_par,
      extract = extract,
      save_pred = save_pred,
      alpha = alpha,
      burn_in = burn_in,
      num_ties = num_ties,
      randomize = randomize,
      pkgs = pkgs,
      save_workflow = save_workflow
    )

    class(res) <- c("control_race")
    res
  }

#' @export
print.control_race <- function(x, ...) {
  cat("Racing method control object\n")
  invisible(x)
}

