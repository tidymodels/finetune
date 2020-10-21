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
#' @param event_level A single string containing either "first" or "second".
#' This argument is passed on to `yardstick` metric functions when any type of
#' class prediction is made, and specifies which level of the outcome is
#' considered the "event".
#' @param parallel_over A single string containing either `"resamples"` or
#'   `"everything"` describing how to use parallel processing.
#'
#'   If `"resamples"`, then tuning will be performed in parallel over resamples
#'   alone. Within each resample, the preprocessor (i.e. recipe or formula) is
#'   processed once, and is then reused across all models that need to be fit.
#'
#'   If `"everything"`, then tuning will be performed in parallel at two levels.
#'   An outer parallel loop will iterate over resamples. Additionally, an
#'   inner parallel loop will iterate over all unique combinations of
#'   preprocessor and model tuning parameters for that specific resample. This
#'   will result in the preprocessor being re-processed multiple times, but
#'   can be faster if that processing is extremely fast.
#'
#'   Note that this parameter has a different default (`"everything"`) than its
#'   counterpart in [tune::control_grid()]. This is the most advantageous value
#'   for racing.
#'
#' @export
control_race <-
  function(verbose = FALSE, verbose_elim = FALSE, allow_par = TRUE,
           extract = NULL, save_pred = FALSE,
           burn_in = 3, num_ties = 10, alpha = 0.05, randomize = TRUE,
           pkgs = NULL, save_workflow = FALSE, event_level = "first",
           parallel_over = "everything") {

    tune::val_class_and_single(verbose,       "logical",   "control_race()")
    tune::val_class_and_single(verbose_elim,  "logical",   "control_race()")
    tune::val_class_and_single(allow_par,     "logical",   "control_race()")
    tune::val_class_and_single(alpha,         "numeric",   "control_race()")
    tune::val_class_and_single(burn_in,       "numeric",   "control_race()")
    tune::val_class_and_single(randomize,     "logical",   "control_race()")
    tune::val_class_and_single(num_ties,      "numeric",   "control_race()")
    tune::val_class_and_single(save_pred,     "logical",   "control_race()")
    tune::val_class_or_null(pkgs,             "character", "control_race()")
    tune::val_class_and_single(event_level,   "character", "control_race()")
    tune::val_class_or_null(extract,          "function",  "control_race()")
    tune::val_class_and_single(save_workflow, "logical",   "control_race()")
    val_parallel_over(parallel_over, "control_bayes()")

    if (alpha <= 0 | alpha >= 1) {
      rlang::abort("'alpha' should be on (0, 1)")
    }

    if (burn_in < 2) {
      rlang::abort("'burn_in' should be at least two.")
    }

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
      save_workflow = save_workflow,
      parallel_over = parallel_over,
      event_level = event_level
    )

    class(res) <- c("control_race")
    res
  }

#' @export
print.control_race <- function(x, ...) {
  cat("Racing method control object\n")
  invisible(x)
}

