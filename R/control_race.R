#' Control aspects of the grid search racing process
#'
#' @inheritParams tune::control_grid
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
#' For repeated cross-validation the randomization occurs within each repeat.
#' @return An object of class `control_race` that echos the argument values.
#' @examples
#' control_race()
#' @export
control_race <-
  function(
    verbose = FALSE,
    verbose_elim = FALSE,
    allow_par = TRUE,
    extract = NULL,
    save_pred = FALSE,
    burn_in = 3,
    num_ties = 10,
    alpha = 0.05,
    randomize = TRUE,
    pkgs = NULL,
    save_workflow = FALSE,
    event_level = "first",
    parallel_over = "everything",
    backend_options = NULL
  ) {
    # Any added arguments should also be added in superset control functions
    # in other package. In other words, if tune_grid adds an option, the same
    # object should be added here (regardless)

    tune::val_class_and_single(verbose, "logical", "control_race()")
    tune::val_class_and_single(verbose_elim, "logical", "control_race()")
    tune::val_class_and_single(allow_par, "logical", "control_race()")
    tune::val_class_and_single(alpha, "numeric", "control_race()")
    tune::val_class_and_single(burn_in, "numeric", "control_race()")
    tune::val_class_and_single(randomize, "logical", "control_race()")
    tune::val_class_and_single(num_ties, "numeric", "control_race()")
    tune::val_class_and_single(save_pred, "logical", "control_race()")
    tune::val_class_or_null(pkgs, "character", "control_race()")
    tune::val_class_and_single(event_level, "character", "control_race()")
    tune::val_class_or_null(extract, "function", "control_race()")
    tune::val_class_and_single(save_workflow, "logical", "control_race()")
    if (!is.null(parallel_over)) {
      val_parallel_over(parallel_over, "control_sim_anneal()")
    }

    if (alpha <= 0 | alpha >= 1) {
      cli::cli_abort("{.arg alpha} should be on (0, 1).")
    }

    if (burn_in < 2) {
      cli::cli_abort("{.arg burn_in} should be at least two.")
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
      event_level = event_level,
      backend_options = backend_options
    )

    class(res) <- c("control_race")
    res
  }

#' @export
print.control_race <- function(x, ...) {
  cat("Racing method control object\n")
  invisible(x)
}
