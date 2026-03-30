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
    backend_options = NULL,
    workflow_size = 100
  ) {
    # Any added arguments should also be added in superset control functions
    # in other package. In other words, if tune_grid adds an option, the same
    # object should be added here (regardless)

    check_bool(verbose)
    check_bool(verbose_elim)
    check_bool(allow_par)
    check_number_decimal(alpha)
    check_number_whole(burn_in, min = 2)
    check_bool(randomize)
    check_number_whole(num_ties)
    check_bool(save_pred)
    check_character(pkgs, allow_null = TRUE)
    check_string(event_level)
    check_function(extract, allow_null = TRUE)
    check_bool(save_workflow)
    check_number_decimal(workflow_size)
    if (!is.null(parallel_over)) {
      rlang::arg_match0(parallel_over, c("resamples", "everything"))
    }

    if (alpha <= 0 | alpha >= 1) {
      cli::cli_abort("{.arg alpha} should be on (0, 1).")
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
      backend_options = backend_options,
      workflow_size = workflow_size
    )

    class(res) <- c("control_race")
    res
  }

#' @export
print.control_race <- function(x, ...) {
  cat("Racing method control object\n")
  invisible(x)
}
