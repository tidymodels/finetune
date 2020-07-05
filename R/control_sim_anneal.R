#' Control aspects of the simulated annealing search process
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

#' @export
control_sim_anneal <-
  function(verbose = FALSE,
           improve_iter = Inf,
           restart_iter = 10L,
           radius = 0.025,
           flip = 0.1,
           seed = sample.int(10^5, 1),
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA,
           pkgs = NULL) {
    # add options for seeds per resample

    tune:::val_class_and_single(verbose, "logical", "control_sim_anneal()")
    tune:::val_class_and_single(save_pred, "logical", "control_sim_anneal()")
    tune:::val_class_and_single(improve_iter, c("numeric", "integer"), "control_sim_anneal()")
    tune:::val_class_and_single(restart_iter, c("numeric", "integer"), "control_sim_anneal()")
    tune:::val_class_and_single(seed, c("numeric", "integer"), "control_sim_anneal()")
    tune:::val_class_or_null(extract, "function", "control_sim_anneal()")
    tune:::val_class_and_single(time_limit, c("logical", "numeric"), "control_sim_anneal()")
    tune:::val_class_or_null(pkgs, "character", "control_sim_anneal()")

    # if (!is.infinite(uncertain) && uncertain > no_improve) {
    #   cli::cli_alert_warning(
    #     "Uncertainty sample scheduled after {uncertain} poor iterations but the search will stop after {no_improve}."
    #   )
    # }

    res <-
      list(
        verbose = verbose,
        improve_iter = improve_iter,
        restart_iter = restart_iter,
        radius = radius,
        flip = flip,
        seed = seed,
        extract = extract,
        save_pred = save_pred,
        time_limit = time_limit,
        pkgs = pkgs
      )

    class(res) <- "control_sim_anneal"
    res
  }

#' @export
print.control_sim_anneal <- function(x, ...) {
  cat("Simulated annealing control object\n")
  invisible(x)
}
