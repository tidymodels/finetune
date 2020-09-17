#' Control aspects of the simulated annealing search process
#'
#' @param verbose A logical for logging results as they are generated. Despite
#'   this argument, warnings and errors are always shown. If using a dark IDE
#'   theme, some logging messages might be hard to see. If this is the case,
#'   try setting the `tidymodels.dark` option with
#'   `options(tidymodels.dark = TRUE)` to print lighter colors.
#' @param no_improve The integer cutoff for the number of iterations without
#'   better results.
#' @param restart The number of iterations with no improvement before new tuning
#' parameter candidates are generated from the last, overall best conditions.
#' @param radius A real number on `(0, 1)` describing what a value "in the
#' neighborhood" of the current result should be. If all numeric parameters were
#' scaled to be on the `[0, 1]` scale, this is the radius of a circle used to
#' generate new numeric parameter values.
#' @param flip A real number between `[0, 1]` for the probability of changing
#' any non-numeric parameter values at each iteration.
#' @param cooling_coef A real, positive number to influence the cooling
#' schedule. Larger values decrease the probability of accepting a sub-optimal
#' parameter setting.
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
#' @param save_workflow A logical for whether the workflow should be appended
#' to the output as an attribute.
#' @export
control_sim_anneal <-
  function(verbose = FALSE,
           no_improve = Inf,
           restart = 8L,
           radius = 0.1,
           flip = 0.2,
           cooling_coef = 0.02,
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA,
           pkgs = NULL,
           save_workflow = FALSE) {
    # add options for seeds per resample

    tune::val_class_and_single(verbose, "logical", "control_sim_anneal()")
    tune::val_class_and_single(save_pred, "logical", "control_sim_anneal()")
    tune::val_class_and_single(no_improve, c("numeric", "integer"), "control_sim_anneal()")
    tune::val_class_and_single(restart, c("numeric", "integer"), "control_sim_anneal()")
    tune::val_class_and_single(radius, "numeric", "control_sim_anneal()")
    tune::val_class_and_single(flip, "numeric", "control_sim_anneal()")
    tune::val_class_and_single(cooling_coef, "numeric", "control_sim_anneal()")
    tune::val_class_or_null(extract, "function", "control_sim_anneal()")
    tune::val_class_and_single(time_limit, c("logical", "numeric"), "control_sim_anneal()")
    tune::val_class_or_null(pkgs, "character", "control_sim_anneal()")
    tune::val_class_and_single(save_workflow, "logical", "control_sim_anneal()")

    radius[radius <= 0] <- 0.001
    radius[radius >= 1] <- 0.999
    flip[flip < 0] <- 0
    flip[flip > 1] <- 1
    cooling_coef[cooling_coef <= 0] <- 0.0001

    if (no_improve < 2) {
      rlang::abort("'no_improve' should be > 1")
    }
    if (restart < 2) {
      rlang::abort("'restart' should be > 1")
    }
    if (!is.infinite(restart) && restart > no_improve) {
      cli::cli_alert_warning(
        "Parameter restart is scheduled after {restart} poor iterations but the search will stop after {no_improve}."
      )
    }

    res <-
      list(
        verbose = verbose,
        no_improve = no_improve,
        restart = restart,
        radius = radius,
        flip = flip,
        cooling_coef = cooling_coef,
        extract = extract,
        save_pred = save_pred,
        time_limit = time_limit,
        pkgs = pkgs,
        save_workflow = save_workflow
      )

    class(res) <- "control_sim_anneal"
    res
  }

#' @export
print.control_sim_anneal <- function(x, ...) {
  cat("Simulated annealing control object\n")
  invisible(x)
}
