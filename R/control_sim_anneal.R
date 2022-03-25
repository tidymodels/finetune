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
#' @param radius Two real numbers on `(0, 1)` describing what a value "in the
#' neighborhood" of the current result should be. If all numeric parameters were
#' scaled to be on the `[0, 1]` scale, these values set the min. and max.
#' of a radius of a circle used to generate new numeric parameter values.
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
#' @param save_history A logical to save the iteration details of the search.
#'  These are saved to `tempdir()` named `sa_history.RData`. These results are
#'  deleted when the R session ends. This option is only useful for teaching
#'  purposes.
#' @param event_level A single string containing either "first" or "second".
#' This argument is passed on to `yardstick` metric functions when any type of
#' class prediction is made, and specifies which level of the outcome is
#' considered the "event".
#' @param parallel_over A single string containing either `"resamples"` or
#'   `"everything"` describing how to use parallel processing. Alternatively,
#'   `NULL` is allowed, which chooses between `"resamples"` and `"everything"`
#'   automatically.
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
#'   If `NULL`, chooses `"resamples"` if there are more than one resample,
#'   otherwise chooses `"everything"` to attempt to maximize core utilization.
#' @return An object of class `control_sim_anneal` that echos the argument values.
#' @examples
#' control_sim_anneal()
#' @export
control_sim_anneal <-
  function(verbose = TRUE,
           no_improve = Inf,
           restart = 8L,
           radius = c(0.05, 0.15),
           flip = 3 / 4,
           cooling_coef = 0.02,
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA,
           pkgs = NULL,
           save_workflow = FALSE,
           save_history = FALSE,
           event_level = "first",
           parallel_over = NULL) {
    tune::val_class_and_single(verbose, "logical", "control_sim_anneal()")
    tune::val_class_and_single(save_pred, "logical", "control_sim_anneal()")
    tune::val_class_and_single(no_improve, c("numeric", "integer"), "control_sim_anneal()")
    tune::val_class_and_single(restart, c("numeric", "integer"), "control_sim_anneal()")
    tune::val_class_and_single(flip, "numeric", "control_sim_anneal()")
    tune::val_class_and_single(cooling_coef, "numeric", "control_sim_anneal()")
    tune::val_class_or_null(extract, "function", "control_sim_anneal()")
    tune::val_class_and_single(time_limit, c("logical", "numeric"), "control_sim_anneal()")
    tune::val_class_or_null(pkgs, "character", "control_sim_anneal()")
    tune::val_class_and_single(save_workflow, "logical", "control_sim_anneal()")
    tune::val_class_and_single(save_history, "logical", "control_sim_anneal()")
    if (!is.null(parallel_over)) {
      val_parallel_over(parallel_over, "control_sim_anneal()")
    }

    if (!is.numeric(radius) | !length(radius) == 2) {
      rlang::abort("Argument 'radius' should be two numeric values.")
    }
    radius <- sort(radius)
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
        save_workflow = save_workflow,
        save_history = save_history,
        event_level = event_level,
        parallel_over = parallel_over
      )

    class(res) <- "control_sim_anneal"
    res
  }

#' @export
print.control_sim_anneal <- function(x, ...) {
  cat("Simulated annealing control object\n")
  invisible(x)
}


val_parallel_over <- function(parallel_over, where) {
  val_class_and_single(parallel_over, "character", where)
  rlang::arg_match0(parallel_over, c("resamples", "everything"), "parallel_over")
  invisible(NULL)
}
