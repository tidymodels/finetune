#' Control aspects of the simulated annealing search process
#' @inheritParams tune::control_grid
#' @param verbose_iter A logical for logging results of the search
#'   process. Defaults to FALSE. If using a dark IDE theme, some logging
#'   messages might be hard to see; try setting the `tidymodels.dark` option
#'   with `options(tidymodels.dark = TRUE)` to print lighter colors.
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
#' @param save_history A logical to save the iteration details of the search.
#'  These are saved to `tempdir()` named `sa_history.RData`. These results are
#'  deleted when the R session ends. This option is only useful for teaching
#'  purposes.
#' @return An object of class `control_sim_anneal` that echos the argument values.
#' @examples
#' control_sim_anneal()
#' @export
control_sim_anneal <-
  function(
    verbose = FALSE,
    verbose_iter = TRUE,
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
    parallel_over = NULL,
    allow_par = TRUE,
    backend_options = NULL
  ) {
    # Any added arguments should also be added in superset control functions
    # in other package. In other words, if tune_grid adds an option, the same
    # object should be added here (regardless)

    tune::val_class_and_single(verbose, "logical", "control_sim_anneal()")
    tune::val_class_and_single(verbose_iter, "logical", "control_sim_anneal()")
    tune::val_class_and_single(save_pred, "logical", "control_sim_anneal()")
    tune::val_class_and_single(
      no_improve,
      c("numeric", "integer"),
      "control_sim_anneal()"
    )
    tune::val_class_and_single(
      restart,
      c("numeric", "integer"),
      "control_sim_anneal()"
    )
    tune::val_class_and_single(flip, "numeric", "control_sim_anneal()")
    tune::val_class_and_single(cooling_coef, "numeric", "control_sim_anneal()")
    tune::val_class_or_null(extract, "function", "control_sim_anneal()")
    tune::val_class_and_single(
      time_limit,
      c("logical", "numeric"),
      "control_sim_anneal()"
    )
    tune::val_class_or_null(pkgs, "character", "control_sim_anneal()")
    tune::val_class_and_single(save_workflow, "logical", "control_sim_anneal()")
    tune::val_class_and_single(save_history, "logical", "control_sim_anneal()")
    tune::val_class_and_single(allow_par, "logical", "control_sim_anneal()")

    if (!is.null(parallel_over)) {
      val_parallel_over(parallel_over, "control_sim_anneal()")
    }

    if (!is.numeric(radius) | !length(radius) == 2) {
      cli::cli_abort("Argument {.arg radius} should be two numeric values.")
    }
    radius <- sort(radius)
    radius[radius <= 0] <- 0.001
    radius[radius >= 1] <- 0.999

    flip[flip < 0] <- 0
    flip[flip > 1] <- 1
    cooling_coef[cooling_coef <= 0] <- 0.0001

    if (no_improve < 2) {
      cli::cli_abort("{.arg no_improve} should be > 1.")
    }
    if (restart < 2) {
      cli::cli_abort("{.arg restart} should be > 1.")
    }
    if (!is.infinite(restart) && restart > no_improve) {
      cli::cli_alert_warning(
        "Parameter restart is scheduled after {restart} poor iterations but the search will stop after {no_improve}."
      )
    }

    res <-
      list(
        verbose = verbose,
        verbose_iter = verbose_iter,
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
        parallel_over = parallel_over,
        allow_par = allow_par,
        backend_options = backend_options
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
  rlang::arg_match0(
    parallel_over,
    c("resamples", "everything"),
    "parallel_over"
  )
  invisible(NULL)
}
