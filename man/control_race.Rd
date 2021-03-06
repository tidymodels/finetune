% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/control_race.R
\name{control_race}
\alias{control_race}
\title{Control aspects of the grid search racing process}
\usage{
control_race(
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
  parallel_over = "everything"
)
}
\arguments{
\item{verbose}{A logical for logging results as they are generated. Despite
this argument, warnings and errors are always shown. If using a dark IDE
theme, some logging messages might be hard to see. If this is the case,
try setting the \code{tidymodels.dark} option with
\code{options(tidymodels.dark = TRUE)} to print lighter colors.}

\item{verbose_elim}{A logical for whether logging of the elimination of
tuning parameter combinations should occur.}

\item{allow_par}{A logical to allow parallel processing (if a parallel
backend is registered).}

\item{extract}{An optional function with at least one argument (or \code{NULL})
that can be used to retain arbitrary objects from the model fit object,
recipe, or other elements of the workflow.}

\item{save_pred}{A logical for whether the out-of-sample predictions should
be saved for each model \emph{evaluated}.}

\item{burn_in}{An integer for how many resamples should be completed for all
grid combinations before parameter filtering begins.}

\item{num_ties}{An integer for when tie-breaking should occur. If there are
two final parameter combinations being evaluated, \code{num_ties} specified how
many more resampling iterations should be evaluated. After \code{num_ties} more
iterations, the parameter combination with the current best results is
retained.}

\item{alpha}{The alpha level for a one-sided confidence interval for each
parameter combination.}

\item{randomize}{Should the resamples be evaluated in a random order?  By
default, the resamples are evaluated in a random order so the random number
seed should be control prior to calling this method (to be reproducible).
For repeated cross-validation the randomization occurs within each repeat.}

\item{pkgs}{An optional character string of R package names that should be
loaded (by namespace) during parallel processing.}

\item{save_workflow}{A logical for whether the workflow should be appended
to the output as an attribute.}

\item{event_level}{A single string containing either "first" or "second".
This argument is passed on to \code{yardstick} metric functions when any type of
class prediction is made, and specifies which level of the outcome is
considered the "event".}

\item{parallel_over}{A single string containing either \code{"resamples"} or
\code{"everything"} describing how to use parallel processing. Alternatively,
\code{NULL} is allowed, which chooses between \code{"resamples"} and \code{"everything"}
automatically.

If \code{"resamples"}, then tuning will be performed in parallel over resamples
alone. Within each resample, the preprocessor (i.e. recipe or formula) is
processed once, and is then reused across all models that need to be fit.

If \code{"everything"}, then tuning will be performed in parallel at two levels.
An outer parallel loop will iterate over resamples. Additionally, an
inner parallel loop will iterate over all unique combinations of
preprocessor and model tuning parameters for that specific resample. This
will result in the preprocessor being re-processed multiple times, but
can be faster if that processing is extremely fast.

If \code{NULL}, chooses \code{"resamples"} if there are more than one resample,
otherwise chooses \code{"everything"} to attempt to maximize core utilization.}
}
\description{
Control aspects of the grid search racing process
}
\examples{
control_race()
}
