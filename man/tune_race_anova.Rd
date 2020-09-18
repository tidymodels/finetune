% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tune_race_anova.R
\name{tune_race_anova}
\alias{tune_race_anova}
\alias{tune_race_anova.model_spec}
\alias{tune_race_anova.workflow}
\title{Model tuning via grid search}
\usage{
tune_race_anova(object, ...)

\method{tune_race_anova}{model_spec}(
  object,
  preprocessor,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  control = control_race()
)

\method{tune_race_anova}{workflow}(
  object,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  control = control_race()
)
}
\arguments{
\item{object}{A \code{parsnip} model specification or a \code{\link[workflows:workflow]{workflows::workflow()}}.}

\item{...}{Not currently used.}

\item{preprocessor}{A traditional model formula or a recipe created using
\code{\link[recipes:recipe]{recipes::recipe()}}.}

\item{resamples}{An \code{rset()} object.}

\item{param_info}{A \code{\link[dials:parameters]{dials::parameters()}} object or \code{NULL}. If none is given,
a parameters set is derived from other arguments. Passing this argument can
be useful when parameter ranges need to be customized.}

\item{grid}{A data frame of tuning combinations or a positive integer. The
data frame should have columns for each parameter being tuned and rows for
tuning parameter candidates. An integer denotes the number of candidate
parameter sets to be created automatically.}

\item{metrics}{A \code{\link[yardstick:metric_set]{yardstick::metric_set()}} or \code{NULL}.}

\item{control}{An object used to modify the tuning process.}
}
\description{
\code{\link[=tune_race_anova]{tune_race_anova()}} computes a set of performance metrics (e.g. accuracy or RMSE)
for a pre-defined set of tuning parameters that correspond to a model or
recipe across one or more resamples of the data.
}