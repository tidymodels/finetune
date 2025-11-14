# Control aspects of the simulated annealing search process

Control aspects of the simulated annealing search process

## Usage

``` r
control_sim_anneal(
  verbose = FALSE,
  verbose_iter = TRUE,
  no_improve = Inf,
  restart = 8L,
  radius = c(0.05, 0.15),
  flip = 3/4,
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
  backend_options = NULL,
  workflow_size = 100
)
```

## Arguments

- verbose:

  A logical for logging results (other than warnings and errors, which
  are always shown) as they are generated during training in a single R
  process. When using most parallel backends, this argument typically
  will not result in any logging. If using a dark IDE theme, some
  logging messages might be hard to see; try setting the
  `tidymodels.dark` option with `options(tidymodels.dark = TRUE)` to
  print lighter colors.

- verbose_iter:

  A logical for logging results of the search process. Defaults to
  FALSE. If using a dark IDE theme, some logging messages might be hard
  to see; try setting the `tidymodels.dark` option with
  `options(tidymodels.dark = TRUE)` to print lighter colors.

- no_improve:

  The integer cutoff for the number of iterations without better
  results.

- restart:

  The number of iterations with no improvement before new tuning
  parameter candidates are generated from the last, overall best
  conditions.

- radius:

  Two real numbers on `(0, 1)` describing what a value "in the
  neighborhood" of the current result should be. If all numeric
  parameters were scaled to be on the `[0, 1]` scale, these values set
  the min. and max. of a radius of a circle used to generate new numeric
  parameter values.

- flip:

  A real number between `[0, 1]` for the probability of changing any
  non-numeric parameter values at each iteration.

- cooling_coef:

  A real, positive number to influence the cooling schedule. Larger
  values decrease the probability of accepting a sub-optimal parameter
  setting.

- extract:

  An optional function with at least one argument (or `NULL`) that can
  be used to retain arbitrary objects from the model fit object, recipe,
  or other elements of the workflow.

- save_pred:

  A logical for whether the out-of-sample predictions should be saved
  for each model *evaluated*.

- time_limit:

  A number for the minimum number of *minutes* (elapsed) that the
  function should execute. The elapsed time is evaluated at internal
  checkpoints and, if over time, the results at that time are returned
  (with a warning). This means that the `time_limit` is not an exact
  limit, but a minimum time limit.

- pkgs:

  An optional character string of R package names that should be loaded
  (by namespace) during parallel processing.

- save_workflow:

  A logical for whether the workflow should be appended to the output as
  an attribute.

- save_history:

  A logical to save the iteration details of the search. These are saved
  to [`tempdir()`](https://rdrr.io/r/base/tempfile.html) named
  `sa_history.RData`. These results are deleted when the R session ends.
  This option is only useful for teaching purposes.

- event_level:

  A single string containing either `"first"` or `"second"`. This
  argument is passed on to yardstick metric functions when any type of
  class prediction is made, and specifies which level of the outcome is
  considered the "event".

- parallel_over:

  A single string containing either `"resamples"` or `"everything"`
  describing how to use parallel processing. Alternatively, `NULL` is
  allowed, which chooses between `"resamples"` and `"everything"`
  automatically.

  If `"resamples"`, then tuning will be performed in parallel over
  resamples alone. Within each resample, the preprocessor (i.e. recipe
  or formula) is processed once, and is then reused across all models
  that need to be fit.

  If `"everything"`, then tuning will be performed in parallel at two
  levels. An outer parallel loop will iterate over resamples.
  Additionally, an inner parallel loop will iterate over all unique
  combinations of preprocessor and model tuning parameters for that
  specific resample. This will result in the preprocessor being
  re-processed multiple times, but can be faster if that processing is
  extremely fast.

  If `NULL`, chooses `"resamples"` if there are more than one resample,
  otherwise chooses `"everything"` to attempt to maximize core
  utilization.

  Note that switching between `parallel_over` strategies is not
  guaranteed to use the same random number generation schemes. However,
  re-tuning a model using the same `parallel_over` strategy is
  guaranteed to be reproducible between runs.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

- backend_options:

  An object of class `"tune_backend_options"` as created by
  [`tune::new_backend_options()`](https://tune.tidymodels.org/reference/control_grid.html),
  used to pass arguments to specific tuning backend. Defaults to `NULL`
  for default backend options.

- workflow_size:

  A non-negative number (in MB) that is used as a threshold for a
  warning regarding the size of the workflow. Only used when
  `save_workflow = TRUE`.

## Value

An object of class `control_sim_anneal` that echos the argument values.

## Examples

``` r
control_sim_anneal()
#> Simulated annealing control object
```
