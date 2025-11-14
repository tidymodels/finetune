# Control aspects of the grid search racing process

Control aspects of the grid search racing process

## Usage

``` r
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
  parallel_over = "everything",
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

- verbose_elim:

  A logical for whether logging of the elimination of tuning parameter
  combinations should occur.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

- extract:

  An optional function with at least one argument (or `NULL`) that can
  be used to retain arbitrary objects from the model fit object, recipe,
  or other elements of the workflow.

- save_pred:

  A logical for whether the out-of-sample predictions should be saved
  for each model *evaluated*.

- burn_in:

  An integer for how many resamples should be completed for all grid
  combinations before parameter filtering begins.

- num_ties:

  An integer for when tie-breaking should occur. If there are two final
  parameter combinations being evaluated, `num_ties` specified how many
  more resampling iterations should be evaluated. After `num_ties` more
  iterations, the parameter combination with the current best results is
  retained.

- alpha:

  The alpha level for a one-sided confidence interval for each parameter
  combination.

- randomize:

  Should the resamples be evaluated in a random order? By default, the
  resamples are evaluated in a random order so the random number seed
  should be control prior to calling this method (to be reproducible).
  For repeated cross-validation the randomization occurs within each
  repeat.

- pkgs:

  An optional character string of R package names that should be loaded
  (by namespace) during parallel processing.

- save_workflow:

  A logical for whether the workflow should be appended to the output as
  an attribute.

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

An object of class `control_race` that echos the argument values.

## Examples

``` r
control_race()
#> Racing method control object
```
