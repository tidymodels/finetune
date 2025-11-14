# Obtain and format results produced by racing functions

Obtain and format results produced by racing functions

## Usage

``` r
# S3 method for class 'tune_race'
collect_predictions(
  x,
  ...,
  summarize = FALSE,
  parameters = NULL,
  all_configs = FALSE
)

# S3 method for class 'tune_race'
collect_metrics(
  x,
  ...,
  summarize = TRUE,
  type = c("long", "wide"),
  all_configs = FALSE
)
```

## Arguments

- x:

  The results of
  [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
  [`tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html),
  [`fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html),
  or
  [`last_fit()`](https://tune.tidymodels.org/reference/last_fit.html).
  For
  [`collect_predictions()`](https://tune.tidymodels.org/reference/collect_predictions.html),
  the control option `save_pred = TRUE` should have been used.

- ...:

  Not currently used.

- summarize:

  A logical; should metrics be summarized over resamples (`TRUE`) or
  return the values for each individual resample. Note that, if `x` is
  created by
  [`last_fit()`](https://tune.tidymodels.org/reference/last_fit.html),
  `summarize` has no effect. For the other object types, the method of
  summarizing predictions is detailed below.

- parameters:

  An optional tibble of tuning parameter values that can be used to
  filter the predicted values before processing. This tibble should only
  have columns for each tuning parameter identifier (e.g. `"my_param"`
  if `tune("my_param")` was used).

- all_configs:

  A logical: should we return the complete set of model configurations
  or just those that made it to the end of the race (the default).

- type:

  One of `"long"` (the default) or `"wide"`. When `type = "long"`,
  output has columns `.metric` and one of `.estimate` or `mean`.
  `.estimate`/`mean` gives the values for the `.metric`. When
  `type = "wide"`, each metric has its own column and the `n` and
  `std_err` columns are removed, if they exist.

## Value

A tibble. The column names depend on the results and the mode of the
model.

## Details

For
[`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
and
[`tune::collect_predictions()`](https://tune.tidymodels.org/reference/collect_predictions.html),
when unsummarized, there are columns for each tuning parameter (using
the `id` from
[`hardhat::tune()`](https://hardhat.tidymodels.org/reference/tune.html),
if any).
[`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
also has columns `.metric`, and `.estimator`. When the results are
summarized, there are columns for `mean`, `n`, and `std_err`. When not
summarized, the additional columns for the resampling identifier(s) and
`.estimate`.

For
[`tune::collect_predictions()`](https://tune.tidymodels.org/reference/collect_predictions.html),
there are additional columns for the resampling identifier(s), columns
for the predicted values (e.g., `.pred`, `.pred_class`, etc.), and a
column for the outcome(s) using the original column name(s) in the data.

[`tune::collect_predictions()`](https://tune.tidymodels.org/reference/collect_predictions.html)
can summarize the various results over replicate out-of-sample
predictions. For example, when using the bootstrap, each row in the
original training set has multiple holdout predictions (across
assessment sets). To convert these results to a format where every
training set same has a single predicted value, the results are averaged
over replicate predictions.

For regression cases, the numeric predictions are simply averaged. For
classification models, the problem is more complex. When class
probabilities are used, these are averaged and then re-normalized to
make sure that they add to one. If hard class predictions also exist in
the data, then these are determined from the summarized probability
estimates (so that they match). If only hard class predictions are in
the results, then the mode is used to summarize.

For racing results, it is best to only collect model configurations that
finished the race (i.e., were completely resampled). Comparing
performance metrics for configurations averaged with different resamples
is likely to lead to inappropriate results.
