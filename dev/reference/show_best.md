# Investigate best tuning parameters

[`tune::show_best()`](https://tune.tidymodels.org/reference/show_best.html)
displays the top sub-models and their performance estimates.

## Usage

``` r
# S3 method for class 'tune_race'
show_best(
  x,
  ...,
  metric = NULL,
  eval_time = NULL,
  n = 5,
  call = rlang::current_env()
)
```

## Arguments

- x:

  The results of
  [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
  or
  [`tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html).

- ...:

  For
  [`select_by_one_std_err()`](https://tune.tidymodels.org/reference/show_best.html)
  and
  [`select_by_pct_loss()`](https://tune.tidymodels.org/reference/show_best.html),
  this argument is passed directly to
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  so that the user can sort the models from *most simple to most
  complex*. That is, for a parameter `p`, pass the unquoted expression
  `p` if smaller values of `p` indicate a simpler model, or `desc(p)` if
  larger values indicate a simpler model. At least one term is required
  for these two functions. See the examples below.

- metric:

  A character value for the metric that will be used to sort the models.
  (See <https://yardstick.tidymodels.org/articles/metric-types.html> for
  more details). Not required if a single metric exists in `x`. If there
  are multiple metric and none are given, the first in the metric set is
  used (and a warning is issued).

- eval_time:

  A single numeric time point where dynamic event time metrics should be
  chosen (e.g., the time-dependent ROC curve, etc). The values should be
  consistent with the values used to create `x`. The `NULL` default will
  automatically use the first evaluation time used by `x`.

- n:

  An integer for the maximum number of top results/rows to return.

- call:

  The call to be shown in errors and warnings.

## Details

For racing results (from the finetune package), it is best to only
report configurations that finished the race (i.e., were completely
resampled). Comparing performance metrics for configurations averaged
with different resamples is likely to lead to inappropriate results.
