
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finetune <a href='https://finetune.tidymodels.org'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/finetune/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/finetune/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/finetune/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/finetune?branch=main)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/finetune/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/finetune)
<!-- badges: end -->

`finetune` contains some extra functions for model tuning that extend
what is currently in the `tune` package. You can install the CRAN
version of the package with the following code:

``` r
install.packages("finetune")
```

To install the development version of the package, run:

``` r
# install.packages("pak")
pak::pak("tidymodels/finetune")
```

There are two main sets of tools in the package: *simulated annealing*
and *racing*.

Tuning via *simulated annealing* optimization is an iterative search
tool for finding good values:

``` r
library(tidymodels)
library(finetune)

# Syntax very similar to `tune_grid()` or `tune_bayes()`: 

## -----------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(1)
rs <- bootstraps(two_class_dat, times = 10) # more resamples usually needed

# Optimize a regularized discriminant analysis model
library(discrim)
rda_spec <-
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) |>
  set_engine("klaR")

## -----------------------------------------------------------------------------

set.seed(2)
sa_res <- 
  rda_spec |> 
  tune_sim_anneal(Class ~ ., resamples = rs, iter = 20, initial = 4)
#> Optimizing roc_auc
#> Initial best: 0.88281
#> 1 ◯ accept suboptimal  roc_auc=0.87797 (+/-0.004187)
#> 2 + better suboptimal  roc_auc=0.87811 (+/-0.004082)
#> 3 ◯ accept suboptimal  roc_auc=0.86938 (+/-0.005172)
#> 4 ◯ accept suboptimal  roc_auc=0.85949 (+/-0.006067)
#> 5 ◯ accept suboptimal  roc_auc=0.84727 (+/-0.006757)
#> 6 ◯ accept suboptimal  roc_auc=0.84441 (+/-0.006885)
#> 7 + better suboptimal  roc_auc=0.84715 (+/-0.006718)
#> 8 ✖ restart from best  roc_auc=0.85368 (+/-0.006366)
#> 9 ◯ accept suboptimal  roc_auc=0.88032 (+/-0.00397)
#> 10 ◯ accept suboptimal  roc_auc=0.87373 (+/-0.004807)
#> 11 + better suboptimal  roc_auc=0.87691 (+/-0.004533)
#> 12 ◯ accept suboptimal  roc_auc=0.86149 (+/-0.005802)
#> 13 + better suboptimal  roc_auc=0.86304 (+/-0.005684)
#> 14 + better suboptimal  roc_auc=0.87479 (+/-0.004721)
#> 15 ◯ accept suboptimal  roc_auc=0.86637 (+/-0.005425)
#> 16 ✖ restart from best  roc_auc=0.85841 (+/-0.006111)
#> 17 ◯ accept suboptimal  roc_auc=0.87862 (+/-0.004139)
#> 18 + better suboptimal  roc_auc=0.88011 (+/-0.004023)
#> 19 ◯ accept suboptimal  roc_auc=0.87175 (+/-0.004952)
#> 20 ─ discard suboptimal roc_auc=0.86236 (+/-0.005762)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 × 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.667        0      roc_auc binary     0.883    10 0.00360 initial_…
#> 2           0.793        0.0344 roc_auc binary     0.880    10 0.00397 Iter9    
#> # ℹ 1 more variable: .iter <int>
```

The second set of methods are for *racing*. We start off by doing a
small set of resamples for all of the grid points, then statistically
testing to see which ones should be dropped or investigated more. The
two methods here are based on those should in [Kuhn
(2014)](https://arxiv.org/abs/1405.6974).

For example, using an ANOVA-type analysis to filter out parameter
combinations:

``` r
set.seed(3)
grid <-
  rda_spec |>
  extract_parameter_set_dials() |>
  grid_max_entropy(size = 20)
#> Warning: `grid_max_entropy()` was deprecated in dials 1.3.0.
#> ℹ Please use `grid_space_filling()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.

ctrl <- control_race(verbose_elim = TRUE)

set.seed(4)
grid_anova <- 
  rda_spec |> 
  tune_race_anova(Class ~ ., resamples = rs, grid = grid, control = ctrl)
#> ℹ Evaluating against the initial 3 burn-in resamples.
#> ℹ Racing will maximize the roc_auc metric.
#> ℹ Resamples are analyzed in a random order.
#> ℹ Bootstrap10: 14 eliminated; 6 candidates remain.
#> 
#> ℹ Bootstrap04: 2 eliminated; 4 candidates remain.
#> 
#> ℹ Bootstrap03: All but one parameter combination were eliminated.

show_best(grid_anova, metric = "roc_auc", n = 2)
#> # A tibble: 1 × 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.831        0.0207 roc_auc binary     0.881    10 0.00386 Preproce…
```

`tune_race_win_loss()` can also be used. It treats the tuning parameters
as sports teams in a tournament and computed win/loss statistics.

``` r
set.seed(4)
grid_win_loss<- 
  rda_spec |> 
  tune_race_win_loss(Class ~ ., resamples = rs, grid = grid, control = ctrl)
#> ℹ Racing will maximize the roc_auc metric.
#> ℹ Resamples are analyzed in a random order.
#> ℹ Bootstrap10: 3 eliminated; 17 candidates remain.
#> 
#> ℹ Bootstrap04: 2 eliminated; 15 candidates remain.
#> 
#> ℹ Bootstrap03: 2 eliminated; 13 candidates remain.
#> 
#> ℹ Bootstrap01: 1 eliminated; 12 candidates remain.
#> 
#> ℹ Bootstrap07: 1 eliminated; 11 candidates remain.
#> 
#> ℹ Bootstrap05: 1 eliminated; 10 candidates remain.
#> 
#> ℹ Bootstrap08: 1 eliminated; 9 candidates remain.

show_best(grid_win_loss, metric = "roc_auc", n = 2)
#> # A tibble: 2 × 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.831        0.0207 roc_auc binary     0.881    10 0.00386 Preproce…
#> 2           0.119        0.0470 roc_auc binary     0.879    10 0.00387 Preproce…
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/usemodels/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
