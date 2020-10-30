
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finetune

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tidymodels/finetune/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/finetune?branch=master)
[![R build
status](https://github.com/tidymodels/finetune/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/finetune/actions)
[![R-CMD-check](https://github.com/tidymodels/finetune/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/finetune/actions)
<!-- badges: end -->

`finetune` contains some extra functions for model tuning that extend
what is currently in the `tune` package.

Very rough version of the package right now but it works fairly well.
There are two main sets of tools.

Tuning via *simulated annealing* optimization is another iterative
search tool for finding good values:

``` r
library(tidymodels)
library(finetune)

# Syntax very similar to `tune_grid()` or `tune_Bayes()`: 

## -----------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(1)
rs <- bootstraps(two_class_dat, times = 10) # more resamples usually needed

# optimize an regularized discriminant analysis model
library(discrim)
rda_spec <-
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
  set_engine("klaR")

## -----------------------------------------------------------------------------

ctrl <- control_sim_anneal(verbose = TRUE)

set.seed(2)
sa_res <- 
  rda_spec %>% 
  tune_sim_anneal(Class ~ ., resamples = rs, iter = 20, initial = 4, control = ctrl)
#> 
#> >  Generating a set of 4 initial parameter results
#> ✓ Initialization complete
#> 
#> Optimizing roc_auc
#> Initial best: 0.86480
#>  1 ◯ accept suboptimal  roc_auc=0.85625  (+/-0.006354)
#>  2 + better suboptimal  roc_auc=0.85896  (+/-0.006098)
#>  3 ♥ new best           roc_auc=0.86716  (+/-0.005235)
#>  4 ♥ new best           roc_auc=0.8739   (+/-0.004409)
#>  5 ♥ new best           roc_auc=0.88295  (+/-0.00353)
#>  6 ◯ accept suboptimal  roc_auc=0.87583  (+/-0.00413)
#>  7 ◯ accept suboptimal  roc_auc=0.86858  (+/-0.005111)
#>  8 + better suboptimal  roc_auc=0.86988  (+/-0.005051)
#>  9 + better suboptimal  roc_auc=0.87918  (+/-0.003877)
#> 10 ◯ accept suboptimal  roc_auc=0.87349  (+/-0.004475)
#> 11 ◯ accept suboptimal  roc_auc=0.86836  (+/-0.005163)
#> 12 ◯ accept suboptimal  roc_auc=0.85878  (+/-0.006142)
#> 13 x restart from best  roc_auc=0.86732  (+/-0.005274)
#> 14 ◯ accept suboptimal  roc_auc=0.88057  (+/-0.003712)
#> 15 ◯ accept suboptimal  roc_auc=0.87135  (+/-0.004772)
#> 16 ◯ accept suboptimal  roc_auc=0.86193  (+/-0.005737)
#> 17 ◯ accept suboptimal  roc_auc=0.85101  (+/-0.006655)
#> 18 ◯ accept suboptimal  roc_auc=0.84378  (+/-0.007069)
#> 19 + better suboptimal  roc_auc=0.85134  (+/-0.006625)
#> 20 ◯ accept suboptimal  roc_auc=0.84704  (+/-0.006863)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1          0.132       0.000739 roc_auc binary     0.883    10 0.00353 Prepro…
#> 2          0.0356      0.0286   roc_auc binary     0.881    10 0.00371 Prepro…
#> # … with 1 more variable: .iter <int>
```

The second set of methods are for “racing”. We start off by doing a
small set of resamples for all of the grid points, then statistically
testing to see which ones should be dropped or investigated more. The
two methods here are based on those should in [Kuhn
(2014)](https://arxiv.org/abs/1405.6974).

For example, using an ANOVA-type analysis to filter out parameter
combinations:

``` r
set.seed(3)
grid <-
  rda_spec %>%
  parameters() %>%
  grid_max_entropy(size = 20)

ctrl <- control_race(verbose_elim = TRUE)

set.seed(4)
grid_anova <- 
  rda_spec %>% 
  tune_race_anova(Class ~ ., resamples = rs, grid = grid, control = ctrl)
#> ℹ Racing will maximize the roc_auc metric.
#> ℹ Resamples are analyzed in a random order.
#> ℹ Bootstrap10: 14 eliminated;  6 candidates remain.
#> ℹ Bootstrap04:  2 eliminated;  4 candidates remain.
#> ℹ Bootstrap03: All but one parameter combination were eliminated.

show_best(grid_anova, metric = "roc_auc", n = 2)
#> # A tibble: 1 x 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.831        0.0207 roc_auc binary     0.881    10 0.00386 Preproce…
```

`tune_race_win_loss()` can also be used.

## Code of Conduct

Please note that the finetune project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
