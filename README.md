
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

# Optimize a regularized discriminant analysis model
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
#>  1 ♥ new best           roc_auc=0.87739  (+/-0.004113)
#>  2 ◯ accept suboptimal  roc_auc=0.87315  (+/-0.004446)
#>  3 ◯ accept suboptimal  roc_auc=0.86729  (+/-0.005237)
#>  4 + better suboptimal  roc_auc=0.86747  (+/-0.005196)
#>  5 + better suboptimal  roc_auc=0.87173  (+/-0.004765)
#>  6 + better suboptimal  roc_auc=0.87337  (+/-0.004425)
#>  7 ◯ accept suboptimal  roc_auc=0.87085  (+/-0.004774)
#>  8 ◯ accept suboptimal  roc_auc=0.85972  (+/-0.006017)
#>  9 x restart from best  roc_auc=0.85759  (+/-0.00626)
#> 10 ♥ new best           roc_auc=0.87757  (+/-0.004086)
#> 11 ◯ accept suboptimal  roc_auc=0.8704   (+/-0.005025)
#> 12 ─ discard suboptimal roc_auc=0.85845  (+/-0.006172)
#> 13 + better suboptimal  roc_auc=0.87247  (+/-0.004713)
#> 14 ─ discard suboptimal roc_auc=0.86196  (+/-0.005814)
#> 15 ♥ new best           roc_auc=0.8788   (+/-0.003924)
#> 16 ─ discard suboptimal roc_auc=0.87121  (+/-0.004967)
#> 17 ♥ new best           roc_auc=0.88255  (+/-0.003558)
#> 18 ◯ accept suboptimal  roc_auc=0.88233  (+/-0.003613)
#> 19 ◯ accept suboptimal  roc_auc=0.8761   (+/-0.004405)
#> 20 + better suboptimal  roc_auc=0.88149  (+/-0.003718)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1           0.237       0.00661 roc_auc binary     0.883    10 0.00356 Iter17 
#> 2           0.333       0.00903 roc_auc binary     0.882    10 0.00361 Iter18 
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

`tune_race_win_loss()` can also be used. It treats the tuning parameters
as sports teams in a tournament and computed win/loss statistics.

``` r
set.seed(4)
grid_win_loss<- 
  rda_spec %>% 
  tune_race_win_loss(Class ~ ., resamples = rs, grid = grid, control = ctrl)
#> ℹ Racing will maximize the roc_auc metric.
#> ℹ Resamples are analyzed in a random order.
#> ℹ Bootstrap10:  3 eliminated; 17 candidates remain.
#> ℹ Bootstrap04:  2 eliminated; 15 candidates remain.
#> ℹ Bootstrap03:  2 eliminated; 13 candidates remain.
#> ℹ Bootstrap01:  1 eliminated; 12 candidates remain.
#> ℹ Bootstrap07:  1 eliminated; 11 candidates remain.
#> ℹ Bootstrap05:  1 eliminated; 10 candidates remain.
#> ℹ Bootstrap08:  1 eliminated;  9 candidates remain.

show_best(grid_win_loss, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.831        0.0207 roc_auc binary     0.881    10 0.00386 Preproce…
#> 2           0.119        0.0470 roc_auc binary     0.879    10 0.00387 Preproce…
```

## Code of Conduct

Please note that the finetune project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
