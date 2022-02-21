
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finetune <a href='https://finetune.tidymodels.org'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tidymodels/finetune/branch/main/graph/badge.svg)](https://codecov.io/gh/tidymodels/finetune?branch=main)
[![R-CMD-check](https://github.com/tidymodels/finetune/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/finetune/actions)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
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
#>  1 ◯ accept suboptimal  roc_auc=0.86254  (+/-0.005754)
#>  2 ◯ accept suboptimal  roc_auc=0.86119  (+/-0.00582)
#>  3 ─ discard suboptimal roc_auc=0.85296  (+/-0.006486)
#>  4 ◯ accept suboptimal  roc_auc=0.86043  (+/-0.005998)
#>  5 ♥ new best           roc_auc=0.87226  (+/-0.004803)
#>  6 ♥ new best           roc_auc=0.88229  (+/-0.003629)
#>  7 ◯ accept suboptimal  roc_auc=0.87871  (+/-0.004048)
#>  8 ◯ accept suboptimal  roc_auc=0.87115  (+/-0.005025)
#>  9 ◯ accept suboptimal  roc_auc=0.86354  (+/-0.005684)
#> 10 + better suboptimal  roc_auc=0.86984  (+/-0.005159)
#> 11 + better suboptimal  roc_auc=0.88031  (+/-0.003984)
#> 12 ◯ accept suboptimal  roc_auc=0.87273  (+/-0.004879)
#> 13 ◯ accept suboptimal  roc_auc=0.86086  (+/-0.005867)
#> 14 x restart from best  roc_auc=0.84515  (+/-0.006771)
#> 15 ◯ accept suboptimal  roc_auc=0.88183  (+/-0.003638)
#> 16 ♥ new best           roc_auc=0.88233  (+/-0.003587)
#> 17 ◯ accept suboptimal  roc_auc=0.87944  (+/-0.003864)
#> 18 ─ discard suboptimal roc_auc=0.86931  (+/-0.00514)
#> 19 + better suboptimal  roc_auc=0.88197  (+/-0.003607)
#> 20 ─ discard suboptimal roc_auc=0.87388  (+/-0.004439)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1           0.287       0.00861 roc_auc binary     0.882    10 0.00359 Iter16 
#> 2           0.440       0.0109  roc_auc binary     0.882    10 0.00363 Iter6  
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
#> ℹ Bootstrap10: 18 eliminated;  2 candidates remain.
#> ℹ Bootstrap07:  0 eliminated;  2 candidates remain.
#> ℹ Bootstrap05: All but one parameter combination were eliminated.

show_best(grid_anova, metric = "roc_auc", n = 2)
#> # A tibble: 1 x 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.132        0.0136 roc_auc binary     0.882    10 0.00363 Preproce…
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
#> ℹ Bootstrap10:  4 eliminated; 16 candidates remain.
#> ℹ Bootstrap07:  2 eliminated; 14 candidates remain.
#> ℹ Bootstrap05:  1 eliminated; 13 candidates remain.
#> ℹ Bootstrap03:  2 eliminated; 11 candidates remain.
#> ℹ Bootstrap06:  1 eliminated; 10 candidates remain.
#> ℹ Bootstrap09:  1 eliminated;  9 candidates remain.
#> ℹ Bootstrap01:  1 eliminated;  8 candidates remain.

show_best(grid_win_loss, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1           0.132        0.0136 roc_auc binary     0.882    10 0.00363 Preproce…
#> 2           0.975        0.0491 roc_auc binary     0.879    10 0.00422 Preproce…
```

## Code of Conduct

Please note that the finetune project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
