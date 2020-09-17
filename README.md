
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finetune

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tidymodels/finetune/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/finetune?branch=master)
[![R build
status](https://github.com/tidymodels/finetune/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/finetune/actions)
<!-- badges: end -->

`finetune` contains some extra functions for model tuning that extend
what is currently in the `tune` package.

Very rough version of the package right now but it works fairly well.
There are two main sets of tools.

Tuning via *simulated annealing* optimization is another iterative
search tool for finding good values:

``` r
library(tidymodels)
#> ── Attaching packages ─────────────────────────────────────── tidymodels 0.1.1 ──
#> ✓ broom     0.7.0          ✓ recipes   0.1.13    
#> ✓ dials     0.0.8.9001     ✓ rsample   0.0.7.9000
#> ✓ dplyr     1.0.2          ✓ tibble    3.0.3     
#> ✓ ggplot2   3.3.2          ✓ tidyr     1.1.2     
#> ✓ infer     0.5.2          ✓ tune      0.1.1.9000
#> ✓ modeldata 0.0.2          ✓ workflows 0.1.3.9000
#> ✓ parsnip   0.1.3          ✓ yardstick 0.0.7     
#> ✓ purrr     0.3.4
#> ── Conflicts ────────────────────────────────────────── tidymodels_conflicts() ──
#> x purrr::discard() masks scales::discard()
#> x dplyr::filter()  masks stats::filter()
#> x dplyr::lag()     masks stats::lag()
#> x recipes::step()  masks stats::step()
library(finetune)

# Syntax very similar to `tune_grid()` or `tune_Bayes()`: 

## -----------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(6376)
rs <- bootstraps(two_class_dat, times = 10) # more resamples usually needed

# optimize an regularized discriminant analysis model
library(discrim)
#> 
#> Attaching package: 'discrim'
#> The following object is masked from 'package:dials':
#> 
#>     smoothness
rda_spec <-
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
  set_engine("klaR")

## -----------------------------------------------------------------------------

set.seed(8300)
sa_res <- rda_spec %>% tune_sim_anneal(Class ~ ., resamples = rs, iter = 20)
#> Loading required package: MASS
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Initial best: 0.85545
#>  1 ♥  roc_auc: 0.86490    new best
#>  2 ◯  roc_auc: 0.85687    accept suboptimal
#>  3 ◯  roc_auc: 0.85310    accept suboptimal
#>  4 +  roc_auc: 0.86269    better suboptimal
#>  5 ◯  roc_auc: 0.85284    accept suboptimal
#>  6 ◯  roc_auc: 0.84860    accept suboptimal
#>  7 +  roc_auc: 0.85752    better suboptimal
#>  8 ◯  roc_auc: 0.84824    accept suboptimal
#>  9 x  roc_auc: 0.84314    restart from best
#> 10 ♥  roc_auc: 0.87509    new best
#> 11 ♥  roc_auc: 0.87587    new best
#> 12 ♥  roc_auc: 0.88370    new best
#> 13 ◯  roc_auc: 0.88092    accept suboptimal
#> 14 ♥  roc_auc: 0.88706    new best
#> 15 ◯  roc_auc: 0.88445    accept suboptimal
#> 16 ♥  roc_auc: 0.88735    new best
#> 17 ◯  roc_auc: 0.88048    accept suboptimal
#> 18 ◯  roc_auc: 0.87469    accept suboptimal
#> 19 +  roc_auc: 0.87631    better suboptimal
#> 20 ◯  roc_auc: 0.86665    accept suboptimal
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1           0.235        0.0134 roc_auc binary     0.887    10 0.00514 Model1 
#> 2           0.232        0.0186 roc_auc binary     0.887    10 0.00514 Model1 
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
set.seed(511)
grid <-
  rda_spec %>%
  parameters() %>%
  grid_max_entropy(size = 20)

set.seed(11)
grid_anova <- rda_spec %>% tune_race_anova(Class ~ ., resamples = rs, grid = grid)
#> ℹ Racing will maximize the roc_auc metric.
#> ℹ Resamples are analyzed in a random order.
#> ℹ Bootstrap05: 4 of 20 candidate sub-models remain.
#> ℹ Bootstrap07: 4 of 20 candidate sub-models remain.
#> ℹ Bootstrap10: 4 of 20 candidate sub-models remain.
#> ℹ Bootstrap01: 4 of 20 candidate sub-models remain.
#> ℹ Bootstrap08: 4 of 20 candidate sub-models remain.
#> ℹ Bootstrap03: 4 of 20 candidate sub-models remain.
#> ℹ Bootstrap09: 4 of 20 candidate sub-models remain.
show_best(grid_anova, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1          0.0164        0.0618 roc_auc binary     0.884    10 0.00511 Model10
#> 2          0.327         0.0662 roc_auc binary     0.884    10 0.00503 Model16
```

`tune_race_win_loss()` can also be used.

## Code of Conduct

Please note that the finetune project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
