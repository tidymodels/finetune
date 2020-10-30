
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
library(finetune)

# Syntax very similar to `tune_grid()` or `tune_Bayes()`: 

## -----------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(6376)
rs <- bootstraps(two_class_dat, times = 10) # more resamples usually needed

# optimize an regularized discriminant analysis model
library(discrim)
rda_spec <-
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
  set_engine("klaR")

## -----------------------------------------------------------------------------

set.seed(8300)
sa_res <- 
  rda_spec %>% 
  tune_sim_anneal(Class ~ ., resamples = rs, iter = 20, initial = 4)
#> 
#> >  Generating a set of 4 initial parameter results
#> Loading required package: MASS
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> ✓ Initialization complete
#> 
#> Optimizing roc_auc
#> Initial best: 0.88802
#>  1 ◯ accept suboptimal  roc_auc=0.8833   (+/-0.005067)
#>  2 ◯ accept suboptimal  roc_auc=0.88092  (+/-0.005024)
#>  3 + better suboptimal  roc_auc=0.88755  (+/-0.00513)
#>  4 ◯ accept suboptimal  roc_auc=0.88317  (+/-0.005064)
#>  5 ◯ accept suboptimal  roc_auc=0.87502  (+/-0.005022)
#>  6 ◯ accept suboptimal  roc_auc=0.86507  (+/-0.005064)
#>  7 + better suboptimal  roc_auc=0.87192  (+/-0.005008)
#>  8 x restart from best  roc_auc=0.88028  (+/-0.005088)
#>  9 ◯ accept suboptimal  roc_auc=0.88738  (+/-0.005146)
#> 10 ◯ accept suboptimal  roc_auc=0.88141  (+/-0.005035)
#> 11 ─ discard suboptimal roc_auc=0.87528  (+/-0.004934)
#> 12 ◯ accept suboptimal  roc_auc=0.878    (+/-0.00495)
#> 13 ◯ accept suboptimal  roc_auc=0.86893  (+/-0.005054)
#> 14 + better suboptimal  roc_auc=0.87481  (+/-0.004947)
#> 15 ─ discard suboptimal roc_auc=0.86514  (+/-0.005068)
#> 16 x restart from best  roc_auc=0.86691  (+/-0.005042)
#> 17 ◯ accept suboptimal  roc_auc=0.88099  (+/-0.005)
#> 18 + better suboptimal  roc_auc=0.88763  (+/-0.005165)
#> 19 ◯ accept suboptimal  roc_auc=0.8815   (+/-0.005016)
#> 20 + better suboptimal  roc_auc=0.8849   (+/-0.005134)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1          0.0345      0.000182 roc_auc binary     0.888    10 0.00516 Prepro…
#> 2          0.0697      0.00761  roc_auc binary     0.888    10 0.00516 Prepro…
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
show_best(grid_anova, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 8
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config  
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>    
#> 1          0.0164        0.0618 roc_auc binary     0.884    10 0.00511 Preproce…
#> 2          0.327         0.0662 roc_auc binary     0.884    10 0.00503 Preproce…
```

`tune_race_win_loss()` can also be used.

## Code of Conduct

Please note that the finetune project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
