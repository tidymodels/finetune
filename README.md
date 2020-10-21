
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
#> ── Attaching packages ────────────────────────────────────── tidymodels 0.1.1 ──
#> ✓ broom     0.7.0          ✓ recipes   0.1.14    
#> ✓ dials     0.0.9.9000     ✓ rsample   0.0.8.9000
#> ✓ dplyr     1.0.2          ✓ tibble    3.0.4     
#> ✓ ggplot2   3.3.2          ✓ tidyr     1.1.2     
#> ✓ infer     0.5.2          ✓ tune      0.1.1.9001
#> ✓ modeldata 0.0.2          ✓ workflows 0.2.1.9000
#> ✓ parsnip   0.1.3.9000     ✓ yardstick 0.0.7     
#> ✓ purrr     0.3.4
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
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
#>  1 ◯ accept suboptimal  roc_auc=0.88454  (+/-0.005114)
#>  2 + better suboptimal  roc_auc=0.88649  (+/-0.005143)
#>  3 ◯ accept suboptimal  roc_auc=0.88499  (+/-0.005126)
#>  4 ◯ accept suboptimal  roc_auc=0.8833   (+/-0.005075)
#>  5 ◯ accept suboptimal  roc_auc=0.87731  (+/-0.004954)
#>  6 + better suboptimal  roc_auc=0.88078  (+/-0.005048)
#>  7 ◯ accept suboptimal  roc_auc=0.88028  (+/-0.005061)
#>  8 x restart from best  roc_auc=0.88474  (+/-0.005052)
#>  9 ◯ accept suboptimal  roc_auc=0.88113  (+/-0.005018)
#> 10 ◯ accept suboptimal  roc_auc=0.87766  (+/-0.004922)
#> 11 + better suboptimal  roc_auc=0.88147  (+/-0.005013)
#> 12 + better suboptimal  roc_auc=0.88781  (+/-0.005152)
#> 13 ─ discard suboptimal roc_auc=0.88125  (+/-0.005021)
#> 14 ─ discard suboptimal roc_auc=0.88363  (+/-0.005089)
#> 15 ─ discard suboptimal roc_auc=0.88158  (+/-0.005025)
#> 16 x restart from best  roc_auc=0.88105  (+/-0.005013)
#> 17 ◯ accept suboptimal  roc_auc=0.88183  (+/-0.005037)
#> 18 + better suboptimal  roc_auc=0.88674  (+/-0.005158)
#> 19 ─ discard suboptimal roc_auc=0.87913  (+/-0.005037)
#> 20 ◯ accept suboptimal  roc_auc=0.88358  (+/-0.005096)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 x 9
#>   frac_common_cov frac_identity .metric .estimator  mean     n std_err .config
#>             <dbl>         <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
#> 1          0.0345      0.000182 roc_auc binary     0.888    10 0.00516 Prepro…
#> 2          0.0572      0.00318  roc_auc binary     0.888    10 0.00515 Prepro…
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
#> ℹ Bootstrap05: 18 eliminated;  2 candidates remain.
#> ℹ Bootstrap07:  0 eliminated;  2 candidates remain.
#> ℹ Bootstrap10:  0 eliminated;  2 candidates remain.
#> ℹ Bootstrap01:  0 eliminated;  2 candidates remain.
#> ℹ Bootstrap08:  0 eliminated;  2 candidates remain.
#> ℹ Bootstrap03:  0 eliminated;  2 candidates remain.
#> ℹ Bootstrap09:  0 eliminated;  2 candidates remain.
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
