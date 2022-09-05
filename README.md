
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finetune <a href='https://finetune.tidymodels.org'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/finetune/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/finetune/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/finetune/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/finetune?branch=main)
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
#> Optimizing roc_auc
#> Initial best: 0.86480
#>  1 ♥ new best           roc_auc=0.87327  (+/-0.004592)
#>  2 ♥ new best           roc_auc=0.87915  (+/-0.003864)
#>  3 ◯ accept suboptimal  roc_auc=0.87029  (+/-0.004994)
#>  4 + better suboptimal  roc_auc=0.87171  (+/-0.004717)
#>  5 ◯ accept suboptimal  roc_auc=0.86944  (+/-0.005081)
#>  6 ◯ accept suboptimal  roc_auc=0.86812  (+/-0.0052)
#>  7 ♥ new best           roc_auc=0.88172  (+/-0.003647)
#>  8 ◯ accept suboptimal  roc_auc=0.87678  (+/-0.004276)
#>  9 ◯ accept suboptimal  roc_auc=0.8627   (+/-0.005784)
#> 10 + better suboptimal  roc_auc=0.87003  (+/-0.005106)
#> 11 + better suboptimal  roc_auc=0.87088  (+/-0.004962)
#> 12 ◯ accept suboptimal  roc_auc=0.86803  (+/-0.005195)
#> 13 ◯ accept suboptimal  roc_auc=0.85294  (+/-0.006498)
#> 14 ─ discard suboptimal roc_auc=0.84689  (+/-0.006867)
#> 15 ✖ restart from best  roc_auc=0.85021  (+/-0.006623)
#> 16 ◯ accept suboptimal  roc_auc=0.87607  (+/-0.004318)
#> 17 ◯ accept suboptimal  roc_auc=0.87245  (+/-0.004799)
#> 18 + better suboptimal  roc_auc=0.87706  (+/-0.004131)
#> 19 ◯ accept suboptimal  roc_auc=0.87213  (+/-0.004791)
#> 20 ◯ accept suboptimal  roc_auc=0.86218  (+/-0.005773)
show_best(sa_res, metric = "roc_auc", n = 2)
#> # A tibble: 2 × 9
#>   frac_common_cov frac_ident…¹ .metric .esti…²  mean     n std_err .config .iter
#>             <dbl>        <dbl> <chr>   <chr>   <dbl> <int>   <dbl> <chr>   <int>
#> 1           0.308       0.0166 roc_auc binary  0.882    10 0.00365 Iter7       7
#> 2           0.121       0.0474 roc_auc binary  0.879    10 0.00386 Iter2       2
#> # … with abbreviated variable names ¹​frac_identity, ²​.estimator
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
  extract_parameter_set_dials() %>%
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
#> # A tibble: 2 × 9
#>   .order frac_common_cov frac_iden…¹ .metric .esti…²  mean     n std_err .config
#>    <int>           <dbl>       <dbl> <chr>   <chr>   <dbl> <int>   <dbl> <chr>  
#> 1      9           0.831      0.0207 roc_auc binary  0.901     1      NA Prepro…
#> 2      8           0.831      0.0207 roc_auc binary  0.893     1      NA Prepro…
#> # … with abbreviated variable names ¹​frac_identity, ²​.estimator
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
#> # A tibble: 2 × 9
#>   .order frac_common_cov frac_iden…¹ .metric .esti…²  mean     n std_err .config
#>    <int>           <dbl>       <dbl> <chr>   <chr>   <dbl> <int>   <dbl> <chr>  
#> 1      9           0.831      0.0207 roc_auc binary  0.901     1      NA Prepro…
#> 2      9           0.321      0.0561 roc_auc binary  0.898     1      NA Prepro…
#> # … with abbreviated variable names ¹​frac_identity, ²​.estimator
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

-   For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

-   If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/usemodels/issues).

-   Either way, learn how to create and share a
    [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
    (a minimal, reproducible example), to clearly communicate about your
    code.

-   Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
