
<!-- README.md is generated from README.Rmd. Please edit that file -->

# finetune

<!-- badges: start -->

<!-- badges: end -->

`finetune` contains some extra functions for model tuning that extend
what is currently in the `tune` package.

Very rough version of the package right now but it works fairly well.
There are two main sets of tools.

Tuning via *simulated annealing* optimization is another iterative
search tool for finding good values:

``` r
library(tidymodels)
#> ── Attaching packages ──────────────────────────────────────────────────────────────────────── tidymodels 0.1.1 ──
#> ✓ broom     0.7.0      ✓ recipes   0.1.13
#> ✓ dials     0.0.8      ✓ rsample   0.0.7 
#> ✓ dplyr     1.0.0      ✓ tibble    3.0.3 
#> ✓ ggplot2   3.3.2      ✓ tidyr     1.1.0 
#> ✓ infer     0.5.2      ✓ tune      0.1.1 
#> ✓ modeldata 0.0.2      ✓ workflows 0.1.2 
#> ✓ parsnip   0.1.2      ✓ yardstick 0.0.7 
#> ✓ purrr     0.3.4
#> ── Conflicts ─────────────────────────────────────────────────────────────────────────── tidymodels_conflicts() ──
#> x purrr::discard() masks scales::discard()
#> x dplyr::filter()  masks stats::filter()
#> x dplyr::lag()     masks stats::lag()
#> x recipes::step()  masks stats::step()
library(finetune)

# Syntax very similar to `tune_grid()` or `tune_Bayes()`: 

## -----------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(6376)
rs <- bootstraps(two_class_dat, times = 20)

# optimize an xgboost model

xgb <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune()
    ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## -----------------------------------------------------------------------------

set.seed(8300)
sa_res <- xgb %>% tune_sim_anneal(Class ~ ., resamples = rs, iter = 20)
#> Initial best: 0.8706370
#>  1 ♥  roc_auc: 0.8721229  improvement (new best)
#>  2 ♥  roc_auc: 0.8735684  improvement (new best)
#>  3 ♥  roc_auc: 0.8760699  improvement (new best)
#>  4 ♥  roc_auc: 0.8768542  improvement (new best)
#>  5 ♥  roc_auc: 0.8776209  improvement (new best)
#>  6 ♥  roc_auc: 0.8781190  improvement (new best)
#>  7 +  roc_auc: 0.8776486  accept
#>  8 ♥  roc_auc: 0.8777705  improvement
#>  9 ♥  roc_auc: 0.8779312  improvement
#> 10 +  roc_auc: 0.8774184  accept
#> 11 ♥  roc_auc: 0.8781017  improvement
#> 12 +  roc_auc: 0.8767606  accept
#> 13 ♥  roc_auc: 0.8780978  improvement
#> 14 +  roc_auc: 0.8779688  accept
#> 15 +  roc_auc: 0.8773394  accept
#> 16 +  roc_auc: 0.8761216  accept
#> 17 ♥  roc_auc: 0.8779585  improvement
#> 18 +  roc_auc: 0.8766236  accept
#> 19 ♥  roc_auc: 0.8774352  improvement
#> 20 +  roc_auc: 0.8765054  accept
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
  xgb %>%
  parameters() %>%
  grid_max_entropy(size = 20)

set.seed(11)
grid_anova <- xgb %>% tune_race_anova(Class ~ ., resamples = rs, grid = grid)
#> ℹ Bootstrap05: 8 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap07: 5 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap10: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap19: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap11: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap01: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap08: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap18: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap12: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap20: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap17: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap03: 4 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap16: 3 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap15: 3 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap14: 3 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap09: 3 of 20 candidate sub-models remain (filtered using roc_auc).
#> ℹ Bootstrap13: 2 of 20 candidate sub-models remain (filtered using roc_auc).
```

`tune_race_win_loss()` can also be used.

## Code of Conduct

Please note that the finetune project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
