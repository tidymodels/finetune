## Benchmarking results

To demonstrate, we use a SVM model with the `kernlab` package. 

```r
library(kernlab)
library(tidymodels)
library(finetune)
library(doParallel)

## -----------------------------------------------------------------------------

data(cells, package = "modeldata")
cells <- cells %>% select(-case)

## -----------------------------------------------------------------------------

set.seed(6376)
rs <- bootstraps(cells, times = 25)
```

We'll only tune the model parameters (i.e., not recipe tuning): 

```r
## -----------------------------------------------------------------------------

svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_rec <-
  recipe(class ~ ., data = cells) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors())

svm_wflow <-
  workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(svm_rec)

set.seed(1)
svm_grid <-
  svm_spec %>%
  parameters() %>%
  grid_latin_hypercube(size = 25)
```

We'll get the times for grid search and ANOVA racing with and without parallel processing: 

```r
## -----------------------------------------------------------------------------
## Regular grid search

system.time({
  set.seed(2)
  svm_wflow %>% tune_grid(resamples = rs, grid = svm_grid)
})
```

```
##    user  system elapsed 
## 792.239  21.934 814.532
```


```r
## -----------------------------------------------------------------------------
## With racing

system.time({
  set.seed(2)
  svm_wflow %>% tune_race_anova(resamples = rs, grid = svm_grid)
})
```

```
##    user  system elapsed 
## 382.121   9.401 391.558
```

Speed-up of 2.08-fold for racing. 


```r
## -----------------------------------------------------------------------------
## Parallel processing setup

cores <- parallel::detectCores(logical = FALSE)
cores
```

```
## [1] 10
```

```r
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
```


```r
## -----------------------------------------------------------------------------
## Parallel grid search

system.time({
  set.seed(2)
  svm_wflow %>% tune_grid(resamples = rs, grid = svm_grid)
})
```

```
##    user  system elapsed 
##   1.219   0.200 132.704
```

Parallel processing with grid search was 6.138-fold faster than sequential grid search.


```r
## -----------------------------------------------------------------------------
## Parallel racing

system.time({
  set.seed(2)
  svm_wflow %>% tune_race_anova(resamples = rs, grid = svm_grid)
})
```

```
##    user  system elapsed 
##  17.161   2.549  85.953
```

Parallel processing with racing was 9.476-fold faster than sequential grid search.

There is a compounding effect of racing and parallel processing but its magnitude depends on the type of model, number of resamples, number of tuning parameters, and so on. 


