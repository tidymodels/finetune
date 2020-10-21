## Benchmarking results

To demonstrate, we use a SVM model with the `kernlab` package. 

```r
library(kernlab)
library(tidymodels)
library(finetune)
library(doMC)

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
## 752.559  20.979 773.619
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
## 383.148   9.764 392.931
```

Speed-up of 1.97-fold for racing. 


```r
## -----------------------------------------------------------------------------
## Parallel processing setup

cores <- parallel::detectCores(logical = TRUE)
cores
```

```
## [1] 20
```

```r
registerDoMC(cores = cores)
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
##     user   system  elapsed 
## 1324.321   29.923  105.402
```

Parallel processing with grid search was 7.34-fold faster than sequential grid search.


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
## 727.499 190.218  78.423
```

Parallel processing with racing was 9.86-fold faster than sequential grid search.

There is a compounding effect of racing and parallel processing but its magnitude depends on the type of model, number of resamples, number of tuning parameters, and so on. 


