library(finetune)
library(rsample)
library(workflows)
library(parsnip)
library(dplyr)
library(recipes)

## -----------------------------------------------------------------------------

data(cells, package = "modeldata")
cells <- cells %>% dplyr::select(class, contains("ch_1"))
set.seed(33)
cell_folds <- vfold_cv(cells, v = 3)

## -----------------------------------------------------------------------------

knn_spec <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")

knn_rec <-
  recipe(class ~ ., data = cells) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

rec_wflow <-
  cell_knn <-
  workflow() %>%
  add_model(knn_spec) %>%
  add_recipe(knn_rec)

f_wflow <-
  cell_knn <-
  workflow() %>%
  add_model(knn_spec) %>%
  add_formula(class ~ .)

var_wflow <-
  cell_knn <-
  workflow() %>%
  add_model(knn_spec) %>%
  add_variables(class, everything())

# ------------------------------------------------------------------------------

test_that('formula interface', {
  skip_on_cran()
  expect_message(
    expect_error({
      set.seed(1)
      res <- f_wflow %>%
        tune_sim_anneal(cell_folds, iter = 2,
                        control = control_sim_anneal(verbose = TRUE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})

# ------------------------------------------------------------------------------

test_that('recipe interface', {
  skip_on_cran()
  expect_silent(
    expect_error({
      set.seed(1)
      res <- rec_wflow %>%
        tune_sim_anneal(cell_folds, iter = 2,
                        control = control_sim_anneal(verbose = FALSE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})

# ------------------------------------------------------------------------------

test_that('variable interface', {
  skip_on_cran()
  expect_silent(
    expect_error({
      set.seed(1)
      res <- var_wflow %>%
        tune_sim_anneal(cell_folds, iter = 2,
                        control = control_sim_anneal(verbose = FALSE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})
