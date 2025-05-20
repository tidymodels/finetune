suppressPackageStartupMessages(library(finetune))
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(yardstick))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(modeldata))

# ------------------------------------------------------------------------------

data(cells, package = "modeldata")
cells <- modeldata::cells |> dplyr::select(class, contains("ch_1"))
set.seed(33)
cell_folds <- rsample::vfold_cv(cells, v = 3, repeats = 2)

## -----------------------------------------------------------------------------

cart_spec <-
  parsnip::decision_tree(
    cost_complexity = parsnip::tune(),
    min_n = parsnip::tune()
  ) |>
  parsnip::set_mode("classification") |>
  parsnip::set_engine("rpart")

cart_rec <-
  recipes::recipe(class ~ ., data = cells) |>
  recipes::step_normalize(recipes::all_predictors()) |>
  recipes::step_pca(recipes::all_predictors(), num_comp = parsnip::tune())

## -----------------------------------------------------------------------------

rec_wflow <-
  cell_knn <-
    workflows::workflow() |>
    workflows::add_model(cart_spec) |>
    workflows::add_recipe(cart_rec)

f_wflow <-
  cell_knn <-
    workflows::workflow() |>
    workflows::add_model(cart_spec) |>
    workflows::add_formula(class ~ .)

var_wflow <-
  cell_knn <-
    workflows::workflow() |>
    workflows::add_model(cart_spec) |>
    workflows::add_variables(class, dplyr::everything())


# ------------------------------------------------------------------------------

grid_mod <-
  expand.grid(cost_complexity = c(0.001, 0.0001), min_n = c(3, 4))

grid_mod_rec <-
  expand.grid(cost_complexity = c(0.001, 0.0001), min_n = 3:4, num_comp = 19:20)

# ------------------------------------------------------------------------------
