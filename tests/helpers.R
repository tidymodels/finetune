library(finetune)
library(rsample)
library(workflows)
library(parsnip)
library(dplyr)
library(recipes)

# ------------------------------------------------------------------------------

if (rlang::is_installed("modeldata")) {

  data(cells, package = "modeldata")
  cells <- cells %>% dplyr::select(class, contains("ch_1"))
  set.seed(33)
  cell_folds <- vfold_cv(cells, v = 3, repeats = 2)

  ## -----------------------------------------------------------------------------

  cart_spec <-
    decision_tree(cost_complexity = tune(), min_n = tune()) %>%
    set_mode("classification") %>%
    set_engine("rpart")

  cart_rec <-
    recipe(class ~ ., data = cells) %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), num_comp = tune())

  ## -----------------------------------------------------------------------------

  rec_wflow <-
    cell_knn <-
    workflow() %>%
    add_model(cart_spec) %>%
    add_recipe(cart_rec)

  f_wflow <-
    cell_knn <-
    workflow() %>%
    add_model(cart_spec) %>%
    add_formula(class ~ .)

  var_wflow <-
    cell_knn <-
    workflow() %>%
    add_model(cart_spec) %>%
    add_variables(class, everything())


  # ------------------------------------------------------------------------------

  grid_mod <-
    expand.grid(cost_complexity = c(0.001, 0.0001), min_n = c(3, 4))

  grid_mod_rec <-
    expand.grid(cost_complexity = c(0.001, 0.0001), min_n = 3:4, num_comp = 19:20)
}
