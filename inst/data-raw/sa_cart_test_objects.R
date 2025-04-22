library(finetune)
library(rpart)
library(dplyr)
library(tune)
library(rsample)
library(parsnip)
library(workflows)
library(ggplot2)

## -----------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")

set.seed(5046)
bt <- bootstraps(two_class_dat, times = 5)

## -----------------------------------------------------------------------------

cart_mod <-
  decision_tree(cost_complexity = tune(), min_n = tune()) |>
  set_engine("rpart") |>
  set_mode("classification")

## -----------------------------------------------------------------------------

ctrl <- control_sim_anneal(save_history = TRUE)

set.seed(2981)
# For reproducibility, set the seed before running.
cart_search <-
  cart_mod |>
  tune_sim_anneal(Class ~ ., resamples = bt, iter = 12, control = ctrl)

load(file.path(tempdir(), "sa_history.RData"))
cart_history <- result_history

save(
  cart_history,
  cart_search,
  file = file.path(testthat::test_path(), "sa_cart_test_objects.RData"),
  version = 2,
  compress = "xz"
)

if (!interactive()) {
  q("no")
}
