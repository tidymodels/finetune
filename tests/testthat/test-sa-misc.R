library(finetune)
library(tune)
library(dplyr)
library(discrim)
library(rsample)
library(workflows)
library(recipes)
library(modeldata)

## -----------------------------------------------------------------------------

data("two_class_dat")

## -----------------------------------------------------------------------------

rda_spec <-
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>%
  set_engine("klaR")

rda_param <- rda_spec %>%
  parameters() %>%
  update(
    frac_common_cov = frac_common_cov(c(.3, .6)),
    frac_identity = frac_identity(c(.3, .6))
  )


set.seed(813)
rs <- bootstraps(two_class_dat, times = 3)

rec <- recipe(Class ~ ., data = two_class_dat) %>%
  step_ns(A, deg_free = tune())

## -----------------------------------------------------------------------------

test_that("tune_sim_anneal formula", {
  skip_on_cran()

  expect_message(
    expect_error(
      {
        set.seed(1)
        f_res_1 <- rda_spec %>% tune_sim_anneal(Class ~ ., rs, iter = 3)
      },
      regex = NA
    )
  )
  expect_message(
    expect_error(
      {
        set.seed(1)
        f_res_2 <- rda_spec %>% tune_sim_anneal(Class ~ ., rs, iter = 3, param_info = rda_param)
      },
      regex = NA
    )
  )
  expect_true(all(collect_metrics(f_res_2)$frac_common_cov >= 0.3))
  expect_true(all(collect_metrics(f_res_2)$frac_common_cov <= 0.6))
  expect_true(all(collect_metrics(f_res_2)$frac_identity >= 0.3))
  expect_true(all(collect_metrics(f_res_2)$frac_identity <= 0.6))
})

## -----------------------------------------------------------------------------

test_that("tune_sim_anneal recipe", {
  skip_on_cran()

  expect_message(
    expect_error(
      {
        set.seed(1)
        f_rec_1 <- rda_spec %>% tune_sim_anneal(rec, rs, iter = 3)
      },
      regex = NA
    )
  )
  expect_equal(sum(names(collect_metrics(f_rec_1)) == "deg_free"), 1)
  expect_equal(sum(names(collect_metrics(f_rec_1)) == "frac_common_cov"), 1)
  expect_equal(sum(names(collect_metrics(f_rec_1)) == "frac_identity"), 1)
})



## -----------------------------------------------------------------------------

test_that("tune_sim_anneal workflow", {
  skip_on_cran()

  wflow <-
    workflow() %>%
    add_model(rda_spec) %>%
    add_recipe(rec)

  expect_message(
    expect_error(
      {
        set.seed(1)
        f_wflow_1 <- wflow %>% tune_sim_anneal(rs, iter = 3)
      },
      regex = NA
    )
  )
  expect_equal(sum(names(collect_metrics(f_wflow_1)) == "deg_free"), 1)
  expect_equal(sum(names(collect_metrics(f_wflow_1)) == "frac_common_cov"), 1)
  expect_equal(sum(names(collect_metrics(f_wflow_1)) == "frac_identity"), 1)
})

## -----------------------------------------------------------------------------

test_that("tune_sim_anneal with wrong type", {
  expect_error(
    tune_sim_anneal(1),
    "should be either a model or workflow"
  )
})
