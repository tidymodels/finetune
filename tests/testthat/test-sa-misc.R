## -----------------------------------------------------------------------------

test_that("tune_sim_anneal interfaces", {
  skip_on_cran()
  skip_if_not_installed(c("discrim", "klaR"))

  library(discrim)
  data("two_class_dat", package = "modeldata")

  ## -----------------------------------------------------------------------------

  rda_spec <-
    discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) |>
    set_engine("klaR")

  rda_param <- rda_spec |>
    extract_parameter_set_dials() |>
    update(
      frac_common_cov = frac_common_cov(c(.3, .6)),
      frac_identity = frac_identity(c(.3, .6))
    )

  set.seed(813)
  rs <- bootstraps(two_class_dat, times = 3)

  rec <- recipe(Class ~ ., data = two_class_dat) |>
    step_ns(A, deg_free = tune())

  # ------------------------------------------------------------------------------
  # formula interface

  expect_snapshot({
    set.seed(1)
    f_res_1 <- rda_spec |> tune_sim_anneal(Class ~ ., rs, iter = 3)
  })

  expect_snapshot({
    set.seed(1)
    f_res_2 <- rda_spec |>
      tune_sim_anneal(Class ~ ., rs, iter = 3, param_info = rda_param)
  })

  expect_true(all(collect_metrics(f_res_2)$frac_common_cov >= 0.3))
  expect_true(all(collect_metrics(f_res_2)$frac_common_cov <= 0.6))
  expect_true(all(collect_metrics(f_res_2)$frac_identity >= 0.3))
  expect_true(all(collect_metrics(f_res_2)$frac_identity <= 0.6))

  # ------------------------------------------------------------------------------
  # recipe interface

  expect_snapshot({
    set.seed(1)
    f_rec_1 <- rda_spec |> tune_sim_anneal(rec, rs, iter = 3)
  })
  expect_equal(sum(names(collect_metrics(f_rec_1)) == "deg_free"), 1)
  expect_equal(sum(names(collect_metrics(f_rec_1)) == "frac_common_cov"), 1)
  expect_equal(sum(names(collect_metrics(f_rec_1)) == "frac_identity"), 1)

  # ------------------------------------------------------------------------------
  # workflow interface

  wflow <-
    workflow() |>
    add_model(rda_spec) |>
    add_recipe(rec)

  expect_snapshot({
    set.seed(1)
    f_wflow_1 <- wflow |> tune_sim_anneal(rs, iter = 3)
  })
  expect_equal(sum(names(collect_metrics(f_wflow_1)) == "deg_free"), 1)
  expect_equal(sum(names(collect_metrics(f_wflow_1)) == "frac_common_cov"), 1)
  expect_equal(sum(names(collect_metrics(f_wflow_1)) == "frac_identity"), 1)
})

## -----------------------------------------------------------------------------

test_that("tune_sim_anneal with wrong type", {
  expect_snapshot(
    tune_sim_anneal(1),
    error = TRUE
  )
})
