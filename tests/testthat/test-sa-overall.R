test_that("formula interface", {
  skip_on_cran()
  expect_snapshot({
    set.seed(1)
    res <- f_wflow |>
      tune_sim_anneal(
        cell_folds,
        iter = 2,
        control = control_sim_anneal(verbose = TRUE)
      )
  })
  expect_equal(
    class(res),
    c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) == 9)
  expect_equal(res, .Last.tune.result)
  expect_null(.get_tune_eval_times(res))
  expect_null(.get_tune_eval_time_target(res))
})

# ------------------------------------------------------------------------------

test_that("recipe interface", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  expect_silent({
    set.seed(1)
    res <- rec_wflow |>
      tune_sim_anneal(
        cell_folds,
        iter = 2,
        control = control_sim_anneal(verbose = FALSE, verbose_iter = FALSE)
      )
  })

  expect_equal(
    class(res),
    c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) == 9)
  expect_equal(res, .Last.tune.result)
})

# ------------------------------------------------------------------------------

test_that("variable interface", {
  skip_on_cran()
  expect_snapshot({
    set.seed(1)
    res <- var_wflow |>
      tune_sim_anneal(
        cell_folds,
        iter = 2,
        control = control_sim_anneal(verbose = TRUE, verbose_iter = TRUE)
      )
  })
  expect_equal(
    class(res),
    c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) == 9)
  expect_equal(res, .Last.tune.result)

  # Check to see if iterations are picked up when an iterative object is used
  # as the initial object

  expect_snapshot({
    set.seed(1)
    new_res <- var_wflow |>
      tune_sim_anneal(
        cell_folds,
        iter = 2,
        initial = res,
        control = control_sim_anneal(verbose = FALSE)
      )
  })
  expect_true(nrow(collect_metrics(new_res)) == 15)
  expect_true(max(new_res$.iter) == 4)
  expect_true(sum(grepl("^initial", collect_metrics(new_res)$.config)) == 9)
  expect_equal(new_res, .Last.tune.result)

  # but not for non-iterative objects
  set.seed(1)
  grid_res <- var_wflow |>
    tune_grid(cell_folds, grid = 2)

  expect_snapshot({
    set.seed(1)
    new_new_res <- var_wflow |>
      tune_sim_anneal(
        cell_folds,
        iter = 2,
        initial = grid_res,
        control = control_sim_anneal(verbose = FALSE)
      )
  })
  expect_true(nrow(collect_metrics(new_new_res)) == 12)
  expect_true(max(new_new_res$.iter) == 2)
  expect_true(sum(grepl("^initial", collect_metrics(new_new_res)$.config)) == 6)
  expect_equal(new_new_res, .Last.tune.result)
})


test_that("unfinalized parameters", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  data(two_class_dat, package = "modeldata")

  set.seed(5046)
  bt <- bootstraps(two_class_dat, times = 5)

  rec_example <- recipe(Class ~ ., data = two_class_dat)

  # RF
  model_rf <- rand_forest(mtry = tune()) |>
    set_mode("classification") |>
    set_engine("ranger")

  wf_rf <- workflow() |>
    add_model(model_rf) |>
    add_recipe(rec_example)

  set.seed(30)
  rf_res <- wf_rf |>
    tune_grid(resamples = bt, grid = 4)

  expect_snapshot({
    set.seed(40)
    rf_res_finetune <- wf_rf |>
      tune_sim_anneal(resamples = bt, initial = rf_res)
  })

  # don't supply an initial grid (#39)
  expect_snapshot({
    set.seed(40)
    rf_res_finetune <- wf_rf |>
      tune_sim_anneal(resamples = bt)
  })
})

test_that("incompatible parameter objects", {
  skip_on_cran()

  skip_if_not_installed("ranger")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("rsample")

  rf_spec <- parsnip::rand_forest(mode = "regression", mtry = tune::tune())

  set.seed(1)
  grid_with_bigger_range <-
    dials::grid_space_filling(dials::mtry(range = c(1, 16)))

  set.seed(1)
  car_folds <- rsample::vfold_cv(car_prices, v = 2)

  car_wflow <- workflows::workflow() |>
    workflows::add_formula(Price ~ .) |>
    workflows::add_model(rf_spec)

  set.seed(1)
  tune_res_with_bigger_range <- tune::tune_grid(
    car_wflow,
    resamples = car_folds,
    grid = grid_with_bigger_range
  )

  set.seed(1)
  parameter_set_with_smaller_range <-
    dials::parameters(dials::mtry(range = c(1, 5)))

  scrub_best <- function(lines) {
    has_best <- grepl("Initial best", lines)
    lines[has_best] <- ""
    lines
  }

  set.seed(1)
  expect_snapshot(error = TRUE, transform = scrub_best, {
    res <-
      tune_sim_anneal(
        car_wflow,
        param_info = parameter_set_with_smaller_range,
        resamples = car_folds,
        initial = tune_res_with_bigger_range,
        iter = 2
      )
  })
})

test_that("set event-level", {
  # See issue 40
  skip_if_not_installed("rpart")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("rsample")
  skip_on_cran()

  # ------------------------------------------------------------------------------

  set.seed(1)
  dat <- modeldata::sim_classification(500, intercept = 8)

  # We should get high sensitivity and low specificity when event_level = "first"
  # count(dat, class)
  # levels(dat$class)

  set.seed(2)
  rs <- vfold_cv(dat, strata = class)

  cart_spec <- decision_tree(min_n = tune()) |> set_mode("classification")

  stats <- metric_set(accuracy, sensitivity, specificity)

  # ------------------------------------------------------------------------------
  # high sensitivity and low specificity

  set.seed(3)
  cart_res_first <-
    cart_spec |>
    tune_sim_anneal(
      class ~ .,
      rs,
      control = control_sim_anneal(event_level = "first", verbose_iter = FALSE),
      metrics = stats
    )

  results_first <-
    cart_res_first |>
    collect_metrics() |>
    dplyr::filter(.metric != "accuracy") |>
    dplyr::select(.config, .metric, mean) |>
    tidyr::pivot_wider(
      id_cols = .config,
      names_from = .metric,
      values_from = mean
    )

  dir_check <- all(results_first$sensitivity > results_first$specificity)
  expect_true(dir_check)

  # ------------------------------------------------------------------------------
  # Now reversed

  set.seed(3)
  cart_res_second <-
    cart_spec |>
    tune_sim_anneal(
      class ~ .,
      rs,
      control = control_sim_anneal(
        event_level = "second",
        verbose_iter = FALSE
      ),
      metrics = stats
    )

  results_second <-
    cart_res_second |>
    collect_metrics() |>
    dplyr::filter(.metric != "accuracy") |>
    dplyr::select(.config, .metric, mean) |>
    tidyr::pivot_wider(
      id_cols = .config,
      names_from = .metric,
      values_from = mean
    )

  rev_dir_check <- all(results_second$sensitivity < results_second$specificity)
  expect_true(rev_dir_check)
})
