test_that("formula interface", {
  skip_on_cran()

  expect_snapshot({
    set.seed(1)
    res <- f_wflow |>
      tune_race_win_loss(
        cell_folds,
        grid = 5,
        control = control_race(verbose_elim = TRUE)
      )
  })

  expect_equal(
    class(res),
    c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) == 12) # this run has one elimination
  expect_equal(res, .Last.tune.result)
})

# ------------------------------------------------------------------------------

test_that("recipe interface", {
  skip_on_cran()
  expect_silent({
    set.seed(1)
    res <- rec_wflow |>
      tune_race_win_loss(
        cell_folds,
        grid = 5,
        control = control_race(verbose_elim = FALSE)
      )
  })
  expect_equal(
    class(res),
    c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) < 10)
  expect_equal(res, .Last.tune.result)
})

# ------------------------------------------------------------------------------

test_that("variable interface", {
  skip_on_cran()
  expect_silent({
    set.seed(1)
    res <- var_wflow |>
      tune_race_win_loss(
        cell_folds,
        grid = 5,
        control = control_race(verbose_elim = FALSE)
      )
  })
  expect_equal(
    class(res),
    c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) == 12) # one elimination
  expect_equal(res, .Last.tune.result)
})

# ------------------------------------------------------------------------------

test_that("one player is really bad", {
  skip_on_cran()
  skip_if_not_installed("tune", "0.1.5.9001")

  set.seed(1341)
  df <- tibble(
    x1 = rnorm(500, 1:500),
    x2 = sample(c(1:4), size = 500, replace = T)
  ) |>
    mutate(
      y = rbinom(500, 1, prob = (x1 / max(x1))) |> as.factor()
    )

  set.seed(121)
  df_folds <- vfold_cv(df, strata = y)

  rf_spec <-
    rand_forest(min_n = tune(), trees = 10) |>
    set_engine("ranger") |>
    set_mode("classification")

  wf <- workflow() |>
    add_formula(y ~ .) |>
    add_model(rf_spec)

  grid <- tibble(min_n = c(1, 40))
  ctrl <- control_race(burn_in = 2, alpha = .05, randomize = TRUE)
  set.seed(3355)
  tuning_results <- tune_race_win_loss(
    wf,
    resamples = df_folds,
    metrics = metric_set(roc_auc),
    grid = grid,
    control = ctrl
  )

  expect_snapshot(best_res <- show_best(tuning_results))
  expect_true(nrow(best_res) == 1)
})
