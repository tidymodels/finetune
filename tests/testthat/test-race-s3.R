test_that("racing S3 methods", {
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")
  skip_if_not_installed("kknn")

  library(purrr)
  library(dplyr)
  library(parsnip)
  library(rsample)
  library(recipes)

  knn_mod_power <-
    nearest_neighbor(mode = "regression", dist_power = tune()) |>
    set_engine("kknn")

  simple_rec <- recipe(mpg ~ ., data = mtcars)

  set.seed(7898)
  race_folds <- vfold_cv(mtcars, repeats = 2)

  ctrl_rc <- control_race(save_pred = TRUE)
  set.seed(9323)
  anova_race <-
    tune_race_anova(
      knn_mod_power,
      simple_rec,
      resamples = race_folds,
      grid = tibble::tibble(dist_power = c(1 / 10, 1, 2)),
      control = ctrl_rc
    )

  # ------------------------------------------------------------------------------
  # collect metrics

  expect_equal(nrow(collect_metrics(anova_race)), 2)
  expect_equal(nrow(collect_metrics(anova_race, all_configs = TRUE)), 6)
  expect_equal(nrow(collect_metrics(anova_race, summarize = FALSE)), 2 * 20)
  expect_equal(
    nrow(collect_metrics(anova_race, summarize = FALSE, all_configs = TRUE)),
    nrow(map(anova_race$.metrics, \(x) x) |> list_rbind())
  )

  # ------------------------------------------------------------------------------
  # collect predictions

  expect_equal(
    nrow(collect_predictions(
      anova_race,
      all_configs = FALSE,
      summarize = TRUE
    )),
    nrow(mtcars) * 1 # 1 config x nrow(mtcars)
  )
  expect_equal(
    nrow(collect_predictions(anova_race, all_configs = TRUE, summarize = TRUE)),
    map(anova_race$.predictions, \(x) x) |>
      list_rbind() |>
      distinct(.config, .row) |>
      nrow()
  )
  expect_equal(
    nrow(collect_predictions(
      anova_race,
      all_configs = FALSE,
      summarize = FALSE
    )),
    nrow(mtcars) * 1 * 2 # 1 config x 2 repeats x nrow(mtcars)
  )
  expect_equal(
    nrow(collect_predictions(
      anova_race,
      all_configs = TRUE,
      summarize = FALSE
    )),
    nrow(map(anova_race$.predictions, \(x) x) |> list_rbind())
  )

  # ------------------------------------------------------------------------------
  # show_best and select_best

  expect_equal(nrow(show_best(anova_race, metric = "rmse")), 1)
  expect_true(all(show_best(anova_race, metric = "rmse")$n == 20))
  expect_equal(nrow(select_best(anova_race, metric = "rmse")), 1)
  expect_equal(
    nrow(select_by_pct_loss(
      anova_race,
      metric = "rmse",
      dist_power,
      limit = 10
    )),
    1
  )
  expect_equal(
    nrow(select_by_one_std_err(anova_race, metric = "rmse", dist_power)),
    1
  )
})
