test_that("top-level win/loss filter interfaces", {
  skip_on_cran()
  # Skip for < 4.0 due to random number differences
  skip_if(getRversion() < "4.0.0")

  library(dials)

  # ------------------------------------------------------------------------------

  set.seed(2332)
  folds <- vfold_cv(mtcars, v = 5, repeats = 2)
  fold_att <- attributes(folds)
  spec <- decision_tree(cost_complexity = tune(), min_n = tune()) |>
    set_engine("rpart") |>
    set_mode("regression")
  wflow <- workflow() |>
    add_model(spec) |>
    add_formula(mpg ~ .)
  grid <- expand.grid(cost_complexity = c(0.001, 0.01), min_n = c(2:5))
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_normalize(all_predictors())
  prm <- extract_parameter_set_dials(wflow) |> update(min_n = min_n(c(2, 20)))

  # ------------------------------------------------------------------------------

  set.seed(129)
  suppressWarnings(
    wl_mod <- spec |> tune_race_win_loss(mpg ~ ., folds, grid = grid)
  )

  expect_true(inherits(wl_mod, "tune_race"))
  expect_true(inherits(wl_mod, "tune_results"))
  expect_true(tibble::is_tibble((wl_mod)))
  expect_null(.get_tune_eval_times(wl_mod))
  expect_null(.get_tune_eval_time_target(wl_mod))

  expect_silent({
    set.seed(129)
    suppressWarnings(
      wl_wlfow <-
        wflow |>
        tune_race_win_loss(
          folds,
          grid = grid,
          param_info = prm,
          control = control_race(verbose_elim = FALSE, save_pred = TRUE)
        )
    )
  })

  expect_true(inherits(wl_wlfow, "tune_race"))
  expect_true(inherits(wl_wlfow, "tune_results"))
  expect_true(tibble::is_tibble((wl_wlfow)))
  expect_true(sum(names(wl_wlfow) == ".predictions") == 1)

  get_mod <- function(x) workflows::extract_fit_parsnip(x)

  expect_silent({
    set.seed(129)
    suppressMessages(
      wl_rec <-
        spec |>
        tune_race_win_loss(
          rec,
          folds,
          grid = expand.grid(cost_complexity = c(.0001, .001), min_n = c(3, 5)),
          param_info = prm,
          control = control_race(
            verbose_elim = FALSE,
            extract = get_mod
          )
        )
    )
  })

  expect_true(inherits(wl_rec, "tune_race"))
  expect_true(inherits(wl_rec, "tune_results"))
  expect_true(tibble::is_tibble((wl_rec)))
  expect_true(sum(names(wl_rec) == ".extracts") == 1)
})
