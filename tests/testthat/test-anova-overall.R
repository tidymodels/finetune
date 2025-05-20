test_that("formula interface", {
  skip_on_cran()
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  expect_snapshot({
    set.seed(1)
    res <- f_wflow |>
      tune_race_anova(
        cell_folds,
        grid = grid_mod,
        control = control_race(verbose_elim = TRUE)
      )
  })
  expect_equal(
    class(res),
    c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) < nrow(grid_mod) * 3)
  expect_equal(res, .Last.tune.result)
  expect_null(.get_tune_eval_times(res))
  expect_null(.get_tune_eval_time_target(res))
})

# ------------------------------------------------------------------------------

test_that("recipe interface", {
  skip_on_cran()
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")
  expect_silent({
    set.seed(1)
    res <- rec_wflow |>
      tune_race_anova(
        cell_folds,
        grid = grid_mod_rec,
        control = control_race(verbose_elim = FALSE)
      )
  })
  expect_equal(
    class(res),
    c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) < nrow(grid_mod) * 3)
  expect_equal(res, .Last.tune.result)
})

# ------------------------------------------------------------------------------

test_that("variable interface", {
  skip_on_cran()
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  expect_silent({
    set.seed(1)
    res <- var_wflow |>
      tune_race_anova(
        cell_folds,
        grid = grid_mod,
        control = control_race(verbose_elim = FALSE)
      )
  })
  expect_equal(
    class(res),
    c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame")
  )
  expect_true(nrow(collect_metrics(res)) < nrow(grid_mod) * 3)
  expect_equal(res, .Last.tune.result)
})

# ------------------------------------------------------------------------------

test_that("too few resamples", {
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  rs <- rsample::vfold_cv(modeldata::cells, v = 2)
  expect_snapshot_error(
    f_wflow |>
      tune_race_anova(
        rs,
        grid = grid_mod,
        control = control_race(verbose_elim = TRUE)
      )
  )
  expect_snapshot_error(
    f_wflow |>
      tune_race_win_loss(
        rs,
        grid = grid_mod,
        control = control_race(verbose_elim = TRUE)
      )
  )
})
