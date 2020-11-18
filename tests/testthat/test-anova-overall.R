source(file.path(test_path(), "..", "helpers.R"))

# ------------------------------------------------------------------------------

test_that('formula interface', {
  skip_on_cran()
  expect_message(
    expect_error({
      set.seed(1)
      res <- f_wflow %>%
        tune_race_anova(cell_folds, grid = grid_mod,
                        control = control_race(verbose_elim = TRUE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == nrow(grid_mod) * 2)
})

# ------------------------------------------------------------------------------

test_that('recipe interface', {
  skip_on_cran()
  expect_silent(
    expect_error({
      set.seed(1)
      res <- rec_wflow %>%
        tune_race_anova(cell_folds, grid = grid_mod_rec,
                        control = control_race(verbose_elim = FALSE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == nrow(grid_mod_rec) * 2)
})

# ------------------------------------------------------------------------------

test_that('variable interface', {
  skip_on_cran()
  expect_silent(
    expect_error({
      set.seed(1)
      res <- var_wflow %>%
        tune_race_anova(cell_folds, grid = grid_mod,
                        control = control_race(verbose_elim = FALSE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("tune_race", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == nrow(grid_mod) * 2)
})
