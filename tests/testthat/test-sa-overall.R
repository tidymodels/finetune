source(file.path(test_path(), "..", "helpers.R"))

# ------------------------------------------------------------------------------

test_that('formula interface', {
  skip_on_cran()
  # Skip for < 4.0 due to random number differences
  skip_if(getRversion() < "4.0.0")
  expect_message(
    expect_error({
      set.seed(1)
      res <- f_wflow %>%
        tune_sim_anneal(cell_folds, iter = 2,
                        control = control_sim_anneal(verbose = TRUE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})

# ------------------------------------------------------------------------------

test_that('recipe interface', {
  skip_on_cran()
  # Skip for < 4.0 due to random number differences
  skip_if(getRversion() < "4.0.0")
  expect_silent(
    expect_error({
      set.seed(1)
      res <- rec_wflow %>%
        tune_sim_anneal(cell_folds, iter = 2,
                        control = control_sim_anneal(verbose = FALSE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})

# ------------------------------------------------------------------------------

test_that('variable interface', {
  skip_on_cran()
  # Skip for < 4.0 due to random number differences
  skip_if(getRversion() < "4.0.0")
  expect_silent(
    expect_error({
      set.seed(1)
      res <- var_wflow %>%
        tune_sim_anneal(cell_folds, iter = 2,
                        control = control_sim_anneal(verbose = FALSE))
    },
    regex = NA)
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})
