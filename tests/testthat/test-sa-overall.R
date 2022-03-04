source(file.path(test_path(), "..", "helpers.R"))

# ------------------------------------------------------------------------------

test_that("formula interface", {
  skip_on_cran()
  expect_message(
    expect_error(
      {
        set.seed(1)
        res <- f_wflow %>%
          tune_sim_anneal(cell_folds,
            iter = 2,
            control = control_sim_anneal(verbose = TRUE)
          )
      },
      regex = NA
    )
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})

# ------------------------------------------------------------------------------

test_that("recipe interface", {
  skip_on_cran()
  expect_silent(
    expect_error(
      {
        set.seed(1)
        res <- rec_wflow %>%
          tune_sim_anneal(cell_folds,
            iter = 2,
            control = control_sim_anneal(verbose = FALSE)
          )
      },
      regex = NA
    )
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)
})

# ------------------------------------------------------------------------------

test_that("variable interface", {
  skip_on_cran()
  expect_silent(
    expect_error(
      {
        set.seed(1)
        res <- var_wflow %>%
          tune_sim_anneal(cell_folds,
            iter = 2,
            control = control_sim_anneal(verbose = FALSE)
          )
      },
      regex = NA
    )
  )
  expect_equal(class(res), c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(collect_metrics(res)) == 6)

  # Check to see if iterations are picked up when an iterative object is used
  # as the initial object

  expect_silent(
    expect_error(
      {
        set.seed(1)
        new_res <- var_wflow %>%
          tune_sim_anneal(cell_folds,
            iter = 2, initial = res,
            control = control_sim_anneal(verbose = FALSE)
          )
      },
      regex = NA
    )
  )
  expect_true(nrow(collect_metrics(new_res)) == 10)
  expect_true(max(new_res$.iter) == 4)
  expect_true(sum(grepl("^initial", collect_metrics(new_res)$.config)) == 6)

  # but not for non-iterative objects
  set.seed(1)
  grid_res <- var_wflow %>%
    tune_grid(cell_folds, grid = 2)

  expect_silent(
    expect_error(
      {
        set.seed(1)
        new_new_res <- var_wflow %>%
          tune_sim_anneal(cell_folds,
            iter = 2, initial = grid_res,
            control = control_sim_anneal(verbose = FALSE)
          )
      },
      regex = NA
    )
  )
  expect_true(nrow(collect_metrics(new_new_res)) == 8)
  expect_true(max(new_new_res$.iter) == 2)
  expect_true(sum(grepl("^initial", collect_metrics(new_new_res)$.config)) == 4)
})


test_that("unfinalized parameters", {
  skip_on_cran()
  library(workflows)
  library(rsample)
  library(parsnip)
  library(tibble)
  library(ranger)

  data(two_class_dat, package = "modeldata")

  set.seed(5046)
  bt <- bootstraps(two_class_dat, times = 5)

  rec_example <- recipe(Class ~ ., data = two_class_dat)

  # RF
  model_rf <- rand_forest(mtry = tune()) %>%
    set_mode("classification") %>%
    set_engine("ranger")

  wf_rf <- workflow() %>%
    add_model(model_rf) %>%
    add_recipe(rec_example)

  set.seed(30)
  rf_res <- wf_rf %>%
    tune_grid(resamples = bt, grid = 4)

  expect_error(
    {
      set.seed(40)
      rf_res_finetune <- wf_rf %>%
        tune_sim_anneal(resamples = bt, initial = rf_res)
    },
    regex = NA
  )
})
