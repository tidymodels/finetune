test_that("restore_rset subsets .resample_weights", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weights <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  attr(folds, ".resample_weights") <- weights

  result <- finetune:::restore_rset(folds, 1:3)

  expect_length(attr(result, ".resample_weights"), 3)
  expect_equal(attr(result, ".resample_weights"), weights[1:3])
})

test_that("restore_rset single-row subset returns single weight", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weights <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  attr(folds, ".resample_weights") <- weights

  result <- finetune:::restore_rset(folds, 2)

  expect_length(attr(result, ".resample_weights"), 1)
  expect_equal(attr(result, ".resample_weights"), 1.0)
})

test_that("restore_rset non-contiguous index maps weights correctly", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weights <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  attr(folds, ".resample_weights") <- weights

  result <- finetune:::restore_rset(folds, c(1, 3, 5))

  expect_length(attr(result, ".resample_weights"), 3)
  expect_equal(attr(result, ".resample_weights"), c(0.5, 1.5, 2.5))
})

test_that("restore_rset full index preserves all weights", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weights <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  attr(folds, ".resample_weights") <- weights

  result <- finetune:::restore_rset(folds, 1:5)

  expect_equal(attr(result, ".resample_weights"), weights)
})

test_that("restore_rset without weights keeps NULL", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)

  result <- finetune:::restore_rset(folds, 1:3)

  expect_null(attr(result, ".resample_weights"))
  expect_equal(nrow(result), 3)
})

test_that("randomize_resamples reorders weights to match rows", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weights <- seq_len(nrow(folds)) * 10
  attr(folds, ".resample_weights") <- weights

  split_ids_before <- purrr::map_chr(folds$splits, ~ .x$id$id)

  set.seed(42)
  result <- finetune:::randomize_resamples(folds)

  split_ids_after <- purrr::map_chr(result$splits, ~ .x$id$id)
  result_weights <- attr(result, ".resample_weights")

  expect_length(result_weights, nrow(folds))

  for (i in seq_len(nrow(result))) {
    orig_pos <- which(split_ids_before == split_ids_after[i])
    expect_equal(result_weights[i], weights[orig_pos])
  }
})

test_that("randomize_resamples with repeated CV reorders weights", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 3, repeats = 2)
  weights <- seq_len(nrow(folds)) * 10
  attr(folds, ".resample_weights") <- weights

  id_pairs_before <- paste(folds$id, folds$id2)

  set.seed(42)
  result <- finetune:::randomize_resamples(folds)

  id_pairs_after <- paste(result$id, result$id2)
  result_weights <- attr(result, ".resample_weights")

  expect_length(result_weights, nrow(folds))

  for (i in seq_len(nrow(result))) {
    orig_pos <- which(id_pairs_before == id_pairs_after[i])
    expect_equal(result_weights[i], weights[orig_pos])
  }
})

test_that("randomize_resamples without weights keeps NULL, no column leaks", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  orig_names <- names(folds)

  set.seed(42)
  result <- finetune:::randomize_resamples(folds)

  expect_null(attr(result, ".resample_weights"))
  expect_equal(names(result), orig_names)
  expect_false(".orig_order" %in% names(result))
  expect_false(".rand" %in% names(result))
})

test_that("randomize then restore composes correctly", {
  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weights <- c(10, 20, 30, 40, 50)
  attr(folds, ".resample_weights") <- weights

  split_ids_orig <- purrr::map_chr(folds$splits, ~ .x$id$id)

  set.seed(42)
  randomized <- finetune:::randomize_resamples(folds)
  subset_idx <- 1:3
  restored <- finetune:::restore_rset(randomized, subset_idx)

  split_ids_restored <- purrr::map_chr(restored$splits, ~ .x$id$id)
  restored_weights <- attr(restored, ".resample_weights")

  expect_length(restored_weights, 3)

  for (i in seq_along(split_ids_restored)) {
    orig_pos <- which(split_ids_orig == split_ids_restored[i])
    expect_equal(restored_weights[i], weights[orig_pos])
  }
})

test_that("tune_race_anova works with weighted resamples", {
  skip_if_not(
    exists("add_resample_weights", where = asNamespace("tune"), inherits = FALSE),
    message = "tune::add_resample_weights not available"
  )

  set.seed(1)
  folds <- rsample::vfold_cv(mtcars, v = 5)
  weighted_folds <- tune::add_resample_weights(folds, rep(1, nrow(folds)))

  spec <- parsnip::decision_tree(
    cost_complexity = parsnip::tune(),
    min_n = parsnip::tune()
  ) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("rpart")

  wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(mpg ~ .)

  grid <- expand.grid(cost_complexity = c(0.001, 0.01, 0.1), min_n = c(2, 5))

  expect_no_error({
    set.seed(42)
    result <- tune_race_anova(
      wflow,
      resamples = weighted_folds,
      grid = grid,
      control = control_race(verbose_elim = FALSE, verbose = FALSE)
    )
  })
})
