load(file.path(test_path(), "sa_cart_test_objects.RData"))

## -----------------------------------------------------------------------------

cart_param <- tune::.get_tune_parameters(cart_search)
cart_metrics <- tune::.get_tune_metrics(cart_search)
cart_outcomes <- tune::.get_tune_outcome_names(cart_search)
cart_rset_info <- attributes(cart_search)$rset_info

## -----------------------------------------------------------------------------

test_that("simulated annealing decisions", {
  for (iter_val in 1:max(cart_history$.iter)) {
    iter_hist <- cart_history |> filter(.iter < iter_val)
    iter_res <-
      cart_search |>
      filter(.iter == iter_val) |>
      tune:::new_tune_results(
        parameters = cart_param,
        outcomes = cart_outcomes,
        metrics = cart_metrics,
        eval_time = NULL,
        eval_time_target = NULL,
        rset_info = cart_rset_info
      )
    iter_new_hist <- finetune:::update_history(
      iter_hist,
      iter_res,
      iter_val,
      NULL
    )
    iter_new_hist$random[1:nrow(iter_new_hist)] <- cart_history$random[
      1:nrow(iter_new_hist)
    ]

    expect_equal(
      iter_new_hist$mean[iter_new_hist$.iter == iter_val],
      cart_history$mean[cart_history$.iter == iter_val]
    )

    expect_equal(
      iter_new_hist$std_err[iter_new_hist$.iter == iter_val],
      cart_history$std_err[cart_history$.iter == iter_val]
    )

    new_sa_res <-
      finetune:::sa_decide(
        iter_new_hist,
        parent = cart_history$.parent[cart_history$.iter == iter_val],
        metric = "roc_auc",
        maximize = TRUE,
        coef = control_sim_anneal()$cooling_coef
      )

    expect_equal(
      new_sa_res$results[new_sa_res$.iter == iter_val],
      cart_history$results[cart_history$.iter == iter_val]
    )

    expect_equal(
      new_sa_res$accept[new_sa_res$.iter == iter_val],
      cart_history$accept[cart_history$.iter == iter_val]
    )
  }
})

## -----------------------------------------------------------------------------

test_that("percent difference", {
  expect_equal(finetune:::percent_diff(1, 2), 100)
  expect_equal(finetune:::percent_diff(1, 1), 0)
  expect_equal(finetune:::percent_diff(1, 2, FALSE), -100)
  expect_equal(finetune:::percent_diff(1, 1, FALSE), 0)
})


## -----------------------------------------------------------------------------

test_that("acceptance probabilities", {
  expect_equal(finetune:::acceptance_prob(1, 2, iter = 1, maximize = TRUE), 1)
  expect_equal(finetune:::acceptance_prob(1, 1, iter = 1, maximize = TRUE), 1)

  expect_equal(
    finetune:::acceptance_prob(2, 1, iter = 1, maximize = TRUE),
    exp(finetune:::percent_diff(2, 1) * 1 * control_sim_anneal()$cooling_coef)
  )
  expect_equal(
    finetune:::acceptance_prob(2, 1, iter = 10, maximize = TRUE),
    exp(finetune:::percent_diff(2, 1) * 10 * control_sim_anneal()$cooling_coef)
  )

  expect_equal(finetune:::acceptance_prob(3, 1, iter = 1, maximize = FALSE), 1)
  expect_equal(finetune:::acceptance_prob(3, 1, iter = 1, maximize = FALSE), 1)

  expect_equal(
    finetune:::acceptance_prob(1, 3, iter = 1, maximize = FALSE),
    exp(
      finetune:::percent_diff(1, 3, maximize = FALSE) *
        1 *
        control_sim_anneal()$cooling_coef
    )
  )
  expect_equal(
    finetune:::acceptance_prob(1, 3, iter = 10, maximize = FALSE),
    exp(
      finetune:::percent_diff(1, 3, maximize = FALSE) *
        10 *
        control_sim_anneal()$cooling_coef
    )
  )
})

## -----------------------------------------------------------------------------

test_that("logging results", {
  iters <- max(cart_history$.iter)

  for (i in 1:iters) {
    expect_message(
      finetune:::log_sa_progress(
        x = cart_history |> filter(.iter <= i),
        metric = "roc_auc",
        max_iter = i
      ),
      regexp = cart_history$results[cart_history$.iter == i]
    )
  }
})
