test_that("random integers in range", {
  set.seed(123)
  parameters <- dials::parameters(list(dials::tree_depth(range = c(2, 3))))
  random_integer_neigbors <-
    purrr::map(
      1:500,
      \(x)
        finetune:::random_integer_neighbor_calc(
          tibble::tibble(tree_depth = 3),
          parameters,
          0.75,
          FALSE
        )
    ) |>
    purrr::list_rbind()

  expect_true(all(random_integer_neigbors$tree_depth >= 2))
  expect_true(all(random_integer_neigbors$tree_depth <= 3))
})
