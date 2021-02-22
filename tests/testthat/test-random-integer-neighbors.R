library(dials)
library(purrr)
library(dplyr)

# ------------------------------------------------------------------------------

set.seed(123)
parameters <- parameters(list(tree_depth(range = c(2, 3))))
random_integer_neigbors <-
  map_dfr(1:500,
          ~ finetune:::random_integer_neighbor_calc(tibble(tree_depth = 3),
                                                    parameters, 0.75, FALSE))

# ------------------------------------------------------------------------------
test_that("random integers in range", {
          expect_true(all(random_integer_neigbors$tree_depth >= 2))
          expect_true(all(random_integer_neigbors$tree_depth <= 3))
})
