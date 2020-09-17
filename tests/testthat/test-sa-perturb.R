library(dials)
library(purrr)
library(dplyr)

## -----------------------------------------------------------------------------

num_prm <- parameters(mixture(), threshold())

## -----------------------------------------------------------------------------

test_that('numerical neighborhood', {
  vals <- tibble::tibble(mixture = 0.5, threshold = 0.5)
  set.seed(1)
  new_vals <-
    finetune:::random_neighbor(vals, num_prm, retain = 100, r = 0.12)

  correct_r <-
    purrr::map2_dbl(new_vals$mixture, new_vals$threshold,
                    ~ sqrt((.x - .5) ^ 2 + (.y - .5) ^ 2)) %>%
    map_lgl( ~ isTRUE(all.equal(.x, 0.12, tolerance = 0.001)))
  expect_true(all(correct_r))
})

test_that('numerical neighborhood boundary filters', {
  vals <- tibble::tibble(mixture = 0.05, threshold = 0.05)
  set.seed(1)
  new_vals <-
    finetune:::random_neighbor(vals, num_prm, retain = 100, tries = 100, r = 0.12)
  expect_true(nrow(new_vals) < 100)
})
