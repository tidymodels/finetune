test_that("numerical neighborhood", {
  suppressPackageStartupMessages(library(dials))

  num_prm <- dials::parameters(dials::mixture(), dials::threshold())

  vals <- tibble::tibble(mixture = 0.5, threshold = 0.5)
  set.seed(1)
  new_vals <-
    finetune:::random_real_neighbor(vals, vals[0, ], num_prm, retain = 100)

  rad <- control_sim_anneal()$radius

  correct_r <-
    purrr::map2_dbl(
      new_vals$mixture,
      new_vals$threshold,
      ~ sqrt((.x - .5)^2 + (.y - .5)^2)
    ) |>
    purrr::map_lgl(\(x) x >= rad[1] & x <= rad[2])
  expect_true(all(correct_r))

  set.seed(1)
  prev <- tibble::tibble(mixture = runif(5), threshold = runif(5))

  set.seed(2)
  more_vals <- finetune:::new_in_neighborhood(
    vals,
    prev,
    num_prm,
    radius = rep(0.12, 2)
  )
  rad_vals <- sqrt((more_vals$mixture - .5)^2 + (more_vals$threshold - .5)^2)
  expect_equal(rad_vals, 0.12, tolerance = 0.001)
})

test_that("numerical neighborhood boundary filters", {
  suppressPackageStartupMessages(library(dials))
  num_prm <- dials::parameters(dials::mixture(), dials::threshold())

  vals <- tibble::tibble(mixture = 0.05, threshold = 0.05)
  set.seed(1)
  new_vals <-
    finetune:::random_real_neighbor(
      vals,
      vals[0, ],
      num_prm,
      retain = 100,
      tries = 100,
      r = 0.12
    )
  expect_true(nrow(new_vals) < 100)
})

## -----------------------------------------------------------------------------

test_that("categorical value switching", {
  suppressPackageStartupMessages(library(dials))
  cat_prm <- parameters(activation(), weight_func())

  vals <- tibble::tibble(activation = "relu", weight_func = "biweight")
  set.seed(1)
  new_vals <-
    purrr::map(
      1:1000,
      \(x)
        finetune:::random_discrete_neighbor(
          vals,
          cat_prm,
          prob = 1 / 4,
          change = FALSE
        )
    ) |>
    purrr::list_rbind()
  relu_same <- mean(new_vals$activation == "relu")
  biweight_same <- mean(new_vals$weight_func == "biweight")

  expect_true(relu_same > .7 & relu_same < .8)
  expect_true(biweight_same > .7 & biweight_same < .8)

  set.seed(1)
  prev <- tibble::tibble(
    activation = dials::values_activation[1:4],
    weight_func = dials::values_weight_func[1:4]
  )
  set.seed(2)
  must_change <- finetune:::new_in_neighborhood(vals, prev, cat_prm, flip = 1)
  expect_true(must_change$activation != "relu")
  expect_true(must_change$weight_func != "biweight")
})

## -----------------------------------------------------------------------------

test_that("reverse-unit encoding", {
  suppressPackageStartupMessages(library(dials))
  prm <-
    parameters(batch_size(), Laplace(), activation()) |>
    update(Laplace = Laplace(c(2, 4)), batch_size = batch_size(c(10, 20)))
  unit_vals <- tibble::tibble(batch_size = .1, Laplace = .4, activation = .7)
  vals <- finetune:::encode_set_backwards(unit_vals, prm)
  expect_true(vals$batch_size > 1)
  expect_true(vals$Laplace > 1)
  expect_true(is.character(vals$activation))
})
