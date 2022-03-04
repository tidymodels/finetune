
## -----------------------------------------------------------------------------

test_that("control_race arg passing", {
  expect_equal(control_race(verbose = TRUE)$verbose, TRUE)
  expect_equal(control_race(verbose_elim = TRUE)$verbose_elim, TRUE)
  expect_equal(control_race(burn_in = 13)$burn_in, 13)
  expect_equal(control_race(num_ties = 2)$num_ties, 2)
  expect_equal(control_race(alpha = .12)$alpha, .12)
  expect_equal(control_race(extract = function(x) x)$extract, function(x) x)
  expect_equal(control_race(save_pred = TRUE)$save_pred, TRUE)
  expect_equal(control_race(pkgs = "carrot")$pkgs, "carrot")
  expect_equal(control_race(save_workflow = TRUE)$save_workflow, TRUE)
})

test_that("control_race bad arg passing", {
  expect_error(
    control_race(verbose = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_race(verbose = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_race(verbose_elim = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_race(verbose_elim = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_race(save_pred = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_race(save_pred = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_race(save_workflow = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_race(save_workflow = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_race(burn_in = "yes"),
    "should be a single numeric value"
  )
  expect_error(
    control_race(burn_in = 0:1),
    "should be a single numeric value"
  )
  expect_error(
    control_race(burn_in = 1),
    "ould be at least two"
  )
  expect_error(
    control_race(num_ties = "yes"),
    "a single numeric value"
  )
  expect_error(
    control_race(num_ties = 0:1),
    "a single numeric value"
  )
  expect_error(
    control_race(alpha = 0:1),
    "should be a single numeric value"
  )
  expect_error(
    control_race(alpha = "huge"),
    "should be a single numeric value"
  )
  expect_error(
    control_race(alpha = 1),
    "'alpha' should be on"
  )
  expect_error(
    control_race(pkg = 0:1),
    "should be a character or NULL in"
  )
  expect_error(
    control_race(extract = 0:1),
    "should be a function or NULL in"
  )
})
