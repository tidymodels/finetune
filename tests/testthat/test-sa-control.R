
## -----------------------------------------------------------------------------

test_that('control_sim_anneal arg passing', {
  expect_equal(control_sim_anneal(verbose = TRUE)$verbose, TRUE)
  expect_equal(control_sim_anneal(no_improve = 13)$no_improve, 13L)
  expect_equal(control_sim_anneal(restart = 2)$restart, 2)
  expect_equal(control_sim_anneal(radius = rep(.12, 2))$radius, rep(.12, 2))
  expect_equal(control_sim_anneal(flip = .122)$flip, .122)
  expect_equal(control_sim_anneal(cooling_coef = 1/10)$cooling_coef, 1/10)
  expect_equal(control_sim_anneal(extract = function(x) x)$extract, function(x) x)
  expect_equal(control_sim_anneal(save_pred = TRUE)$save_pred, TRUE)
  expect_equal(control_sim_anneal(time_limit = 2)$time_limit, 2)
  expect_equal(control_sim_anneal(pkgs = "carrot")$pkgs, "carrot")
  expect_equal(control_sim_anneal(save_workflow = TRUE)$save_workflow, TRUE)
})

test_that('control_sim_anneal bad arg passing', {
  expect_error(
    control_sim_anneal(verbose = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_sim_anneal(verbose = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_sim_anneal(save_pred = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_sim_anneal(save_pred = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_sim_anneal(save_workflow = "TRUE"),
    "should be a single logical value"
  )
  expect_error(
    control_sim_anneal(save_workflow = rep(TRUE, 2)),
    "should be a single logical value"
  )
  expect_error(
    control_sim_anneal(no_improve = "yes"),
    "should be a single numeric or integer"
  )
  expect_error(
    control_sim_anneal(no_improve = 0:1),
    "should be a single numeric or integer"
  )
  expect_error(
    control_sim_anneal(no_improve = 1),
    "'no_improve' should be"
  )
  expect_error(
    control_sim_anneal(restart = "yes"),
    "should be a single numeric or integer"
  )
  expect_error(
    control_sim_anneal(restart = 0:1),
    "should be a single numeric or integer"
  )
  expect_error(
    control_sim_anneal(restart = 1),
    "'restart' should be"
  )
  expect_message(
    control_sim_anneal(no_improve = 2, restart = 6),
    "but the search will stop after"
  )
  expect_error(
    control_sim_anneal(radius = "huge"),
    "should be two numeric values"
  )
  expect_equal(control_sim_anneal(radius = c(-1, .2))$radius, c(0.001, .2))
  expect_equal(control_sim_anneal(radius = c(15, .1))$radius, c(.1, 0.999))
  expect_error(
    control_sim_anneal(flip = 0:1),
    "should be a single numeric value"
  )
  expect_error(
    control_sim_anneal(flip = "huge"),
    "should be a single numeric value"
  )
  expect_equal(control_sim_anneal(flip = -1)$flip, 0)
  expect_equal(control_sim_anneal(flip =  2)$flip, 1)
  expect_error(
    control_sim_anneal(cooling_coef = 0:1),
    "should be a single numeric value"
  )
  expect_error(
    control_sim_anneal(cooling_coef = "huge"),
    "should be a single numeric value"
  )
  expect_equal(control_sim_anneal(cooling_coef = -1)$cooling_coef, 0.0001)
  expect_equal(control_sim_anneal(cooling_coef =  2)$cooling_coef, 2)
  expect_error(
    control_sim_anneal(pkg = 0:1),
    "should be a character or NULL in"
  )
  expect_error(
    control_sim_anneal(extract = 0:1),
    "should be a function or NULL in"
  )
})
