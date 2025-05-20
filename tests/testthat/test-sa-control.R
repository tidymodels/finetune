## -----------------------------------------------------------------------------

test_that("control_sim_anneal arg passing", {
  expect_equal(control_sim_anneal(verbose = TRUE)$verbose, TRUE)
  expect_equal(control_sim_anneal(no_improve = 13)$no_improve, 13L)
  expect_equal(control_sim_anneal(restart = 2)$restart, 2)
  expect_equal(control_sim_anneal(radius = rep(.12, 2))$radius, rep(.12, 2))
  expect_equal(control_sim_anneal(flip = .122)$flip, .122)
  expect_equal(control_sim_anneal(cooling_coef = 1 / 10)$cooling_coef, 1 / 10)
  expect_equal(
    control_sim_anneal(extract = function(x) x)$extract,
    function(x) x
  )
  expect_equal(control_sim_anneal(save_pred = TRUE)$save_pred, TRUE)
  expect_equal(control_sim_anneal(time_limit = 2)$time_limit, 2)
  expect_equal(control_sim_anneal(pkgs = "carrot")$pkgs, "carrot")
  expect_equal(control_sim_anneal(save_workflow = TRUE)$save_workflow, TRUE)
})

test_that("control_sim_anneal bad arg passing", {
  expect_snapshot_error(control_sim_anneal(verbose = "TRUE"))
  expect_snapshot_error(control_sim_anneal(verbose = rep(TRUE, 2)))
  expect_snapshot_error(control_sim_anneal(save_pred = "TRUE"))
  expect_snapshot_error(control_sim_anneal(save_pred = rep(TRUE, 2)))
  expect_snapshot_error(control_sim_anneal(save_workflow = "TRUE"))
  expect_snapshot_error(control_sim_anneal(save_workflow = rep(TRUE, 2)))
  expect_snapshot_error(control_sim_anneal(no_improve = "yes"))
  expect_snapshot_error(control_sim_anneal(no_improve = 0:1))
  expect_snapshot_error(control_sim_anneal(no_improve = 1))
  expect_snapshot_error(control_sim_anneal(restart = "yes"))
  expect_snapshot_error(control_sim_anneal(restart = 0:1))
  expect_snapshot_error(control_sim_anneal(restart = 1))
  expect_snapshot(control_sim_anneal(no_improve = 2, restart = 6))
  expect_snapshot_error(control_sim_anneal(radius = "huge"))
  expect_equal(control_sim_anneal(radius = c(-1, .2))$radius, c(0.001, .2))
  expect_equal(control_sim_anneal(radius = c(15, .1))$radius, c(.1, 0.999))
  expect_snapshot_error(control_sim_anneal(flip = 0:1))
  expect_snapshot_error(control_sim_anneal(flip = "huge"))
  expect_equal(control_sim_anneal(flip = -1)$flip, 0)
  expect_equal(control_sim_anneal(flip = 2)$flip, 1)
  expect_snapshot_error(control_sim_anneal(cooling_coef = 0:1))
  expect_snapshot_error(control_sim_anneal(cooling_coef = "huge"))
  expect_equal(control_sim_anneal(cooling_coef = -1)$cooling_coef, 0.0001)
  expect_equal(control_sim_anneal(cooling_coef = 2)$cooling_coef, 2)
  expect_snapshot_error(control_sim_anneal(pkg = 0:1))
  expect_snapshot_error(control_sim_anneal(extract = 0:1))
})

test_that("casting control_sim_anneal to control_grid", {
  expect_snapshot(parsnip::condense_control(
    control_sim_anneal(),
    control_grid()
  ))
})
