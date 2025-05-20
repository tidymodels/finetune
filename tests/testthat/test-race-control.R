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
  expect_snapshot_error(control_race(verbose = "TRUE"))
  expect_snapshot_error(control_race(verbose = rep(TRUE, 2)))
  expect_snapshot_error(control_race(verbose_elim = "TRUE"))
  expect_snapshot_error(control_race(verbose_elim = rep(TRUE, 2)))
  expect_snapshot_error(control_race(save_pred = "TRUE"))
  expect_snapshot_error(control_race(save_pred = rep(TRUE, 2)))
  expect_snapshot_error(control_race(save_workflow = "TRUE"))
  expect_snapshot_error(control_race(save_workflow = rep(TRUE, 2)))
  expect_snapshot_error(control_race(burn_in = "yes"))
  expect_snapshot_error(control_race(burn_in = 0:1))
  expect_snapshot_error(control_race(burn_in = 1))
  expect_snapshot_error(control_race(num_ties = "yes"))
  expect_snapshot_error(control_race(num_ties = 0:1))
  expect_snapshot_error(control_race(alpha = 0:1))
  expect_snapshot_error(control_race(alpha = "huge"))
  expect_snapshot_error(control_race(alpha = 1))
  expect_snapshot_error(control_race(pkg = 0:1))
  expect_snapshot_error(control_race(extract = 0:1))
})

test_that("casting control_race to control_grid", {
  expect_snapshot(parsnip::condense_control(control_race(), control_grid()))
})
