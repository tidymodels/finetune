test_that("control_race works with condense_control", {
  expect_equal(
    parsnip::condense_control(control_race(), control_grid()),
    control_grid(parallel_over = "everything")
  )

  expect_equal(
    parsnip::condense_control(control_race(), control_resamples()),
    control_resamples(parallel_over = "everything")
  )
})

test_that("control_sim_anneal works with condense_control", {
  expect_equal(
    parsnip::condense_control(control_sim_anneal(), control_grid()),
    control_grid(verbose = FALSE)
  )

  expect_equal(
    parsnip::condense_control(control_sim_anneal(), control_resamples()),
    control_resamples(verbose = FALSE)
  )
})
