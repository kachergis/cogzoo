test_that("cr_mpt is registered", {
  expect_true("cr_mpt" %in% names(list_models(domain = "memory")))
})

test_that("cr_mpt predictions are well-formed and ordered as the tree implies", {
  model <- cr_mpt(v = 0.6, g = 0.3, b = 0.2)
  pred <- cm_predict(model)

  expect_true(all(pred >= 0 & pred <= 1))
  # target "old" rate >= lure >= new, since each adds one more route to an "old" response
  expect_true(pred["p_target"] >= pred["p_lure"])
  expect_true(pred["p_lure"] >= pred["p_new"])
})

test_that("cr_mpt recovers known v, g, b from simulated aggregate counts", {
  set.seed(1)
  true_model <- cr_mpt(v = 0.5, g = 0.4, b = 0.15)
  # A large n keeps sampling noise on `b` (the smallest-magnitude parameter,
  # so tightest under expect_equal()'s *relative* tolerance) well under the
  # tolerance below; this is recovery precision, not a fitting bug.
  data <- cm_simulate(true_model, n_target = 20000, n_lure = 20000, n_new = 20000)

  start_model <- cr_mpt(v = 0.3, g = 0.3, b = 0.3)
  fitted <- cm_fit(
    start_model, data,
    method = "L-BFGS-B", lower = c(0.01, 0.01, 0.01), upper = c(0.99, 0.99, 0.99)
  )

  expect_equal(fitted$params$v, true_model$params$v, tolerance = 0.05)
  expect_equal(fitted$params$g, true_model$params$g, tolerance = 0.05)
  expect_equal(fitted$params$b, true_model$params$b, tolerance = 0.05)
})
