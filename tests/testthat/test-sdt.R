test_that("sdt is registered", {
  expect_true("sdt" %in% names(list_models(domain = "memory")))
})

test_that("sdt predictions are well-formed and ordered correctly", {
  model <- sdt(dprime = 1.5, criterion = 0.2)
  pred <- cm_predict(model)

  expect_true(all(pred >= 0 & pred <= 1))
  expect_gt(pred["p_hit"], pred["p_fa"])  # positive discriminability means more hits than false alarms
})

test_that("dprime = 0 gives equal hit and false-alarm rates", {
  pred <- cm_predict(sdt(dprime = 0, criterion = 0.3))
  expect_equal(unname(pred["p_hit"]), unname(pred["p_fa"]))
})

test_that("sdt recovers known dprime and criterion from simulated counts", {
  set.seed(1)
  true_model <- sdt(dprime = 1.2, criterion = 0.3)
  data <- cm_simulate(true_model, n_signal = 5000, n_noise = 5000)

  start_model <- sdt(dprime = 0.5, criterion = 0)
  fitted <- cm_fit(start_model, data, method = "L-BFGS-B", lower = c(0.01, -3), upper = c(5, 3))

  expect_equal(fitted$params$dprime, true_model$params$dprime, tolerance = 0.05)
  expect_equal(fitted$params$criterion, true_model$params$criterion, tolerance = 0.1)
})
