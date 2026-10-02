test_that("ddm is registered", {
  expect_true("ddm" %in% names(list_models(domain = "decision-making")))
})

test_that("ddm's closed-form upper-boundary probability is well-formed (no RWiener needed)", {
  expect_equal(unname(cm_predict(ddm(drift = 0, bias = 0.5))["p_upper"]), 0.5)
  expect_equal(unname(cm_predict(ddm(drift = 0, bias = 0.7))["p_upper"]), 0.7)

  # more positive drift -> higher probability of hitting the upper boundary
  p_low <- cm_predict(ddm(drift = 0.5))["p_upper"]
  p_high <- cm_predict(ddm(drift = 2))["p_upper"]
  expect_gt(p_high, p_low)
})

test_that("cm_simulate.ddm and cm_loglik.ddm error clearly without RWiener installed", {
  skip_if(requireNamespace("RWiener", quietly = TRUE), "RWiener is installed; error path not applicable")
  expect_error(cm_simulate(ddm(), n_trials = 10), "RWiener")
})

test_that("ddm recovers a known drift rate from simulated trials", {
  skip_if_not_installed("RWiener")
  set.seed(1)
  true_model <- ddm(drift = 1, boundary_sep = 1.5, ndt = 0.2, bias = 0.5)
  sim <- cm_simulate(true_model, n_trials = 1000)

  # See test-rescorla-wagner.R: every bound gets a real (if narrow) range --
  # lower == upper on any dimension breaks L-BFGS-B's finite-difference gradient.
  start_model <- ddm(drift = 0, boundary_sep = 1.5, ndt = 0.2, bias = 0.5)
  fitted <- cm_fit(
    start_model, sim,
    method = "L-BFGS-B", lower = c(-5, 1.4, 0.15, 0.45), upper = c(5, 1.6, 0.25, 0.55)
  )

  expect_equal(fitted$params$drift, true_model$params$drift, tolerance = 0.2)
})
