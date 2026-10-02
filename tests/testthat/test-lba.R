test_that("lba is registered", {
  expect_true("lba" %in% names(list_models(domain = "decision-making")))
})

test_that("cm_simulate.lba and cm_loglik.lba error clearly without rtdists installed", {
  skip_if(requireNamespace("rtdists", quietly = TRUE), "rtdists is installed; error path not applicable")
  expect_error(cm_simulate(lba(), n_trials = 10), "rtdists")
})

test_that("lba simulates sensible choices/RTs and recovers a known drift difference", {
  skip_if_not_installed("rtdists")
  set.seed(1)
  true_model <- lba(drift1 = 2, drift2 = 0.5, drift_sd = 0.3, start_range = 0.3, threshold = 1, ndt = 0.2)
  sim <- cm_simulate(true_model, n_trials = 1000)

  expect_true(all(sim$rt > 0))
  expect_true(all(sim$response %in% c(1, 2)))
  # the stronger accumulator (drift1) should win most of the time
  expect_gt(mean(sim$response == 1), 0.7)

  # See test-rescorla-wagner.R: every bound gets a real (if narrow) range --
  # lower == upper on any dimension breaks L-BFGS-B's finite-difference gradient.
  start_model <- lba(drift1 = 1, drift2 = 1, drift_sd = 0.3, start_range = 0.3, threshold = 1, ndt = 0.2)
  fitted <- cm_fit(
    start_model, sim,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01, 0.25, 0.25, 0.9, 0.15), upper = c(5, 5, 0.35, 0.35, 1.1, 0.25)
  )

  expect_gt(fitted$params$drift1, fitted$params$drift2)
})
