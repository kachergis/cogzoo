test_that("rescorla_wagner is registered", {
  expect_true("rescorla_wagner" %in% names(list_models(domain = "learning")))
})

test_that("rescorla_wagner's prediction rises toward the asymptote over acquisition", {
  cues <- matrix(1, nrow = 20, ncol = 1)
  outcomes <- rep(1, 20)
  model <- rescorla_wagner(alpha = 0.3)

  pred <- cm_predict(model, cues, outcomes)

  expect_equal(nrow(pred), 20)
  expect_true(all(diff(pred$prediction) > 0))  # monotonic acquisition
  expect_lt(max(pred$prediction), 1)           # asymptotes at, never exceeds, lambda
})

test_that("rescorla_wagner reproduces blocking: a redundant cue gains no strength", {
  # Phase 1: cue A alone predicts the outcome, to asymptote.
  # Phase 2: A+B compound predicts the same outcome -- B should be "blocked."
  cues <- rbind(
    cbind(A = rep(1, 20), B = rep(0, 20)),
    cbind(A = rep(1, 20), B = rep(1, 20))
  )
  outcomes <- rep(1, 40)
  model <- rescorla_wagner(alpha = 0.3)

  pred <- cm_predict(model, cues, outcomes)

  expect_lt(abs(pred$B[40]), 0.05)  # B's associative strength stays near zero
  expect_gt(pred$A[40], 0.9)        # A accounts for essentially all the prediction
})

test_that("rescorla_wagner recovers a known alpha from simulated expectancy data", {
  set.seed(1)
  cues <- matrix(1, nrow = 20, ncol = 1)
  outcomes <- rep(1, 20)

  true_model <- rescorla_wagner(alpha = 0.4, sigma = 0.05)
  pred <- cm_predict(true_model, cues, outcomes)
  observed <- data.frame(expectancy = pred$prediction + stats::rnorm(20, sd = 0.05))

  # Bounds on sigma deliberately aren't pinned to a single point: L-BFGS-B's
  # internal finite-difference gradient breaks down on a zero-width box
  # constraint (confirmed independently for ebrw()'s fit test), so every
  # fitted parameter here gets a real, if narrow, range instead.
  start_model <- rescorla_wagner(alpha = 0.1, sigma = 0.05)
  fitted <- cm_fit(
    start_model, observed, cues = cues, outcomes = outcomes,
    method = "L-BFGS-B", lower = c(0.01, 0.001), upper = c(0.99, 1)
  )

  expect_equal(fitted$params$alpha, true_model$params$alpha, tolerance = 0.1)
})
