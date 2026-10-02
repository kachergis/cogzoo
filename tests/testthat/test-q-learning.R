test_that("q_learning is registered", {
  expect_true("q_learning" %in% names(list_models(domain = "learning")))
})

test_that("q_learning's choice probability shifts toward the richer option with experience", {
  set.seed(1)
  model <- q_learning(alpha = 0.3, beta = 3)
  sim <- cm_simulate(model, n_trials = 200, reward_probs = c(0.8, 0.2))

  pred <- cm_predict(model, sim, n_options = 2)
  # p_chosen measures calibration regardless of which option was taken each
  # trial; separately check the *overall* choice rate favors option 1.
  expect_true(mean(sim$choice[101:200] == 1) > mean(sim$choice[1:20] == 1))
  expect_true(all(pred$p_chosen >= 0 & pred$p_chosen <= 1))
})

test_that("q_learning recovers known alpha and beta from a simulated choice sequence", {
  set.seed(1)
  true_model <- q_learning(alpha = 0.4, beta = 2)
  sim <- cm_simulate(true_model, n_trials = 300, reward_probs = c(0.75, 0.25))

  start_model <- q_learning(alpha = 0.2, beta = 1)
  fitted <- cm_fit(
    start_model, sim, n_options = 2,
    method = "L-BFGS-B", lower = c(0.01, 0.01), upper = c(1, 10)
  )

  expect_equal(fitted$params$alpha, true_model$params$alpha, tolerance = 0.3)
  expect_equal(fitted$params$beta, true_model$params$beta, tolerance = 0.3)
})
