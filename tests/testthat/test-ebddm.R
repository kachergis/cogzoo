test_that("ebddm is registered", {
  expect_true("ebddm" %in% names(list_models(domain = "decision-making")))
})

test_that("ebddm drift predictions are well-formed (no RWiener needed)", {
  mem <- data.frame(d1 = c(1, 2, 3, 4), d2 = c(4, 3, 2, 1), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = c(1.5, 3.5), d2 = c(3.5, 1.5))
  model <- ebddm()

  pred <- cm_predict(model, probe, mem = mem)

  expect_equal(nrow(pred), nrow(probe))
  expect_true(all(is.finite(pred$drift)))
  # closer to the category-1 exemplars should mean more positive drift
  expect_gt(pred$drift[1], pred$drift[2])
})

test_that("cm_simulate.ebddm errors clearly without RWiener installed", {
  skip_if(requireNamespace("RWiener", quietly = TRUE), "RWiener is installed; error path not applicable")
  mem <- data.frame(d1 = c(1, 2, 3, 4), d2 = c(4, 3, 2, 1), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = 1.5, d2 = 3.5)
  expect_error(cm_simulate(ebddm(), probe, mem = mem), "RWiener")
})

test_that("ebddm simulates and computes a finite log-likelihood", {
  skip_if_not_installed("RWiener")
  set.seed(1)
  mem <- data.frame(d1 = c(1, 2, 3, 4), d2 = c(4, 3, 2, 1), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = 1.5, d2 = 3.5)
  model <- ebddm()

  sim <- cm_simulate(model, probe, mem = mem, n_trials = 20)
  expect_equal(nrow(sim), 20)

  fit_data <- data.frame(d1 = probe$d1, d2 = probe$d2, q = sim$q, resp = sim$resp)
  ll <- cm_loglik(model, fit_data, mem = mem)
  expect_true(is.finite(ll))
})
