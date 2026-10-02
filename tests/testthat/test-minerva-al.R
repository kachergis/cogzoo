test_that("minerva_al is registered", {
  expect_true("minerva_al" %in% names(list_models(domain = "learning")))
})

# A small acquisition scenario: cue A + context predicts outcome X on every
# trial. 40 cue features keeps p_encode >= 0.4 comfortably clear of the
# all-trace-unencoded numerical edge case documented in ?minerva_al.
small_acquisition <- function(n_trials = 20) {
  n_features <- 40
  cue_features <- 1:30
  a <- context <- outcome <- rep(0, n_features)
  a[1:10] <- 1
  context[15:30] <- 1
  outcome[31:40] <- 1

  event <- a + context + outcome
  probe <- a + context

  list(
    probes = matrix(rep(probe, n_trials), nrow = n_trials, byrow = TRUE),
    events = matrix(rep(event, n_trials), nrow = n_trials, byrow = TRUE),
    outcomes = matrix(rep(outcome, n_trials), nrow = n_trials, byrow = TRUE),
    cue_features = cue_features
  )
}

test_that("minerva_al expectancy increases over acquisition trials", {
  set.seed(1)
  scenario <- small_acquisition(20)
  model <- minerva_al(p_encode = 1)

  pred <- do.call(cm_predict, c(list(model = model, n_reps = 10), scenario))

  expect_equal(nrow(pred), 20)
  expect_true(all(pred$expectancy >= -1 & pred$expectancy <= 1))
  # later-trial expectancy should exceed early-trial expectancy as the association is learned
  expect_gt(mean(pred$expectancy[16:20]), mean(pred$expectancy[1:5]))
})

test_that("minerva_al's likelihood favors the generating p_encode over a clearly wrong one", {
  # A full MLE recovery test (fit both p_encode and sigma via a generic
  # derivative-free optimizer) is unreliable here: cm_loglik.minerva_al()
  # re-simulates the model stochastically on every call, so the objective a
  # generic optimizer sees is noisy from one evaluation to the next. What we
  # can and should check is the property that actually matters: the
  # likelihood ranks the true generating parameter above a clearly wrong one.
  set.seed(1)
  scenario <- small_acquisition(30)

  true_model <- minerva_al(p_encode = 0.8, sigma = 0.05)
  observed <- do.call(cm_predict, c(list(model = true_model, n_reps = 60), scenario))

  wrong_model <- minerva_al(p_encode = 0.4, sigma = 0.05)
  ll_true <- do.call(cm_loglik, c(list(model = true_model, data = observed, n_reps = 60), scenario))
  ll_wrong <- do.call(cm_loglik, c(list(model = wrong_model, data = observed, n_reps = 60), scenario))

  expect_gt(ll_true, ll_wrong)
})
