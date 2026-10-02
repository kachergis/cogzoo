test_that("prototype is registered", {
  expect_true("prototype" %in% names(list_models(domain = "categorization")))
})

test_that("prototype predictions are valid probabilities", {
  prototypes <- data.frame(d1 = c(2, 6), d2 = c(6, 2), category = c(1, 2))
  probe <- data.frame(d1 = c(1.5, 6.5), d2 = c(6.5, 1.5))
  model <- prototype()

  pred <- cm_predict(model, probe, prototypes = prototypes)

  expect_length(pred, nrow(probe))
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("prototype is insensitive to within-category exemplar spread (unlike gcm)", {
  # Two memory sets share the same per-category means (prototypes) but differ
  # in spread. The prototype model should predict identically for both,
  # since it only ever looks at the category means.
  tight <- data.frame(d1 = c(3, 4, 6, 7), d2 = c(7, 6, 4, 3), category = c(1, 1, 2, 2))
  spread <- data.frame(d1 = c(1, 6, 4, 9), d2 = c(9, 4, 6, 1), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = 5, d2 = 5)

  prototypes_from <- function(mem) {
    stats::aggregate(cbind(d1, d2) ~ category, data = mem, FUN = mean)[, c("d1", "d2", "category")]
  }

  model <- prototype()
  pred_tight <- cm_predict(model, probe, prototypes = prototypes_from(tight))
  pred_spread <- cm_predict(model, probe, prototypes = prototypes_from(spread))

  expect_equal(pred_tight, pred_spread)
})

test_that("prototype recovers a known sensitivity from simulated data", {
  set.seed(1)
  prototypes <- data.frame(d1 = c(2, 8), d2 = c(8, 2), category = c(1, 2))
  probe <- data.frame(d1 = c(3, 5, 7), d2 = c(7, 5, 3))

  true_model <- prototype(sensitivity = 0.4)
  n_total <- rep(500, nrow(probe))
  pred <- cm_predict(true_model, probe, prototypes = prototypes)
  n_cat1 <- stats::rbinom(length(pred), n_total, pred)
  fit_data <- data.frame(probe, n_cat1 = n_cat1, n_total = n_total)

  # See the comment in test-rescorla-wagner.R: give every parameter a real
  # (if narrow) range rather than pinning any at lower == upper, which
  # breaks L-BFGS-B's finite-difference gradient.
  start_model <- prototype(sensitivity = 1)
  fitted <- cm_fit(
    start_model, fit_data, prototypes = prototypes,
    method = "L-BFGS-B", lower = c(0.4, 0.01, 0.4), upper = c(0.6, 5, 0.6)
  )

  expect_equal(fitted$params$sensitivity, true_model$params$sensitivity, tolerance = 0.3)
})
