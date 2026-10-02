test_that("gcm predictions are valid probabilities", {
  mem <- data.frame(d1 = c(1, 2, 3, 4), d2 = c(4, 3, 2, 1), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = c(1.5, 3.5), d2 = c(3.5, 1.5))
  model <- gcm(w1 = 0.5, sensitivity = 1, bias = 0.5)

  pred <- cm_predict(model, probe, mem = mem)

  expect_length(pred, nrow(probe))
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("gcm reproduces the bundled Nosofsky (1989) data at plausible likelihood", {
  d <- read_cogdata(system.file(
    "extdata", "nosofsky_1989", "nosofsky_1989_responses.csv",
    package = "cogzoo"
  ))
  mem <- data.frame(d1 = seq_len(nrow(d)), d2 = rev(seq_len(nrow(d))), category = rep(1:2, length.out = nrow(d)))
  fit_data <- data.frame(
    d1 = mem$d1, d2 = mem$d2,
    n_cat1 = d[["Cat-1-s"]], n_total = d[["Cat-1-s"]] + d[["Cat-2-s"]]
  )

  ll <- cm_loglik(gcm(), fit_data, mem = mem)

  expect_true(is.finite(ll))
})

test_that("gcm recovers known parameters from simulated data", {
  set.seed(1)
  mem <- data.frame(
    d1 = stats::runif(20, 0, 20), d2 = stats::runif(20, 0, 20),
    category = rep(1:2, 10)
  )
  probe <- data.frame(d1 = stats::runif(40, 0, 20), d2 = stats::runif(40, 0, 20))

  true_model <- gcm(w1 = 0.7, sensitivity = 0.3, bias = 0.5)
  n_total <- rep(50, nrow(probe))
  n_cat1 <- cm_simulate(true_model, probe, mem = mem, n = n_total)
  fit_data <- data.frame(probe, n_cat1 = n_cat1, n_total = n_total)

  start_model <- gcm(w1 = 0.5, sensitivity = 1, bias = 0.5)
  fitted <- cm_fit(
    start_model, fit_data, mem = mem,
    method = "L-BFGS-B", lower = c(0.01, 0.01, 0.01), upper = c(0.99, 5, 0.99)
  )

  expect_equal(fitted$params$w1, true_model$params$w1, tolerance = 0.15)
  expect_equal(fitted$params$sensitivity, true_model$params$sensitivity, tolerance = 0.2)
  expect_equal(fitted$params$bias, true_model$params$bias, tolerance = 0.15)
})
