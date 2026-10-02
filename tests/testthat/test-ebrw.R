test_that("ebrw is registered", {
  expect_true("ebrw" %in% names(list_models(domain = "decision-making")))
})

test_that("ebrw predictions are well-formed", {
  mem <- data.frame(d1 = c(1, 2, 3, 4), d2 = c(4, 3, 2, 1), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = c(1.5, 3.5), d2 = c(3.5, 1.5), category = c(1, 2))
  model <- ebrw()

  pred <- cm_predict(model, probe, mem = mem)

  expect_equal(nrow(pred), nrow(probe))
  expect_true(all(pred$p_cat1 >= 0 & pred$p_cat1 <= 1))
  expect_true(all(pred$rt_cat1 > 0 & pred$rt_cat2 > 0 & pred$mean_rt > 0))
})

test_that("ebrw's predicted accuracy recovers a known sensitivity by direct optimization", {
  # Isolates the underlying math in cm_predict.ebrw() from the generic
  # optimizer: with only 3 probes, letting all 6 parameters float at once
  # (as cm_fit.ebrw() below does) is not well identified, but the
  # relationship between sensitivity and predicted accuracy on its own is
  # smooth and has a unique optimum.
  mem <- data.frame(d1 = c(2, 4, 6, 8), d2 = c(8, 6, 4, 2), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = c(3, 5, 7), d2 = c(7, 5, 3), category = c(1, 1, 2))
  true_sensitivity <- 0.5

  predicted_accuracy <- function(sensitivity) {
    pred <- cm_predict(ebrw(sensitivity = sensitivity), probe, mem = mem)
    ifelse(probe$category == 1, pred$p_cat1, 1 - pred$p_cat1)
  }
  target <- predicted_accuracy(true_sensitivity)

  recovered <- stats::optimize(
    function(s) sum((predicted_accuracy(s) - target)^2),
    interval = c(0.05, 3)
  )

  expect_equal(recovered$minimum, true_sensitivity, tolerance = 0.05)
})

test_that("cm_fit.ebrw's weighted SSE criterion improves on the starting parameters", {
  set.seed(1)
  mem <- data.frame(d1 = c(2, 4, 6, 8), d2 = c(8, 6, 4, 2), category = c(1, 1, 2, 2))
  probe <- data.frame(d1 = c(3, 5, 7), d2 = c(7, 5, 3), category = c(1, 1, 2))

  true_model <- ebrw(sensitivity = 0.5, boundary = 4)
  sim <- cm_simulate(true_model, probe, mem = mem, n_trials = 300)
  observed <- do.call(rbind, lapply(split(sim, sim$stimulus), function(d) {
    data.frame(
      accuracy = mean(d$response == (probe$category[d$stimulus[1]] == 1)),
      mean_rt = mean(d$rt)
    )
  }))
  fit_data <- data.frame(probe, observed)

  weighted_sse <- function(model) {
    pred <- cm_predict(model, fit_data[c("d1", "d2", "category")], mem = mem)
    pred_accuracy <- ifelse(fit_data$category == 1, pred$p_cat1, 1 - pred$p_cat1)
    sum(
      (pred_accuracy - fit_data$accuracy)^2 / (stats::sd(fit_data$accuracy) / sqrt(nrow(fit_data))) +
        (pred$mean_rt - fit_data$mean_rt)^2 / (stats::sd(fit_data$mean_rt) / nrow(fit_data))
    )
  }

  start_model <- ebrw(sensitivity = 2, boundary = 4)
  fitted <- cm_fit(start_model, fit_data, mem = mem, method = "Nelder-Mead")

  expect_equal(fitted$fit$convergence, 0)
  expect_lt(weighted_sse(fitted), weighted_sse(start_model))
})
