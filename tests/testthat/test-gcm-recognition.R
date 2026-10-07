test_that("gcm_recognition is registered", {
  expect_true("gcm_recognition" %in% names(list_models(domain = "memory")))
})

load_shin_nosofsky <- function() {
  read <- function(id) read_cogdata(system.file("extdata", id, paste0(id, ".csv"), package = "cogzoo"))
  stim <- read("shin_nosofsky1992_stimuli")
  resp <- read("shin_nosofsky1992_responses")
  dims <- paste0("dim", 1:6)
  list(
    mem = stim[stim$role == "old", dims],
    # 150 observations per stimulus is the approximation used in the original
    # reproduction (the source tables report proportions, not counts).
    data = data.frame(stim[dims], n_old = round(resp$observed_p_old * 150), n_total = 150)
  )
}

test_that("gcm_recognition predictions are valid probabilities and favor studied items", {
  d <- load_shin_nosofsky()
  model <- gcm_recognition()
  pred <- cm_predict(model, d$data[paste0("dim", 1:6)], mem = d$mem)

  expect_length(pred, nrow(d$data))
  expect_true(all(pred > 0 & pred < 1))

  # a studied exemplar is maximally similar to itself, so it should look
  # more familiar than a distant new item
  old_rows <- which(read_cogdata(system.file("extdata", "shin_nosofsky1992_stimuli", "shin_nosofsky1992_stimuli.csv", package = "cogzoo"))$role == "old")
  expect_gt(mean(pred[old_rows]), mean(pred[-old_rows]))
})

test_that("gcm_recognition's likelihood is -Inf outside the valid weight region", {
  d <- load_shin_nosofsky()
  bad <- gcm_recognition(weights = c(0.5, 0.5, 0.2, 0.1, 0.1))  # sums > 1, so w6 < 0
  expect_equal(cm_loglik(bad, d$data, mem = d$mem), -Inf)
})

test_that("gcm_recognition reproduces Shin & Nosofsky's (1992, Table 5) Exp. 1 estimates", {
  d <- load_shin_nosofsky()

  fitted <- gcm_recognition()
  for (i in 1:6) fitted <- cm_fit(fitted, d$data, mem = d$mem, control = list(maxit = 3000))

  published <- c(w1 = .006, w2 = .084, w3 = .102, w4 = .392, w5 = .218, sensitivity = 4.905, criterion = .280)
  estimated <- unlist(fitted$params)
  expect_lt(max(abs(estimated - published)), 0.05)
  expect_equal(fitted$fit$convergence, 0)
})
