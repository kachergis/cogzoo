test_that("sustain is registered", {
  expect_true("sustain" %in% names(list_models(domain = "categorization")))
})

test_that("cm_predict.sustain errors clearly without catlearn installed", {
  skip_if(requireNamespace("catlearn", quietly = TRUE), "catlearn is installed; error path not applicable")
  s <- shj_sequence(1, n_subj = 1, n_blocks = 1)
  expect_error(cm_predict(sustain(), s$x + 1, s$category), "catlearn")
})

test_that("sustain returns valid response probabilities that rise over training", {
  skip_if_not_installed("catlearn")
  s <- shj_sequence(1)
  probs <- cm_predict(sustain(), s$x + 1, s$category, subject = s$subject)

  expect_equal(dim(probs), c(length(s$category), 2))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(probs)))
  late <- probs[cbind(seq_along(s$category), s$category)][(seq_along(s$category) - 1) %% 64 >= 48]
  expect_gt(mean(late), 0.9)
})

test_that("sustain resets between subjects: concatenating learners equals predicting each alone", {
  skip_if_not_installed("catlearn")
  s <- shj_sequence(2, n_subj = 2, n_blocks = 4)
  together <- cm_predict(sustain(), s$x + 1, s$category, subject = s$subject)

  first <- s$subject == 1
  alone <- cm_predict(sustain(), s$x[first, ] + 1, s$category[first])
  expect_equal(together[first, ], alone)
})

test_that("sustain reproduces the SHJ ordering: Type I is learned faster than Type VI", {
  skip_if_not_installed("catlearn")
  acc <- vapply(c(1, 6), function(type) {
    s <- shj_sequence(type)
    mean_accuracy(cm_predict(sustain(), s$x + 1, s$category, subject = s$subject), s$category)
  }, numeric(1))
  expect_gt(acc[1], acc[2])
})

test_that("sustain's likelihood favors the generating parameters and bars invalid ones", {
  skip_if_not_installed("catlearn")
  s <- shj_sequence(6, n_subj = 6)
  true_model <- sustain(r = 6, beta = 1.5, d = 8, eta = 0.1)
  set.seed(5)
  data <- data.frame(response = cm_simulate(true_model, s$x + 1, s$category, subject = s$subject))

  ll <- function(m) cm_loglik(m, data, x = s$x + 1, category = s$category, subject = s$subject)
  expect_gt(ll(true_model), ll(sustain(r = 1, beta = 5, d = 1, eta = 0.5)))
  expect_equal(ll(sustain(r = -1)), -Inf)
})

test_that("sustain gives identical results in parallel and serial", {
  skip_if_not_installed("catlearn")
  skip_on_os("windows")  # mclapply can't fork there
  s <- shj_sequence(6, n_subj = 6, n_blocks = 4)
  serial <- cm_predict(sustain(), s$x + 1, s$category, subject = s$subject, cores = 1)
  parallel <- cm_predict(sustain(), s$x + 1, s$category, subject = s$subject, cores = 2)
  expect_equal(parallel, serial)

  data <- data.frame(response = s$category)
  ll <- function(cores) cm_loglik(sustain(), data, x = s$x + 1, category = s$category, subject = s$subject, cores = cores)
  expect_equal(ll(2), ll(1))
})
