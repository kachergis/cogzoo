test_that("alcove is registered", {
  expect_true("alcove" %in% names(list_models(domain = "categorization")))
})

test_that("cm_predict.alcove errors clearly without catlearn installed", {
  skip_if(requireNamespace("catlearn", quietly = TRUE), "catlearn is installed; error path not applicable")
  s <- shj_sequence(1, n_subj = 1, n_blocks = 1)
  expect_error(cm_predict(alcove(), s$x, s$category), "catlearn")
})

test_that("alcove returns valid response probabilities that rise over training", {
  skip_if_not_installed("catlearn")
  s <- shj_sequence(1)
  probs <- cm_predict(alcove(c = 2, phi = 2, lw = 0.05, la = 0.05), s$x, s$category, subject = s$subject)

  expect_equal(dim(probs), c(length(s$category), 2))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(probs)))
  expect_gt(mean_accuracy(probs, s$category, n_blocks = 2) , 0.4)
  late <- probs[cbind(seq_along(s$category), s$category)][(seq_along(s$category) - 1) %% 64 >= 48]
  expect_gt(mean(late), 0.7)
})

test_that("alcove resets between subjects: concatenating learners equals predicting each alone", {
  skip_if_not_installed("catlearn")
  s <- shj_sequence(2, n_subj = 2, n_blocks = 4)
  model <- alcove(c = 2, phi = 2, lw = 0.05, la = 0.05)
  together <- cm_predict(model, s$x, s$category, subject = s$subject)

  first <- s$subject == 1
  alone <- cm_predict(model, s$x[first, ], s$category[first], hidden = unique(s$x))
  expect_equal(together[first, ], alone)
})

test_that("alcove reproduces the SHJ ordering: Type I is learned faster than Type VI", {
  skip_if_not_installed("catlearn")
  model <- alcove(c = 2, phi = 2, lw = 0.05, la = 0.05)
  acc <- vapply(c(1, 6), function(type) {
    s <- shj_sequence(type)
    mean_accuracy(cm_predict(model, s$x, s$category, subject = s$subject), s$category)
  }, numeric(1))
  expect_gt(acc[1], acc[2])
})

test_that("alcove's likelihood favors the generating parameters and bars invalid ones", {
  skip_if_not_installed("catlearn")
  s <- shj_sequence(2, n_subj = 10)
  true_model <- alcove(c = 2, phi = 2, lw = 0.05, la = 0.05)
  set.seed(5)
  data <- data.frame(response = cm_simulate(true_model, s$x, s$category, subject = s$subject))

  ll <- function(m) cm_loglik(m, data, x = s$x, category = s$category, subject = s$subject)
  expect_gt(ll(true_model), ll(alcove(c = 0.5, phi = 0.5, lw = 0.3, la = 0.01)))
  expect_equal(ll(alcove(c = -1)), -Inf)
})
