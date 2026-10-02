test_that("{{name}} is registered", {
  expect_true("{{name}}" %in% names(list_models()))
})

test_that("{{name}} predictions are well-formed", {
  model <- {{name}}()
  # TODO: build minimal `newdata` (and any fixed inputs, e.g. a memory set)
  # and check cm_predict() returns values in the expected range (e.g. [0, 1]
  # for a choice-probability model).
})

test_that("{{name}} recovers known parameters from simulated data", {
  # The standard bar for a new model: simulate data from a model with known
  # parameters, fit a model started from different values, and check the
  # fitted parameters land close to the truth. This is what actually
  # validates the log-likelihood is implemented correctly and the
  # parameters are identifiable from the data you simulate.
  set.seed(1)
  true_model <- {{name}}()
  # TODO: simulate `data` from true_model via cm_simulate()
  # fitted <- cm_fit({{name}}(), data)
  # expect_equal(fitted$params, true_model$params, tolerance = 0.15)
})
