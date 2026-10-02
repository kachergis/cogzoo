test_that("gcm is registered", {
  models <- list_models(domain = "categorization")
  expect_true("gcm" %in% names(models))
})

test_that("model_info errors on an unknown model", {
  expect_error(model_info("not_a_real_model"), "Unknown model")
})
