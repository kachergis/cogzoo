test_that("minerva2 is registered", {
  expect_true("minerva2" %in% names(list_models(domain = "memory")))
})

test_that("minerva2 echo intensity is higher for studied than unstudied probes", {
  set.seed(1)
  n_features <- 20
  episodes <- matrix(sample(c(-1, 0, 1), 8 * n_features, replace = TRUE), nrow = 8)
  studied_probe <- episodes[1, , drop = FALSE]
  unstudied_probe <- matrix(sample(c(-1, 0, 1), n_features, replace = TRUE), nrow = 1)

  model <- minerva2(p_encode = 1, retention = 1)
  pred <- cm_predict(model, rbind(studied_probe, unstudied_probe), episodes, n_reps = 20)

  expect_equal(nrow(pred), 2)
  expect_gt(pred$intensity[1], pred$intensity[2])
})

test_that("more repetitions in memory produce higher echo intensity", {
  set.seed(1)
  n_features <- 20
  item <- matrix(sample(c(-1, 0, 1), n_features, replace = TRUE), nrow = 1)
  episodes_once <- item
  episodes_thrice <- rbind(item, item, item)

  model <- minerva2(p_encode = 1, retention = 1)
  intensity_once <- cm_predict(model, item, episodes_once, n_reps = 20)$intensity
  intensity_thrice <- cm_predict(model, item, episodes_thrice, n_reps = 20)$intensity

  expect_gt(intensity_thrice, intensity_once)
})
