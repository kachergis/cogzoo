test_that("every bundled dataset passes schema validation", {
  csvs <- list.files(
    system.file("extdata", package = "cogzoo"),
    pattern = "\\.csv$", recursive = TRUE, full.names = TRUE
  )
  expect_true(length(csvs) > 0)
  for (f in csvs) {
    expect_no_error(validate_cogdata(f))
  }
})

test_that("validate_cogdata errors on a missing sidecar", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(x = 1), tmp, row.names = FALSE)
  expect_error(validate_cogdata(tmp), "Missing metadata sidecar")
})

test_that("validate_cogdata errors when data is missing a declared column", {
  tmp <- tempfile(fileext = ".csv")
  meta <- sub("\\.csv$", ".yaml", tmp)
  on.exit(unlink(c(tmp, meta)))
  utils::write.csv(data.frame(x = 1), tmp, row.names = FALSE)
  yaml::write_yaml(
    list(
      id = "tmp", title = "t", citation = "c", license = "l",
      domain = "d", task_type = "t",
      columns = list(list(name = "y", type = "integer", description = "missing"))
    ),
    meta
  )
  expect_error(validate_cogdata(tmp), "missing column")
})
