#' Scaffold a new model
#'
#' Copies `inst/templates/model-template.R` and
#' `inst/templates/model-test-template.R` into `R/model-<name>.R` and
#' `tests/testthat/test-<name>.R`, substituting `name` and `domain`. Run this
#' from the package root while developing `cogzoo` itself.
#'
#' @param name Model identifier, e.g. `"minerva2"`. Used as the constructor
#'   name and S3 subclass — keep it short and lowercase.
#' @param domain Domain string, e.g. `"memory"`, `"categorization"`,
#'   `"learning"`, `"decision-making"`. Reuse an existing domain unless this
#'   model genuinely doesn't fit one.
#' @param path Package root to scaffold into (default: current directory).
#' @return Invisibly, the paths of the files created.
#' @export
add_model <- function(name, domain, path = ".") {
  stopifnot(grepl("^[a-z][a-z0-9_]*$", name))

  r_file <- file.path(path, "R", paste0("model-", name, ".R"))
  test_file <- file.path(path, "tests", "testthat", paste0("test-", name, ".R"))
  if (file.exists(r_file)) stop(r_file, " already exists.", call. = FALSE)

  fill_template <- function(template) {
    txt <- readLines(system.file("templates", template, package = "cogzoo"))
    txt <- gsub("{{name}}", name, txt, fixed = TRUE)
    txt <- gsub("{{domain}}", domain, txt, fixed = TRUE)
    txt
  }

  writeLines(fill_template("model-template.R"), r_file)
  writeLines(fill_template("model-test-template.R"), test_file)

  message(
    "Created ", r_file, " and ", test_file, ".\n",
    "Next: implement cm_predict.", name, "() and cm_loglik.", name, "(), ",
    "then flesh out the simulate-and-recover test."
  )
  invisible(c(r_file, test_file))
}

#' Scaffold a new dataset
#'
#' Creates `inst/extdata/<id>/<id>.csv` (copied from `data_path`, if given)
#' and a matching `<id>.yaml` metadata sidecar for you to fill in. See
#' [validate_cogdata()] for the required fields.
#'
#' @param id Dataset identifier, e.g. `"nosofsky_1989_responses"`. Becomes
#'   the subdirectory and file basename under `inst/extdata/`.
#' @param data_path Path to an existing `.csv` file to copy in; if `NULL`,
#'   only the metadata sidecar is created and you add the `.csv` yourself.
#' @param path Package root to scaffold into (default: current directory).
#' @return Invisibly, the paths of the files created.
#' @export
add_dataset <- function(id, data_path = NULL, path = ".") {
  dir <- file.path(path, "inst", "extdata", id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  csv_file <- file.path(dir, paste0(id, ".csv"))
  yaml_file <- file.path(dir, paste0(id, ".yaml"))
  if (file.exists(yaml_file)) stop(yaml_file, " already exists.", call. = FALSE)

  if (!is.null(data_path)) file.copy(data_path, csv_file)

  sidecar <- readLines(system.file("templates", "dataset-sidecar-template.yaml", package = "cogzoo"))
  sidecar <- gsub("{{id}}", id, sidecar, fixed = TRUE)
  writeLines(sidecar, yaml_file)

  message(
    "Created ", yaml_file, ".\n",
    "Next: fill in its TODO fields", if (is.null(data_path)) paste0(" and add ", csv_file) else "",
    ", then run validate_cogdata('", csv_file, "')."
  )
  invisible(c(csv_file, yaml_file))
}
