`%||%` <- function(x, y) if (is.null(x)) y else x

#' Validate a bundled dataset against its metadata sidecar
#'
#' Every dataset file `<name>.csv` must ship with a `<name>.yaml` sidecar
#' declaring, at minimum, `id`, `title`, `citation`, `license`, `domain`,
#' `task_type`, and `columns` (a list of `name`/`type`/`description`
#' entries). This function checks the sidecar has those fields and that the
#' declared columns actually exist in the data file — it's the check that
#' runs in CI over every file under `inst/extdata/` so a malformed
#' contribution fails fast instead of silently shipping.
#'
#' @param path Path to the dataset's `.csv` file.
#' @return Invisibly, a list with `data` (a data frame) and `meta` (the
#'   parsed sidecar).
#' @export
validate_cogdata <- function(path) {
  meta_path <- sub("\\.csv$", ".yaml", path)
  if (!file.exists(meta_path)) {
    stop("Missing metadata sidecar: ", meta_path, call. = FALSE)
  }

  meta <- yaml::read_yaml(meta_path)
  required_fields <- c("id", "title", "citation", "license", "domain", "task_type", "columns")
  missing_fields <- setdiff(required_fields, names(meta))
  if (length(missing_fields) > 0) {
    stop(
      "Metadata sidecar ", meta_path, " is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  data <- utils::read.csv(path, sep = meta$sep %||% ",", stringsAsFactors = FALSE, check.names = FALSE)
  declared_cols <- vapply(meta$columns, function(col) col$name, character(1))
  missing_cols <- setdiff(declared_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Data file ", path, " is missing column(s) declared in its sidecar: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(list(data = data, meta = meta))
}

#' Read a bundled dataset, validating it against its sidecar first
#' @param path Path to the dataset's `.csv` file.
#' @return A data frame.
#' @export
read_cogdata <- function(path) {
  validate_cogdata(path)$data
}
