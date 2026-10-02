.cogzoo_registry <- new.env(parent = emptyenv())

#' Register a model with the zoo
#'
#' Call this once at the top level of a model's source file (it runs when
#' the package loads). This is what makes a model discoverable via
#' [list_models()] and is checked by the package's own test suite, so a
#' model that forgets to register itself fails CI rather than going quietly
#' unlisted.
#'
#' @param name Unique model identifier, e.g. `"gcm"`. Should match the
#'   constructor function name.
#' @param domain One of the zoo's domains, e.g. `"categorization"`, `"memory"`,
#'   `"learning"`, `"decision-making"` (new domains are welcome; keep the
#'   string consistent with any existing models in that area).
#' @param task_types Character vector of task types the model applies to.
#' @param constructor The model's constructor function (e.g. `gcm`).
#' @param description One-sentence description.
#' @param citation Citation for the original model.
#' @export
register_model <- function(name, domain, task_types, constructor, description = "", citation = "") {
  assign(
    name,
    list(
      name = name, domain = domain, task_types = task_types,
      constructor = constructor, description = description, citation = citation
    ),
    envir = .cogzoo_registry
  )
  invisible(NULL)
}

#' List registered models
#'
#' @param domain Optional domain to filter by (see [register_model()]).
#' @return A named list of model registry entries.
#' @export
list_models <- function(domain = NULL) {
  models <- as.list(.cogzoo_registry)
  if (!is.null(domain)) {
    models <- Filter(function(m) identical(m$domain, domain), models)
  }
  models
}

#' Look up a registered model's metadata
#' @param name Model identifier, as passed to [register_model()].
#' @export
model_info <- function(name) {
  if (!exists(name, envir = .cogzoo_registry, inherits = FALSE)) {
    stop("Unknown model: '", name, "'. See list_models() for what's registered.", call. = FALSE)
  }
  get(name, envir = .cogzoo_registry)
}
