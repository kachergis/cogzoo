#' TITLE: One-line model name (Author, Year)
#'
#' One-paragraph description of what the model does and what task(s) it applies to.
#'
#' @param param1 Description, bounds, and default.
#' @return A `cogmodel` object of subclass `"{{name}}"`.
#' @export
{{name}} <- function(param1 = 0.5) {
  new_cogmodel(
    params = list(param1 = param1),
    domain = "{{domain}}",
    task_types = "TODO",
    subclass = "{{name}}"
  )
}

#' @param model A `{{name}}` model object.
#' @param newdata Data frame of inputs to predict for.
#' @param ... Unused.
#' @rdname {{name}}
#' @export
cm_predict.{{name}} <- function(model, newdata, ...) {
  stop("TODO: implement cm_predict.{{name}}()")
}

#' @param data Data frame in this model's expected format.
#' @rdname {{name}}
#' @export
cm_loglik.{{name}} <- function(model, data, ...) {
  stop("TODO: implement cm_loglik.{{name}}()")
}

# Delete cm_fit.{{name}} entirely to fall back on the default optim()-based
# fitter (cm_fit.default), which only needs cm_loglik.{{name}} above. Define
# your own method here only if this model needs a custom fitting procedure
# (e.g. a Stan/JAGS backend).

register_model(
  name = "{{name}}",
  domain = "{{domain}}",
  task_types = "TODO",
  constructor = {{name}},
  description = "TODO: one sentence.",
  citation = "TODO: full citation for the original model."
)
