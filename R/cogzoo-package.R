#' cogzoo: A Community Zoo of Computational Cognitive Models
#'
#' Every model in the zoo is an object of class `cogmodel` (plus a subclass
#' for the specific model, e.g. `gcm`) and implements a subset of the
#' generics [cm_simulate()], [cm_loglik()], [cm_fit()], and [cm_predict()].
#' Datasets ship under `inst/extdata/<dataset_id>/` alongside a YAML sidecar
#' validated by [validate_cogdata()]. See `vignette("contributing")` for how
#' to add a new model or dataset.
#'
#' @keywords internal
"_PACKAGE"
