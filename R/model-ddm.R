#' Drift Diffusion Model (Ratcliff, 1978)
#'
#' The field-standard two-choice RT model: evidence accumulates as a
#' Wiener process from a starting point toward one of two boundaries, with
#' drift rate, boundary separation, starting point, and non-decision time
#' as free parameters. Unlike [ebddm()] (where drift is *derived* from
#' GCM similarity to a memory set), here drift is itself a free parameter —
#' this is the general-purpose version most papers mean by "the DDM."
#'
#' [cm_predict.ddm()] is dependency-free: it returns the closed-form
#' upper-boundary hit probability (Bogacz et al., 2006), which only needs
#' base R. [cm_simulate.ddm()] and [cm_loglik.ddm()] need the Suggested
#' package \pkg{RWiener} for full RT-distribution simulation/likelihood (see
#' [ebddm()] for why this is a Suggests dependency, not an Import).
#'
#' @param drift Drift rate; unbounded, sign gives the favored boundary.
#' @param boundary_sep Boundary separation ("a"); > 0.
#' @param ndt Non-decision time ("tau"); >= 0.
#' @param bias Starting point as a fraction of boundary separation ("beta"); in \[0, 1\], 0.5 = unbiased.
#' @return A `cogmodel` object of subclass `"ddm"`.
#' @export
ddm <- function(drift = 0, boundary_sep = 1, ndt = 0.2, bias = 0.5) {
  new_cogmodel(
    params = list(drift = drift, boundary_sep = boundary_sep, ndt = ndt, bias = bias),
    domain = "decision-making",
    task_types = "speeded-binary-classification",
    subclass = "ddm"
  )
}

#' @param model A `ddm` model object.
#' @param ... Unused.
#' @return The probability of terminating at the upper boundary (closed form; Bogacz et al., 2006, Eq. 5).
#' @rdname ddm
#' @export
cm_predict.ddm <- function(model, ...) {
  v <- model$params$drift
  a <- model$params$boundary_sep
  z <- model$params$bias * a

  if (v == 0) {
    p_upper <- z / a
  } else {
    p_upper <- (1 - exp(-2 * v * z)) / (1 - exp(-2 * v * a))
  }

  c(p_upper = p_upper)
}

#' @param n_trials Number of trials to simulate.
#' @rdname ddm
#' @export
cm_simulate.ddm <- function(model, n_trials = 100, ...) {
  if (!requireNamespace("RWiener", quietly = TRUE)) {
    stop("Package 'RWiener' is required to simulate from ddm. Install it with install.packages('RWiener').", call. = FALSE)
  }
  RWiener::rwiener(n_trials, model$params$boundary_sep, model$params$ndt, model$params$bias, model$params$drift)
}

#' @param data Data frame of trial-level responses in RWiener's convention: `q` (response time) and `resp` (`"upper"`/`"lower"`).
#' @rdname ddm
#' @export
cm_loglik.ddm <- function(model, data, ...) {
  if (!requireNamespace("RWiener", quietly = TRUE)) {
    stop("Package 'RWiener' is required for ddm's likelihood. Install it with install.packages('RWiener').", call. = FALSE)
  }
  sum(log(RWiener::dwiener(
    data$q, model$params$boundary_sep, model$params$ndt, model$params$bias, model$params$drift,
    resp = data$resp
  )))
}

register_model(
  name = "ddm",
  domain = "decision-making",
  task_types = "speeded-binary-classification",
  constructor = ddm,
  description = "Free-parameter drift diffusion model of two-choice RT and accuracy.",
  citation = "Ratcliff, R. (1978). A theory of memory retrieval. Psychological Review, 85(2), 59-108."
)
