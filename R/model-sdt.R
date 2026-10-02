#' Equal-variance Gaussian Signal Detection Theory
#'
#' The field's baseline model for a single old/new (or signal/noise)
#' recognition judgment: signal and noise familiarity are each normally
#' distributed with unit variance and means `dprime` apart, and the
#' observer says "old"/"signal" when familiarity exceeds a criterion.
#' Parameterized as in Macmillan & Creelman's *Detection Theory*, with the
#' criterion placed relative to the midpoint between the two distributions
#' (`criterion = 0` is unbiased).
#'
#' @param dprime Discriminability (distance between the signal and noise distribution means); >= 0.
#' @param criterion Response criterion relative to the midpoint between the two distributions; unbounded, positive = more conservative.
#' @return A `cogmodel` object of subclass `"sdt"`.
#' @export
sdt <- function(dprime = 1, criterion = 0) {
  new_cogmodel(
    params = list(dprime = dprime, criterion = criterion),
    domain = "memory",
    task_types = "recognition-memory",
    subclass = "sdt"
  )
}

#' @param model An `sdt` model object.
#' @param ... Unused.
#' @return A named numeric vector `c(p_hit, p_fa)`.
#' @rdname sdt
#' @export
cm_predict.sdt <- function(model, ...) {
  d <- model$params$dprime
  c <- model$params$criterion
  c(
    p_hit = stats::pnorm(d / 2 - c),
    p_fa  = stats::pnorm(-d / 2 - c)
  )
}

#' @param data Data frame/list with `n_hit`/`n_signal` and `n_fa`/`n_noise`
#'   (one row per subject or condition; log-likelihoods are summed across rows).
#' @rdname sdt
#' @export
cm_loglik.sdt <- function(model, data, ...) {
  pred <- cm_predict(model)
  sum(
    stats::dbinom(data$n_hit, data$n_signal, pred["p_hit"], log = TRUE),
    stats::dbinom(data$n_fa, data$n_noise, pred["p_fa"], log = TRUE)
  )
}

#' @param n_signal,n_noise Number of signal and noise trials to simulate.
#' @rdname sdt
#' @export
cm_simulate.sdt <- function(model, n_signal, n_noise, ...) {
  pred <- cm_predict(model)
  data.frame(
    n_hit = stats::rbinom(1, n_signal, pred["p_hit"]), n_signal = n_signal,
    n_fa  = stats::rbinom(1, n_noise, pred["p_fa"]),   n_noise  = n_noise
  )
}

register_model(
  name = "sdt",
  domain = "memory",
  task_types = "recognition-memory",
  constructor = sdt,
  description = "Equal-variance Gaussian signal detection: hit/false-alarm rates from d' and a response criterion.",
  citation = "Green, D. M., & Swets, J. A. (1966). Signal detection theory and psychophysics. Wiley."
)
