#' Generalized Context Model for old/new recognition (Shin & Nosofsky, 1992)
#'
#' The GCM's recognition variant: instead of comparing similarity to
#' exemplars of two competing categories, the probe's summed similarity to
#' *all* studied exemplars gives its familiarity `f` (Eq. 8), and the
#' probability of an "old" response is `f / (f + k)` (Eq. 9), with `k` a
#' response-criterion parameter. Ported from `gcm/gcm_rec_pred.r` and
#' `gcm/gcm_rec_fit.r` in `crsh/cognitive_models`, generalized from a
#' hard-coded 6 dimensions to any number; the math is unchanged. Pairs with
#' the bundled `shin_nosofsky1992_*` datasets.
#'
#' The last attention weight is not free: it is `1 - sum(weights)`. The
#' original code enforced non-negative weights summing to at most 1 with
#' `constrOptim()`; here [cm_loglik.gcm_recognition()] returns `-Inf` for
#' parameter values outside that region, which the default [cm_fit()] treats
#' as a barrier, so fitting works with the default Nelder-Mead optimizer.
#' Nelder-Mead is slow to converge on this 7-parameter model (a single
#' `cm_fit()` call typically stops at the iteration limit, as the original
#' noted for its estimates): feed each fit back in as the next starting
#' model until `convergence` is 0. Six restarts from the defaults reproduce
#' Shin & Nosofsky's (1992, Table 5) Experiment 1 estimates (see the tests).
#'
#' There is no deterministic-response mode (the original's `pred = "single"`):
#' a thresholded response has no likelihood to fit by.
#'
#' @param weights Numeric vector of attention weights for dimensions
#'   `1..(n_dim - 1)`; the final dimension's weight is `1 - sum(weights)`.
#'   Its length sets the number of dimensions (default: 5, i.e. 6 dimensions).
#' @param sensitivity Similarity sensitivity ("c"); > 0.
#' @param criterion Response-criterion parameter ("k"); > 0, larger = fewer "old" responses.
#' @param rho Distance metric: 1 = city-block, 2 = Euclidean.
#' @param p Similarity gradient: 1 = exponential, 2 = Gaussian.
#' @return A `cogmodel` object of subclass `"gcm_recognition"`.
#' @export
gcm_recognition <- function(weights = rep(1 / 6, 5), sensitivity = 3, criterion = 1, rho = 2, p = 1) {
  params <- c(
    stats::setNames(as.list(weights), paste0("w", seq_along(weights))),
    list(sensitivity = sensitivity, criterion = criterion)
  )
  new_cogmodel(
    params = params,
    domain = "memory",
    task_types = "recognition-memory",
    subclass = "gcm_recognition",
    n_weights = length(weights), rho = rho, p_metric = p
  )
}

grec_weights <- function(model) {
  w <- unlist(model$params[paste0("w", seq_len(model$n_weights))])
  c(w, 1 - sum(w))
}

#' @param model A `gcm_recognition` model object.
#' @param newdata Data frame/matrix of probe coordinates (one row per probe, one column per dimension).
#' @param mem Data frame/matrix of studied exemplars' coordinates (one row per exemplar, same columns as `newdata`).
#' @param ... Unused.
#' @return Numeric vector: the probability of an "old" response to each probe.
#' @rdname gcm_recognition
#' @export
cm_predict.gcm_recognition <- function(model, newdata, mem, ...) {
  w <- grec_weights(model)
  mem <- as.matrix(mem)
  obs <- as.matrix(newdata)

  vapply(seq_len(nrow(obs)), function(i) {
    d <- w * abs(obs[i, ] - t(mem))^model$rho
    d <- colSums(d)^(1 / model$rho)                     # Eq. 3, Nosofsky (1988)
    s <- exp(-model$params$sensitivity * d^model$p_metric)
    f <- sum(s)                                          # Eq. 8, Shin & Nosofsky (1992)
    f / (f + model$params$criterion)                     # Eq. 9, Shin & Nosofsky (1992)
  }, numeric(1))
}

#' @param data Data frame with one row per probe: coordinate columns, `n_old`
#'   (observed "old" responses), and `n_total` (total responses).
#' @rdname gcm_recognition
#' @export
cm_loglik.gcm_recognition <- function(model, data, mem, ...) {
  w <- grec_weights(model)
  if (any(w < 0) || model$params$sensitivity <= 0 || model$params$criterion <= 0) return(-Inf)

  probe_cols <- setdiff(names(data), c("n_old", "n_total"))
  pred <- cm_predict(model, data[probe_cols], mem = mem)
  sum(stats::dbinom(data$n_old, data$n_total, pred, log = TRUE))
}

#' @param n Number of trials to simulate per row of `newdata`.
#' @rdname gcm_recognition
#' @export
cm_simulate.gcm_recognition <- function(model, newdata, mem, n = 1, ...) {
  pred <- cm_predict(model, newdata, mem = mem)
  stats::rbinom(length(pred), n, pred)
}

register_model(
  name = "gcm_recognition",
  domain = "memory",
  task_types = "recognition-memory",
  constructor = gcm_recognition,
  description = "GCM for old/new recognition: summed similarity to studied exemplars sets familiarity.",
  citation = paste(
    "Shin, H. J., & Nosofsky, R. M. (1992). Similarity-scaling studies of",
    "dot-pattern classification and recognition. Journal of Experimental",
    "Psychology: General, 121(3), 278-304."
  )
)
