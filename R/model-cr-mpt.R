#' Conjoint Recognition MPT
#'
#' A three-parameter multinomial processing tree for the conjoint
#' recognition paradigm (Brainerd, Reyna, & Mojardin, 1999): an "old"
#' response can arise from verbatim memory for the exact item (`v`), gist
#' memory for its meaning triggering a related-item response (`g`), or
#' guessing (`b`). Ported from the aggregate data-generating process in
#' `cr-mpt/aggregated_cr_model.R` (a JAGS model) into a directly
#' maximum-likelihood-fittable form.
#'
#' The original file is a *hierarchical* Bayesian model estimating
#' per-subject `V`/`G`/`b` via JAGS. This port fits the pooled,
#' aggregate-count version instead — no JAGS dependency needed, since the
#' response probabilities have closed form. The original JAGS files are
#' preserved under `inst/jags/` for anyone who wants to add a hierarchical
#' `cm_fit` method later (see `inst/jags/README.md`).
#'
#' @param v Verbatim memory probability (recognizing the exact item); in \[0, 1\].
#' @param g Gist memory probability (recognizing the item's meaning/category); in \[0, 1\].
#' @param b Guessing bias toward an "old" response; in \[0, 1\].
#' @return A `cogmodel` object of subclass `"cr_mpt"`.
#' @export
cr_mpt <- function(v = 0.5, g = 0.5, b = 0.5) {
  new_cogmodel(
    params = list(v = v, g = g, b = b),
    domain = "memory",
    task_types = "recognition-memory",
    subclass = "cr_mpt"
  )
}

#' @param model A `cr_mpt` model object.
#' @param ... Unused.
#' @return A named numeric vector `c(p_target, p_lure, p_new)`: the probability
#'   of an "old" response to a target item, a related lure, and an unrelated new item.
#' @rdname cr_mpt
#' @export
cm_predict.cr_mpt <- function(model, ...) {
  v <- model$params$v
  g <- model$params$g
  b <- model$params$b
  c(
    p_target = v + (1 - v) * (g + (1 - g) * b),  # target: verbatim, or gist, or guess
    p_lure   = g + (1 - g) * b,                  # related lure: gist, or guess (no verbatim trace)
    p_new    = b                                 # unrelated new item: guess only
  )
}

#' @param data Data frame/list with `n_target_old`/`n_target_total`,
#'   `n_lure_old`/`n_lure_total`, and `n_new_old`/`n_new_total` (one row per
#'   subject or group; log-likelihoods are summed across rows).
#' @rdname cr_mpt
#' @export
cm_loglik.cr_mpt <- function(model, data, ...) {
  pred <- cm_predict(model)
  sum(
    stats::dbinom(data$n_target_old, data$n_target_total, pred["p_target"], log = TRUE),
    stats::dbinom(data$n_lure_old, data$n_lure_total, pred["p_lure"], log = TRUE),
    stats::dbinom(data$n_new_old, data$n_new_total, pred["p_new"], log = TRUE)
  )
}

#' @param n_target,n_lure,n_new Number of target, related-lure, and new-item trials to simulate.
#' @rdname cr_mpt
#' @export
cm_simulate.cr_mpt <- function(model, n_target, n_lure, n_new, ...) {
  pred <- cm_predict(model)
  data.frame(
    n_target_old = stats::rbinom(1, n_target, pred["p_target"]), n_target_total = n_target,
    n_lure_old   = stats::rbinom(1, n_lure, pred["p_lure"]),     n_lure_total   = n_lure,
    n_new_old    = stats::rbinom(1, n_new, pred["p_new"]),       n_new_total    = n_new
  )
}

register_model(
  name = "cr_mpt",
  domain = "memory",
  task_types = "recognition-memory",
  constructor = cr_mpt,
  description = "Verbatim/gist/guessing tree for target, related-lure, and new-item recognition.",
  citation = paste(
    "Brainerd, C. J., Reyna, V. F., & Mojardin, A. H. (1999). Conjoint",
    "recognition. Psychological Review, 106(1), 160-179."
  )
)
