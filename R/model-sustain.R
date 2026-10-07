#' SUSTAIN (Love, Medin, & Gureckis, 2004)
#'
#' An adaptive clustering model of category learning: the model starts with
#' one cluster and recruits a new one, centered on the current stimulus,
#' whenever its category prediction is wrong; clusters compete, dimensions
#' get learned attention weights, and the winning cluster's connections to
#' the category units are learned by error correction. This is a thin
#' wrapper around [catlearn::slpSUSTAIN()] from the Suggested package
#' \pkg{catlearn}, which provides the maintained, validated implementation;
#' `sustain()` adds only the shared `cogzoo` interface (see [alcove()] for
#' the same design on the other catlearn model).
#'
#' Only supervised learning is wrapped (catlearn itself marks unsupervised
#' SUSTAIN as early-stage), so the unsupervised threshold `tau` is fixed at
#' 0. Predictions depend on trial order and on the model's own recruitment
#' history, so every method takes the whole trial sequence, and learning is
#' driven by true category feedback rather than by the model's responses.
#'
#' Ties between equally activated clusters are broken by recruitment order
#' (`ties = "first"`) by default, not at random: random tie-breaking would
#' make the likelihood differ from call to call and break optimizers. Set
#' `ties = "random"` to match catlearn's current default.
#'
#' SUSTAIN's decision consistency `d` is typically large, so predicted
#' probabilities are often near 0 or 1; [cm_loglik.sustain()] floors them at
#' 1e-10 so a single confident miss doesn't make the likelihood `-Inf`.
#'
#' @param r Attentional focus; >= 0.
#' @param beta Cluster competition; >= 0.
#' @param d Decision consistency; >= 0.
#' @param eta Learning rate; in \[0, 1\].
#' @param ties How ties between equally activated clusters are resolved: `"first"` or `"random"`.
#' @return A `cogmodel` object of subclass `"sustain"`.
#' @export
sustain <- function(r = 9, beta = 1.25, d = 16, eta = 0.09, ties = "first") {
  new_cogmodel(
    params = list(r = r, beta = beta, d = d, eta = eta),
    domain = "categorization",
    task_types = "category-learning",
    subclass = "sustain",
    ties = ties
  )
}

# One-hot ("padded") encoding of nominal levels, one block of columns per dimension.
sustain_pad <- function(levels, n_levels) {
  do.call(cbind, lapply(seq_along(n_levels), function(j) {
    block <- matrix(0, length(levels[[j]]), n_levels[j])
    block[cbind(seq_along(levels[[j]]), levels[[j]])] <- 1
    block
  }))
}

#' @param model A `sustain` model object.
#' @param x Matrix/data frame of integer nominal levels (1, 2, ...) for each stimulus dimension: one row per training trial in presentation order, one column per dimension.
#' @param category Integer vector (1..K), the true category on each trial (the feedback the model learns from).
#' @param subject Optional vector marking independent learners (e.g. a subject ID, sorted so each learner's trials are contiguous): the network resets whenever it changes. If `NULL`, `x` is a single learner's sequence.
#' @param dims Optional integer vector giving the number of levels of each stimulus dimension; defaults to the largest level seen in each column of `x`.
#' @param ... Unused.
#' @return A matrix of category response probabilities: one row per trial, one column per category.
#' @rdname sustain
#' @export
cm_predict.sustain <- function(model, x, category, subject = NULL, dims = NULL, ...) {
  cl_require("sustain")
  x <- as.matrix(x)
  n <- nrow(x)
  n_cat <- max(category)
  if (is.null(dims)) dims <- apply(x, 2, max)

  input <- sustain_pad(c(lapply(seq_len(ncol(x)), function(j) x[, j]), list(category)), c(dims, n_cat))
  tr <- cbind(ctrl = cl_ctrl(n, subject), input)
  st <- list(
    r = model$params$r, beta = model$params$beta, d = model$params$d, eta = model$params$eta,
    tau = 0, lambda = rep(1, ncol(x)), dims = dims, cluster = NA, w = NA, colskip = 1
  )
  catlearn::slpSUSTAIN(st, tr, ties = model$ties)$probs
}

#' @param data Data frame with `response`: the category (1..K) the participant chose on each trial, in the same order as `x`.
#' @rdname sustain
#' @export
cm_loglik.sustain <- function(model, data, x, category, subject = NULL, dims = NULL, ...) {
  p <- model$params
  if (p$r < 0 || p$beta < 0 || p$d < 0 || p$eta < 0 || p$eta > 1) return(-Inf)
  probs <- cm_predict(model, x, category, subject = subject, dims = dims)
  if (anyNA(probs)) return(-Inf)
  cl_loglik_response(probs, data$response)
}

#' @rdname sustain
#' @export
cm_simulate.sustain <- function(model, x, category, subject = NULL, dims = NULL, ...) {
  cl_sample_response(cm_predict(model, x, category, subject = subject, dims = dims))
}

register_model(
  name = "sustain",
  domain = "categorization",
  task_types = "category-learning",
  constructor = sustain,
  description = "Adaptive clustering model of category learning with learned attention (catlearn backend).",
  citation = paste(
    "Love, B. C., Medin, D. L., & Gureckis, T. M. (2004). SUSTAIN: A network",
    "model of category learning. Psychological Review, 111(2), 309-332."
  )
)
