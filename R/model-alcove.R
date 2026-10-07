#' ALCOVE (Kruschke, 1992)
#'
#' An exemplar-based connectionist model of category learning: stimuli
#' activate hidden "exemplar" nodes by similarity (as in [gcm()]), which
#' feed category output nodes through associative weights learned by error
#' correction, while selective attention to each stimulus dimension is also
#' learned. This is a thin wrapper around [catlearn::slpALCOVE()] from the
#' Suggested package \pkg{catlearn}, which provides the actively maintained,
#' independently validated implementation (see the README's Related work);
#' `alcove()` adds only the shared `cogzoo` interface: a trial sequence in,
#' response probabilities out, a likelihood for fitting, and resets between
#' simulated or real participants.
#'
#' Because ALCOVE learns trial by trial, its predictions depend on the
#' *order* of the training trials, so every method takes the whole trial
#' sequence. Learning is driven by the true category feedback, not by the
#' model's own responses, so [cm_simulate.alcove()] samples responses from
#' the predicted probabilities without feeding them back.
#'
#' By default there is one hidden node at each distinct stimulus location
#' in `x` (the usual exemplar-node setup for small stimulus sets); pass
#' `hidden` to override.
#'
#' @param c Specificity (similarity sensitivity); > 0.
#' @param phi Decision/mapping constant; > 0.
#' @param lw Associative (output-weight) learning rate; in (0, 1].
#' @param la Attention learning rate; in (0, 1].
#' @param r Distance metric: 1 = city-block, 2 = Euclidean.
#' @param q Similarity gradient: 1 = exponential, 2 = Gaussian.
#' @param dec Decision rule: `"ER"` (exponential ratio, Kruschke 1992) or
#'   `"BN"` (background noise ratio, Nosofsky et al. 1994).
#' @param humble Use a humble (rather than strict) teacher.
#' @param attcon Constrain the attention weights to sum to 1, as Nosofsky et al. (1994) did.
#' @param absval Teaching value for "category absent" (catlearn's `absval`).
#' @return A `cogmodel` object of subclass `"alcove"`.
#' @export
alcove <- function(c = 1, phi = 1, lw = 0.1, la = 0.1, r = 2, q = 1, dec = "ER", humble = TRUE, attcon = FALSE, absval = -1) {
  new_cogmodel(
    params = list(c = c, phi = phi, lw = lw, la = la),
    domain = "categorization",
    task_types = "category-learning",
    subclass = "alcove",
    r = r, q = q, dec = dec, humble = humble, attcon = attcon, absval = absval
  )
}

#' @param model An `alcove` model object.
#' @param x Matrix/data frame of stimulus coordinates, one row per training trial in presentation order, one column per dimension.
#' @param category Integer vector (1..K), the true category on each trial (the feedback the model learns from).
#' @param subject Optional vector marking independent learners (e.g. a subject ID, sorted so each learner's trials are contiguous): the network resets whenever it changes. If `NULL`, `x` is a single learner's sequence.
#' @param hidden Optional matrix of hidden-node locations, one row per node and one column per dimension; defaults to the distinct rows of `x`.
#' @param ... Unused.
#' @return A matrix of category response probabilities: one row per trial, one column per category.
#' @rdname alcove
#' @export
cm_predict.alcove <- function(model, x, category, subject = NULL, hidden = NULL, ...) {
  cl_require("alcove")
  x <- as.matrix(x)
  n <- nrow(x)
  n_cat <- max(category)
  if (is.null(hidden)) hidden <- unique(x)
  hidden <- as.matrix(hidden)

  teach <- matrix(model$absval, n, n_cat)
  teach[cbind(seq_len(n), category)] <- 1

  tr <- cbind(ctrl = cl_ctrl(n, subject), x, teach, matrix(0, n, ncol(x)))
  st <- list(
    colskip = 1, c = model$params$c, r = model$r, q = model$q, phi = model$params$phi,
    lw = model$params$lw, la = model$params$la,
    h = t(hidden), alpha = rep(1, ncol(x)), w = matrix(0, n_cat, nrow(hidden))
  )
  catlearn::slpALCOVE(st, tr, dec = model$dec, humble = model$humble, attcon = model$attcon, absval = model$absval)$prob
}

#' @param data Data frame with `response`: the category (1..K) the participant chose on each trial, in the same order as `x`.
#' @rdname alcove
#' @export
cm_loglik.alcove <- function(model, data, x, category, subject = NULL, hidden = NULL, ...) {
  p <- model$params
  if (p$c <= 0 || p$phi <= 0 || p$lw <= 0 || p$lw > 1 || p$la <= 0 || p$la > 1) return(-Inf)
  probs <- cm_predict(model, x, category, subject = subject, hidden = hidden)
  if (anyNA(probs)) return(-Inf)
  cl_loglik_response(probs, data$response)
}

#' @rdname alcove
#' @export
cm_simulate.alcove <- function(model, x, category, subject = NULL, hidden = NULL, ...) {
  cl_sample_response(cm_predict(model, x, category, subject = subject, hidden = hidden))
}

register_model(
  name = "alcove",
  domain = "categorization",
  task_types = "category-learning",
  constructor = alcove,
  description = "Exemplar-based connectionist category learning with learned attention (catlearn backend).",
  citation = paste(
    "Kruschke, J. K. (1992). ALCOVE: An exemplar-based connectionist model",
    "of category learning. Psychological Review, 99(1), 22-44."
  )
)
