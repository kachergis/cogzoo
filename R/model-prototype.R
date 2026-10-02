#' Prototype model of categorization
#'
#' Categories are represented by a single prototype (typically the mean of
#' their training exemplars) rather than every stored exemplar; an item's
#' category-1 response probability is the similarity-weighted comparison of
#' the probe to each category's prototype. Uses the same similarity/choice
#' machinery as [gcm()] (Eqs. 2-4 of Nosofsky, 1989) — the prototype vs.
#' exemplar distinction is entirely about *what's stored* (one summary point
#' per category vs. every exemplar), not the similarity or choice rule, so
#' comparing `prototype()` and `gcm()` fits on the same data is the
#' textbook way to ask which representation a dataset favors.
#'
#' @param w1 Attention weight on dimension 1 of the (2D) psychological space; in \[0, 1\].
#' @param sensitivity Similarity sensitivity ("c"); > 0.
#' @param bias Response bias toward category 1; in \[0, 1\].
#' @param rho Distance metric: 1 = city-block, 2 = Euclidean.
#' @param p Similarity gradient: 1 = exponential, 2 = Gaussian.
#' @return A `cogmodel` object of subclass `"prototype"`.
#' @export
prototype <- function(w1 = 0.5, sensitivity = 1, bias = 0.5, rho = 2, p = 1) {
  new_cogmodel(
    params = list(w1 = w1, sensitivity = sensitivity, bias = bias),
    domain = "categorization",
    task_types = "binary-classification",
    subclass = "prototype",
    rho = rho, p_metric = p
  )
}

#' @param model A `prototype` model object.
#' @param newdata Data frame/matrix of probe coordinates (one row per item, one column per dimension).
#' @param prototypes Data frame/matrix, one row per category: dimension columns followed by a `category` column (1 or 2) — typically exactly one row per category.
#' @param ... Unused.
#' @rdname prototype
#' @export
cm_predict.prototype <- function(model, newdata, prototypes, ...) {
  w <- c(model$params$w1, 1 - model$params$w1)
  sensitivity <- model$params$sensitivity
  bias <- model$params$bias

  prototypes <- as.matrix(prototypes)
  obs <- as.matrix(newdata)
  n_dim <- ncol(obs)

  vapply(seq_len(nrow(obs)), function(i) {
    probe <- obs[i, seq_len(n_dim)]
    d <- w * abs(probe - t(prototypes[, seq_len(n_dim), drop = FALSE]))^model$rho
    d <- colSums(d)^(1 / model$rho)
    s <- exp(-sensitivity * d^model$p_metric)
    s_cat1 <- sum(s[prototypes[, n_dim + 1] == 1])
    s_cat2 <- sum(s[prototypes[, n_dim + 1] == 2])
    bias * s_cat1 / (bias * s_cat1 + (1 - bias) * s_cat2)
  }, numeric(1))
}

#' @param data Data frame with one row per probe: dimension columns, `n_cat1`
#'   (observed category-1 responses), and `n_total` (total responses).
#' @rdname prototype
#' @export
cm_loglik.prototype <- function(model, data, prototypes, ...) {
  probe_cols <- setdiff(names(data), c("n_cat1", "n_total"))
  pred <- cm_predict(model, data[probe_cols], prototypes = prototypes)
  sum(stats::dbinom(data$n_cat1, data$n_total, pred, log = TRUE))
}

#' @param n Number of trials to simulate per row of `newdata`.
#' @rdname prototype
#' @export
cm_simulate.prototype <- function(model, newdata, prototypes, n = 1, ...) {
  pred <- cm_predict(model, newdata, prototypes = prototypes)
  stats::rbinom(length(pred), n, pred)
}

register_model(
  name = "prototype",
  domain = "categorization",
  task_types = "binary-classification",
  constructor = prototype,
  description = "Categorization by similarity to a single stored prototype per category, rather than to every exemplar.",
  citation = paste(
    "Reed, S. K. (1972). Pattern recognition and categorization.",
    "Cognitive Psychology, 3(3), 382-407."
  )
)
