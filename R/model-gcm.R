#' Generalized Context Model (Nosofsky, 1986)
#'
#' Exemplar-based categorization model: an item's category-1 response
#' probability is the similarity-weighted proportion of category-1
#' exemplars in memory. Ported to the `cogzoo` interface from the
#' original `gcm_pred()`/`gcm_fit()` in the `gcm/` folder of the
#' `cognitive_models` repo; the math (Eqs. 2-4 of Nosofsky, 1989) is
#' unchanged.
#'
#' @param w1 Attention weight on dimension 1 of the (2D) psychological space; in \[0, 1\].
#' @param sensitivity Similarity sensitivity ("c" in Nosofsky's notation); > 0.
#' @param bias Response bias toward category 1; in \[0, 1\].
#' @param rho Distance metric: 1 = city-block, 2 = Euclidean.
#' @param p Similarity gradient: 1 = exponential, 2 = Gaussian.
#' @return A `cogmodel` object of subclass `"gcm"`.
#' @export
gcm <- function(w1 = 0.5, sensitivity = 1, bias = 0.5, rho = 2, p = 1) {
  new_cogmodel(
    params = list(w1 = w1, sensitivity = sensitivity, bias = bias),
    domain = "categorization",
    task_types = "binary-classification",
    subclass = "gcm",
    rho = rho, p_metric = p
  )
}

#' @param model A `gcm` model object.
#' @param newdata Data frame/matrix of probe coordinates (one row per item, one column per dimension).
#' @param mem Data frame/matrix of exemplars in memory: dimension columns followed by a `category` column (1 or 2).
#' @param ... Unused.
#' @rdname gcm
#' @export
cm_predict.gcm <- function(model, newdata, mem, ...) {
  w <- c(model$params$w1, 1 - model$params$w1)
  sensitivity <- model$params$sensitivity
  bias <- model$params$bias

  mem <- as.matrix(mem)
  obs <- as.matrix(newdata)
  n_dim <- ncol(obs)

  vapply(seq_len(nrow(obs)), function(i) {
    probe <- obs[i, seq_len(n_dim)]
    d <- w * abs(probe - t(mem[, seq_len(n_dim), drop = FALSE]))^model$rho
    d <- colSums(d)^(1 / model$rho)                 # Eq. 3, Nosofsky (1988)
    s <- exp(-sensitivity * d^model$p_metric)        # Eq. 4, Nosofsky (1989)
    s_cat1 <- sum(s[mem[, n_dim + 1] == 1])
    s_cat2 <- sum(s[mem[, n_dim + 1] == 2])
    bias * s_cat1 / (bias * s_cat1 + (1 - bias) * s_cat2)  # Eq. 2, Nosofsky (1989)
  }, numeric(1))
}

#' @param data Data frame with one row per probe: dimension columns, `n_cat1`
#'   (observed category-1 responses), and `n_total` (total responses).
#' @rdname gcm
#' @export
cm_loglik.gcm <- function(model, data, mem, ...) {
  probe_cols <- setdiff(names(data), c("n_cat1", "n_total"))
  pred <- cm_predict(model, data[probe_cols], mem = mem)
  sum(stats::dbinom(data$n_cat1, data$n_total, pred, log = TRUE))
}

#' @param n Number of trials to simulate per row of `newdata`.
#' @rdname gcm
#' @export
cm_simulate.gcm <- function(model, newdata, mem, n = 1, ...) {
  pred <- cm_predict(model, newdata, mem = mem)
  stats::rbinom(length(pred), n, pred)
}

register_model(
  name = "gcm",
  domain = "categorization",
  task_types = "binary-classification",
  constructor = gcm,
  description = "Exemplar-based categorization via similarity to stored exemplars.",
  citation = paste(
    "Nosofsky, R. M. (1986). Attention, similarity, and the",
    "identification-categorization relationship.",
    "Journal of Experimental Psychology: General, 115(1), 39-57."
  )
)
