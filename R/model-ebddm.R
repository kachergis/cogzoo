#' Exemplar-Based Diffusion Model
#'
#' A drift-diffusion extension of the GCM ([gcm()]): the log-odds of
#' similarity-based category membership sets the drift rate of a Wiener
#' diffusion process, predicting the full RT distribution for each
#' response. Ported from `ebddm/ebddm_pred.R`.
#'
#' Requires the Suggested package \pkg{RWiener} for [cm_simulate.ebddm()]
#' and [cm_loglik.ebddm()] (not for [cm_predict.ebddm()], which only
#' computes drift rates); install it with `install.packages("RWiener")`.
#' Keeping heavy, model-specific dependencies as Suggests rather than hard
#' Imports is deliberate — installing `cogzoo` shouldn't require every
#' backend every model might use.
#'
#' @param w1 Attention weight on dimension 1 of the (2D) psychological space; in \[0, 1\].
#' @param sensitivity Similarity sensitivity ("c"); > 0.
#' @param boundary_sep Diffusion boundary separation ("alpha" in RWiener's notation); > 0.
#' @param ndt Non-decision time ("tau"); >= 0.
#' @param bias Starting-point bias ("beta"); in \[0, 1\].
#' @param rho Distance metric: 1 = city-block, 2 = Euclidean.
#' @return A `cogmodel` object of subclass `"ebddm"`.
#' @export
ebddm <- function(w1 = 0.5, sensitivity = 1, boundary_sep = 1, ndt = 0.2, bias = 0.5, rho = 2) {
  new_cogmodel(
    params = list(w1 = w1, sensitivity = sensitivity, boundary_sep = boundary_sep, ndt = ndt, bias = bias),
    domain = "decision-making",
    task_types = "speeded-binary-classification",
    subclass = "ebddm",
    rho = rho
  )
}

#' @param model An `ebddm` model object.
#' @param newdata Data frame of probe coordinates (one row per probe or per trial; repeat rows for repeated probes).
#' @param mem Data frame/matrix of exemplars in memory: dimension columns followed by a `category` column (1 or 2).
#' @param ... Unused.
#' @rdname ebddm
#' @export
cm_predict.ebddm <- function(model, newdata, mem, ...) {
  w <- c(model$params$w1, 1 - model$params$w1)
  sensitivity <- model$params$sensitivity

  mem <- as.matrix(mem)
  obs <- as.matrix(newdata)
  n_dim <- ncol(obs)

  drift <- vapply(seq_len(nrow(obs)), function(i) {
    probe <- obs[i, seq_len(n_dim)]
    d <- w * abs(probe - t(mem[, seq_len(n_dim), drop = FALSE]))^model$rho
    d <- colSums(d)^(1 / model$rho)                # Eq. 3, Nosofsky (1988)
    s <- exp(-sensitivity * d)                      # Eq. 4, Nosofsky (1989)
    p <- sum(s[mem[, n_dim + 1] == 1]) / sum(s)     # Eq. 2, Nosofsky (1989)
    log(p / (1 - p))
  }, numeric(1))

  data.frame(newdata, drift = drift)
}

#' @param n_trials Number of trials to simulate per row of `newdata`.
#' @rdname ebddm
#' @export
cm_simulate.ebddm <- function(model, newdata, mem, n_trials = 100, ...) {
  if (!requireNamespace("RWiener", quietly = TRUE)) {
    stop("Package 'RWiener' is required to simulate from ebddm. Install it with install.packages('RWiener').", call. = FALSE)
  }
  pred <- cm_predict(model, newdata, mem = mem)
  alpha <- model$params$boundary_sep
  tau <- model$params$ndt
  beta <- model$params$bias

  do.call(rbind, lapply(seq_len(nrow(pred)), function(i) {
    cbind(stimulus = i, RWiener::rwiener(n_trials, alpha, tau, beta, pred$drift[i]))
  }))
}

#' @param data Data frame of trial-level responses in RWiener's convention:
#'   probe coordinate columns, `q` (response time), and `resp` (`"upper"`/`"lower"`).
#' @rdname ebddm
#' @export
cm_loglik.ebddm <- function(model, data, mem, ...) {
  if (!requireNamespace("RWiener", quietly = TRUE)) {
    stop("Package 'RWiener' is required for ebddm's likelihood. Install it with install.packages('RWiener').", call. = FALSE)
  }
  probe_cols <- setdiff(names(data), c("q", "resp"))
  pred <- cm_predict(model, data[probe_cols], mem = mem)
  alpha <- model$params$boundary_sep
  tau <- model$params$ndt
  beta <- model$params$bias
  sum(log(RWiener::dwiener(data$q, alpha, tau, beta, pred$drift, resp = data$resp)))
}

register_model(
  name = "ebddm",
  domain = "decision-making",
  task_types = "speeded-binary-classification",
  constructor = ebddm,
  description = "Drift-diffusion extension of the GCM: similarity sets the diffusion drift rate.",
  citation = paste(
    "Ratcliff, R., & Smith, P. L. (2004). A comparison of sequential sampling",
    "models for two-choice reaction time. Psychological Review, 111(2), 333-367."
  )
)
