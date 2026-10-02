#' Exemplar-Based Random Walk model (Nosofsky & Palmeri, 1997)
#'
#' A random-walk extension of the GCM ([gcm()]) that predicts choice
#' probability *and* response time by accumulating similarity-driven
#' evidence toward one of two boundaries. Ported from this monorepo's
#' original `ebrw/ebrw_pred.R`, `ebrw/ebrw_fit.R`, and `ebrw/ebrw_sim.R`;
#' the math (Eqs. 2-21 of Nosofsky & Palmeri, 1997) is unchanged.
#'
#' Unlike [gcm()], this model has no `cm_loglik` method: the original
#' `ebrw_fit()` fits summary accuracy and mean RT by a weighted
#' sum-of-squares criterion (Nosofsky & Stanton, 2005), not by maximum
#' likelihood over trial-level responses. [cm_fit.ebrw()] reproduces that
#' criterion directly rather than going through the default
#' optim-over-`cm_loglik` path — a model needs a custom `cm_fit` method
#' whenever "fit" doesn't mean "maximize a likelihood."
#'
#' @param w1 Attention weight on dimension 1 of the (2D) psychological space; in \[0, 1\].
#' @param sensitivity Similarity sensitivity ("c"); > 0.
#' @param boundary Random-walk boundary, symmetric around 0 (A = B in the original notation); > 0.
#' @param alpha Retrieval time constant; >= 0.
#' @param k Response-time scaling constant; > 0.
#' @param mu Residual (non-decision) response time; >= 0.
#' @param rho Distance metric: 1 = city-block, 2 = Euclidean.
#' @return A `cogmodel` object of subclass `"ebrw"`.
#' @export
ebrw <- function(w1 = 0.5, sensitivity = 1, boundary = 5, alpha = 0.1, k = 1, mu = 0.2, rho = 2) {
  new_cogmodel(
    params = list(w1 = w1, sensitivity = sensitivity, boundary = boundary, alpha = alpha, k = k, mu = mu),
    domain = "decision-making",
    task_types = "speeded-binary-classification",
    subclass = "ebrw",
    rho = rho
  )
}

#' @param model An `ebrw` model object.
#' @param newdata Data frame of probe coordinates followed by a `category` column
#'   giving each probe's correct category (1 or 2).
#' @param mem Data frame/matrix of exemplars in memory: dimension columns followed by a `category` column (1 or 2).
#' @param ... Unused.
#' @rdname ebrw
#' @export
cm_predict.ebrw <- function(model, newdata, mem, ...) {
  w <- c(model$params$w1, 1 - model$params$w1)
  sensitivity <- model$params$sensitivity
  A <- B <- model$params$boundary
  alpha <- model$params$alpha
  k <- model$params$k
  mu <- model$params$mu

  mem <- as.matrix(mem)
  obs <- as.matrix(newdata)
  n_dim <- ncol(obs) - 1
  n_obs <- nrow(obs)

  out <- matrix(NA_real_, n_obs, 4, dimnames = list(NULL, c("p_cat1", "rt_cat1", "rt_cat2", "mean_rt")))

  for (i in seq_len(n_obs)) {
    probe <- obs[i, seq_len(n_dim)]
    d <- w * abs(probe - t(mem[, seq_len(n_dim), drop = FALSE]))^model$rho
    d <- colSums(d)^(1 / model$rho)                  # Eq. 3, Nosofsky (1988)
    s <- exp(-sensitivity * d)                        # Eq. 4, Nosofsky (1989)
    s_total <- sum(s[mem[, n_dim + 1] == 1]) + sum(s[mem[, n_dim + 1] == 2])

    p <- sum(s[mem[, n_dim + 1] == 1]) / s_total      # Eq. 2, Nosofsky (1989)
    q <- 1 - p
    t_step <- alpha + 1 / s_total                     # Eq. 10, Nosofsky & Palmeri (1997)

    if (p != 0.5) {
      p_cat1 <- (1 - (q / p)^B) / (1 - (q / p)^(A + B))                       # Eq. 16a

      theta1 <- ((p / q)^(A + B) + 1) / ((p / q)^(A + B) - 1)                 # Eq. 19
      theta2 <- ((p / q)^B + 1) / ((p / q)^B - 1)
      n_step_cat1 <- 1 / (p - q) * (theta1 * (A + B) - theta2 * B)           # Eq. 18a

      theta1 <- ((p / q)^-(A + B) + 1) / ((p / q)^-(A + B) - 1)               # Eq. 21
      theta2 <- ((p / q)^-A + 1) / ((p / q)^-A - 1)
      n_step_cat2 <- 1 / (q - p) * (theta1 * (A + B) - theta2 * A)           # Eq. 20a

      n_steps <- B / (q - p) - (A + B) / (q - p) * ((1 - (q / p)^B) / (1 - (q / p)^(A + B)))  # Eq. 14a
    } else {
      p_cat1 <- B / (A + B)                            # Eq. 16b
      n_step_cat1 <- A / 3 * (2 * B + A)                # Eq. 18b
      n_step_cat2 <- B / 3 * (2 * A + B)                # Eq. 20b
      n_steps <- A * B                                  # Eq. 14b
    }

    out[i, "p_cat1"]  <- p_cat1
    out[i, "rt_cat1"] <- (n_step_cat1 * t_step) * k + mu
    out[i, "rt_cat2"] <- (n_step_cat2 * t_step) * k + mu
    out[i, "mean_rt"] <- (n_steps * t_step) * k + mu
  }

  as.data.frame(out)
}

#' @param n_trials Number of trials to simulate per probe.
#' @rdname ebrw
#' @export
cm_simulate.ebrw <- function(model, newdata, mem, n_trials = 1, ...) {
  w <- c(model$params$w1, 1 - model$params$w1)
  sensitivity <- model$params$sensitivity
  A <- B <- model$params$boundary
  alpha <- model$params$alpha
  k <- model$params$k
  mu <- model$params$mu

  mem <- as.matrix(mem)
  obs <- as.matrix(newdata)
  n_dim <- ncol(obs) - 1
  n_obs <- nrow(obs)

  results <- vector("list", n_obs)

  for (i in seq_len(n_obs)) {
    probe <- obs[i, seq_len(n_dim)]
    d <- w * abs(probe - t(mem[, seq_len(n_dim), drop = FALSE]))^model$rho
    d <- colSums(d)^(1 / model$rho)
    s <- exp(-sensitivity * d)
    s_total <- sum(s[mem[, n_dim + 1] == 1]) + sum(s[mem[, n_dim + 1] == 2])
    p <- sum(s[mem[, n_dim + 1] == 1]) / s_total
    q <- 1 - p
    t_step <- alpha + 1 / s_total

    response <- integer(n_trials)
    rt <- numeric(n_trials)
    for (j in seq_len(n_trials)) {
      rw <- 0L
      n_steps <- 0L
      while (rw < A && rw > -B) {
        rw <- rw + sample(c(1L, -1L), 1, prob = c(p, q))
        n_steps <- n_steps + 1L
      }
      response[j] <- as.integer(rw >= A)
      rt[j] <- (n_steps * t_step) * k + mu
    }
    results[[i]] <- data.frame(stimulus = i, trial = seq_len(n_trials), response = response, rt = rt)
  }

  do.call(rbind, results)
}

#' @param data Data frame with probe coordinates, `category` (correct category),
#'   observed `accuracy`, and observed `mean_rt` — one row per probe.
#' @param method,lower,upper Passed to [stats::optim()].
#' @rdname ebrw
#' @export
cm_fit.ebrw <- function(model, data, mem, method = "Nelder-Mead", lower = -Inf, upper = Inf, ...) {
  skeleton <- model$params
  par0 <- unlist(skeleton)
  probe_cols <- setdiff(names(data), c("accuracy", "mean_rt"))

  objective <- function(par) {
    model$params <- utils::relist(par, skeleton)
    pred <- cm_predict(model, data[probe_cols], mem = mem)
    pred_accuracy <- ifelse(data$category == 1, pred$p_cat1, 1 - pred$p_cat1)

    # Weighted sum-of-squares criterion (Nosofsky & Stanton, 2005), not a likelihood.
    sse <- (pred_accuracy - data$accuracy)^2 / (stats::sd(data$accuracy) / sqrt(nrow(data))) +
      (pred$mean_rt - data$mean_rt)^2 / (stats::sd(data$mean_rt) / nrow(data))
    if (any(!is.finite(sse))) return(1e10)
    sum(sse)
  }

  opt <- stats::optim(par0, objective, method = method, lower = lower, upper = upper)
  model$params <- utils::relist(opt$par, skeleton)
  model$fit <- opt
  model
}

register_model(
  name = "ebrw",
  domain = "decision-making",
  task_types = "speeded-binary-classification",
  constructor = ebrw,
  description = "Random-walk extension of the GCM predicting choice and response time jointly.",
  citation = paste(
    "Nosofsky, R. M., & Palmeri, T. J. (1997). An exemplar-based random walk",
    "model of speeded classification. Psychological Review, 104(2), 266-300."
  )
)
