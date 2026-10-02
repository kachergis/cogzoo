#' MINERVA-AL (Jamieson, Crump, & Hannah, 2012)
#'
#' An instance-based associative learning model: MINERVA 2's storage/
#' retrieval architecture ([minerva2()]) applied to cue-outcome learning.
#' Each trial probes memory with the current cues to produce an expectancy
#' of the outcome, then stores the (discrepancy-encoded) trial as a new
#' trace. Ported from `minerva-al/minerva-al.R`; the math (Eqs. 2-6,
#' Jamieson et al., 2012) is unchanged.
#'
#' The original code had no fitting procedure — it was used to simulate
#' expectancy curves and compare them qualitatively to a reference
#' implementation (see `minerva-al/reproduce_jamieson_etal_2012.Rmd`).
#' [cm_loglik.minerva_al()] adds a genuine one: it treats an observed
#' expectancy trajectory as normally distributed around the model's
#' (replication-averaged) predicted trajectory, with the residual SD as a
#' free nuisance parameter (`sigma`). This is a reasonable default when
#' fitting a stochastic simulation model to smooth aggregate curves, but a
#' choice, not the only one — see `CONTRIBUTING.md` if you want to add an
#' alternative (e.g. a likelihood over individual, rather than averaged, trials).
#'
#' Note a numerical-stability limitation inherited from the original
#' implementation: `mval_probe_memory()`'s similarity computation divides by
#' `rowSums(relevant_memory^2)`, which is exactly zero for any trace whose
#' cue features were *entirely* unencoded that trial (probability
#' `(1 - p_encode)^length(cue_features)`). That produces `NaN` echoes (and
#' hence `NaN` log-likelihoods) whenever `p_encode` is low enough, relative
#' to the number of cue features, for an all-zero trace to be a realistic
#' draw. Use enough cue features (or keep `p_encode` high enough) that this
#' stays astronomically unlikely — the original paper's simulations used
#' 100 cue features for exactly this reason.
#'
#' @param p_encode Probability a given feature is encoded (without error) into a new trace; in \[0, 1\].
#' @param sigma Residual SD of observed expectancy around the model's predicted expectancy; > 0.
#' @return A `cogmodel` object of subclass `"minerva_al"`.
#' @export
minerva_al <- function(p_encode = 1, sigma = 0.1) {
  new_cogmodel(
    params = list(p_encode = p_encode, sigma = sigma),
    domain = "learning",
    task_types = "cue-outcome-expectancy",
    subclass = "minerva_al"
  )
}

# Internal: activate memory with a probe, restricted to `cue_features` (Eqs. 2-4, Jamieson et al. 2012).
# Not exported -- prefixed to avoid colliding with minerva2()'s differently-shaped probe_memory().
mval_probe_memory <- function(probe, memory, cue_features) {
  if (is.null(memory)) {
    echo <- stats::runif(length(probe), -0.001, 0.001)  # First trial is noise (p. 65)
    return(echo / max(abs(echo)))                        # Eq. 4
  }

  probe <- probe[cue_features]
  relevant_memory <- memory[, cue_features, drop = FALSE]

  similarity <- colSums(probe * t(relevant_memory)) / sqrt(sum(probe^2) * rowSums(relevant_memory^2))  # simplified Eq. 7
  activation <- similarity^3                                       # Eq. 2
  echo <- colSums(activation * memory)                             # Eq. 3
  echo <- echo + stats::runif(length(echo), -0.001, 0.001)         # Add noise (p. 64)
  echo / max(abs(echo))                                            # Eq. 4
}

# Internal: expectancy of an outcome given the current echo (Eq. 5, Jamieson et al. 2012).
mval_expect_event <- function(outcome, normalized_echo) {
  sum(outcome * normalized_echo) / sum(outcome != 0 & normalized_echo != 0)
}

# Internal: store the discrepancy between an event and its expectation as a new trace (Eq. 6).
mval_learn <- function(normalized_echo, event, p_encode, memory) {
  encoding_error <- if (p_encode < 1) stats::rbinom(length(event), 1, p_encode) else rep(1, length(event))
  rbind(memory, (event - normalized_echo) * encoding_error)
}

#' @param model A `minerva_al` model object.
#' @param probes Matrix/data frame, one row per trial: the cue(+context) feature vector queried that trial.
#' @param events Matrix/data frame, one row per trial: the full (cue+context+outcome) feature vector experienced that trial.
#' @param outcomes Matrix/data frame, one row per trial: the outcome feature vector used to score expectancy.
#' @param cue_features Integer vector of feature indices to use when computing similarity (Eq. 7 restricts to cue features).
#' @param ... Unused.
#' @rdname minerva_al
#' @export
cm_simulate.minerva_al <- function(model, probes, events, outcomes, cue_features, ...) {
  probes <- as.matrix(probes)
  events <- as.matrix(events)
  outcomes <- as.matrix(outcomes)
  n_trials <- nrow(probes)

  memory <- NULL
  expectancy <- numeric(n_trials)
  for (t in seq_len(n_trials)) {
    echo <- mval_probe_memory(probes[t, ], memory, cue_features)
    expectancy[t] <- mval_expect_event(outcomes[t, ], echo)
    memory <- mval_learn(echo, events[t, ], model$params$p_encode, memory)
  }

  data.frame(trial = seq_len(n_trials), expectancy = expectancy)
}

#' @param n_reps Number of stochastic replications to average over (encoding is stochastic when `p_encode` < 1).
#' @rdname minerva_al
#' @export
cm_predict.minerva_al <- function(model, probes, events, outcomes, cue_features, n_reps = 20, ...) {
  n_trials <- nrow(as.matrix(probes))
  # See the comment in cm_predict.minerva2(): vapply's FUN.VALUE keeps the row
  # count fixed at n_trials even for a single-trial scenario.
  reps <- vapply(
    seq_len(n_reps),
    function(r) cm_simulate(model, probes, events, outcomes, cue_features)$expectancy,
    numeric(n_trials)
  )
  expectancy <- if (n_trials == 1) mean(reps) else rowMeans(reps)
  data.frame(trial = seq_len(n_trials), expectancy = expectancy)
}

#' @param data Data frame with `trial` and observed `expectancy` (e.g. averaged human ratings).
#' @rdname minerva_al
#' @export
cm_loglik.minerva_al <- function(model, data, probes, events, outcomes, cue_features, n_reps = 20, ...) {
  pred <- cm_predict(model, probes, events, outcomes, cue_features, n_reps = n_reps)
  sum(stats::dnorm(data$expectancy, mean = pred$expectancy, sd = model$params$sigma, log = TRUE))
}

register_model(
  name = "minerva_al",
  domain = "learning",
  task_types = "cue-outcome-expectancy",
  constructor = minerva_al,
  description = "Instance-based associative learning: MINERVA 2's storage/retrieval applied to cue-outcome trials.",
  citation = paste(
    "Jamieson, R. K., Crump, M. J. C., & Hannah, S. D. (2012). An instance",
    "theory of associative learning. Learning & Behavior, 40(1), 61-82."
  )
)
