#' MINERVA 2 (Hintzman, 1984, 1988)
#'
#' Multiple-trace memory model: episodes are stored as (possibly
#' imperfectly encoded) traces, and a probe activates all traces in
#' proportion to their similarity cubed; the summed activation is "echo
#' intensity" (used for e.g. frequency and recognition judgments) and the
#' activation-weighted sum of traces is "echo content" (used for e.g. cued
#' recall). Ported from `minerva2/minerva2.R`; the math (Eqs. 1-4,
#' Hintzman 1984) is unchanged.
#'
#' Unlike the other models in the zoo, MINERVA 2 has no `cm_loglik`/`cm_fit`
#' method: in its original use here (reproducing Hintzman, 1988's frequency-
#' judgment simulation) it's compared against human data via the *shape* of
#' the resulting echo-intensity distribution, not fit to individual trial
#' responses by maximum likelihood. A model is welcome in the zoo without a
#' fit method as long as that's a property of the model's usual evaluation,
#' not a shortcut — [minerva_al()] is the same architecture with a fit
#' method added because there was a natural trial-by-trial outcome to fit to.
#'
#' @param p_encode Probability a given feature is encoded (without error) into a new trace; in \[0, 1\].
#' @param retention Probability a given memory cell survives forgetting (1 = no forgetting); in \[0, 1\].
#' @return A `cogmodel` object of subclass `"minerva2"`.
#' @export
minerva2 <- function(p_encode = 1, retention = 1) {
  new_cogmodel(
    params = list(p_encode = p_encode, retention = retention),
    domain = "memory",
    task_types = c("frequency-judgment", "recognition-memory"),
    subclass = "minerva2"
  )
}

# Internal: activate a memory matrix with a probe (Eqs. 1-4, Hintzman 1984).
# Not exported -- prefixed to avoid colliding with minerva_al()'s differently-shaped probe_memory().
mv2_probe_memory <- function(probe, memory, normalize = FALSE) {
  similarity <- colSums(probe * t(memory)) / colSums((probe != 0 | t(memory) != 0))  # Eq. 1
  activation <- similarity^3                                                          # Eq. 2
  echo_intensity <- sum(activation)                                                   # Eq. 3
  echo_content <- colSums(activation * memory)                                        # Eq. 4
  if (normalize) echo_content <- echo_content / max(abs(echo_content))

  list(content = echo_content, intensity = echo_intensity)
}

# Internal: store one episode as a new (possibly imperfectly encoded) trace.
mv2_encode <- function(episode, memory, p_encode) {
  encoding_error <- stats::rbinom(length(episode), 1, p_encode)
  rbind(memory, episode * encoding_error)
}

# Internal: degrade an existing memory matrix by cell-wise forgetting.
# `retention` is the probability a cell survives (matches minerva2()'s parameter name).
mv2_forget <- function(memory, retention) {
  survives <- stats::rbinom(length(memory), 1, retention)
  memory * matrix(survives, ncol = ncol(memory))
}

#' @param model A `minerva2` model object.
#' @param episodes Matrix/data frame of study episodes, one row per feature vector.
#' @param probes Matrix/data frame of test probes, one row per feature vector.
#' @param ... Unused.
#' @rdname minerva2
#' @export
cm_simulate.minerva2 <- function(model, probes, episodes, ...) {
  episodes <- as.matrix(episodes)
  probes <- as.matrix(probes)

  memory <- NULL
  for (i in seq_len(nrow(episodes))) {
    memory <- mv2_encode(episodes[i, ], memory, model$params$p_encode)
  }
  if (model$params$retention < 1) {
    memory <- mv2_forget(memory, model$params$retention)
  }

  intensity <- vapply(seq_len(nrow(probes)), function(i) {
    mv2_probe_memory(probes[i, ], memory)$intensity
  }, numeric(1))

  data.frame(probe = seq_len(nrow(probes)), intensity = intensity)
}

#' @param n_reps Number of stochastic study/test replications to average over
#'   (encoding and forgetting are both stochastic when `p_encode`/`retention` < 1).
#' @rdname minerva2
#' @export
cm_predict.minerva2 <- function(model, probes, episodes, n_reps = 50, ...) {
  n_probes <- nrow(as.matrix(probes))
  # vapply's FUN.VALUE fixes the row count at n_probes even when n_probes == 1,
  # where replicate()/sapply() would otherwise collapse to a plain vector and
  # silently average across probes instead of across replications.
  reps <- vapply(seq_len(n_reps), function(r) cm_simulate(model, probes, episodes)$intensity, numeric(n_probes))
  intensity <- if (n_probes == 1) mean(reps) else rowMeans(reps)
  data.frame(probe = seq_len(n_probes), intensity = intensity)
}

register_model(
  name = "minerva2",
  domain = "memory",
  task_types = c("frequency-judgment", "recognition-memory"),
  constructor = minerva2,
  description = "Multiple-trace memory model: echo intensity/content from cubed-similarity activation.",
  citation = paste(
    "Hintzman, D. L. (1984). MINERVA 2: A simulation model of human memory.",
    "Behavior Research Methods, Instruments, & Computers, 16(2), 96-101."
  )
)
