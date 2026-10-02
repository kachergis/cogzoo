#' Rescorla-Wagner model (Rescorla & Wagner, 1972)
#'
#' The classic error-correction model of associative learning: on each
#' trial, every present cue's associative strength moves toward the
#' trial's outcome in proportion to the *summed* prediction error across
#' all present cues (not each cue's own error) — the mechanism behind
#' blocking, overshadowing, and conditioned inhibition.
#'
#' Unlike [minerva_al()] (a different architecture for the same kind of
#' task), this model is fully deterministic given a cue/outcome sequence,
#' so [cm_loglik.rescorla_wagner()] doesn't need to average over stochastic
#' replications — it adds one nuisance parameter (`sigma`) for the residual
#' SD of observed expectancy around the model's (exact) predicted
#' expectancy.
#'
#' @param alpha Learning rate (combined salience x learning-rate term); in \[0, 1\].
#' @param sigma Residual SD of observed expectancy around predicted expectancy; > 0.
#' @param v0 Starting associative strength for every cue (not fit; a fixed setting).
#' @return A `cogmodel` object of subclass `"rescorla_wagner"`.
#' @export
rescorla_wagner <- function(alpha = 0.3, sigma = 0.2, v0 = 0) {
  new_cogmodel(
    params = list(alpha = alpha, sigma = sigma),
    domain = "learning",
    task_types = "cue-outcome-association",
    subclass = "rescorla_wagner",
    v0 = v0
  )
}

#' @param model A `rescorla_wagner` model object.
#' @param cues Matrix/data frame, one row per trial, one column per cue: 1 if the cue is present that trial, else 0.
#' @param outcomes Numeric vector, one value per trial: the outcome magnitude ("lambda", e.g. 1 if reinforced, 0 if not).
#' @param ... Unused.
#' @return A data frame with one row per trial: `prediction` (summed associative
#'   strength immediately before that trial's update) and one column per cue
#'   giving its associative strength immediately *after* that trial's update.
#' @rdname rescorla_wagner
#' @export
cm_simulate.rescorla_wagner <- function(model, cues, outcomes, ...) {
  cues <- as.matrix(cues)
  n_trials <- nrow(cues)
  n_cues <- ncol(cues)

  v <- rep(model$v0, n_cues)
  v_hist <- matrix(NA_real_, n_trials, n_cues, dimnames = list(NULL, colnames(cues)))
  prediction <- numeric(n_trials)

  for (t in seq_len(n_trials)) {
    prediction[t] <- sum(v * cues[t, ])                                     # V_sum, summed associative strength
    delta <- model$params$alpha * (outcomes[t] - prediction[t])             # Rescorla-Wagner delta rule
    v <- v + delta * cues[t, ]
    v_hist[t, ] <- v
  }

  data.frame(trial = seq_len(n_trials), prediction = prediction, v_hist)
}

#' @rdname rescorla_wagner
#' @export
cm_predict.rescorla_wagner <- function(model, cues, outcomes, ...) {
  cm_simulate(model, cues, outcomes)
}

#' @param data Data frame with observed `expectancy`, one row per trial (same order as `cues`/`outcomes`).
#' @rdname rescorla_wagner
#' @export
cm_loglik.rescorla_wagner <- function(model, data, cues, outcomes, ...) {
  pred <- cm_predict(model, cues, outcomes)
  sum(stats::dnorm(data$expectancy, mean = pred$prediction, sd = model$params$sigma, log = TRUE))
}

register_model(
  name = "rescorla_wagner",
  domain = "learning",
  task_types = "cue-outcome-association",
  constructor = rescorla_wagner,
  description = "Error-correction associative learning: cues compete for a limited amount of associative strength.",
  citation = paste(
    "Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian",
    "conditioning: Variations in the effectiveness of reinforcement and",
    "nonreinforcement. In A. H. Black & W. F. Prokasy (Eds.), Classical",
    "Conditioning II (pp. 64-99)."
  )
)
