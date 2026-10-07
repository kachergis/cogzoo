# Shared plumbing for the catlearn-backed models (alcove(), sustain()).

cl_require <- function(model_name) {
  if (!requireNamespace("catlearn", quietly = TRUE)) {
    stop(
      "Package 'catlearn' is required for ", model_name, ". Install it with install.packages('catlearn').",
      call. = FALSE
    )
  }
}

# catlearn's "ctrl" column: 1 resets the network before that trial, 0 is a normal trial.
# `subject` (optional) marks independent learners: the network resets at each change of subject.
cl_ctrl <- function(n, subject = NULL) {
  if (is.null(subject)) {
    first <- 1L
  } else {
    stopifnot(length(subject) == n)
    first <- which(c(TRUE, subject[-1] != subject[-n]))
  }
  ctrl <- integer(n)
  ctrl[first] <- 1L
  ctrl
}

# Probability of the observed response on each trial, floored so one confidently
# wrong prediction doesn't send the log-likelihood to -Inf.
cl_loglik_response <- function(probs, response) {
  p <- probs[cbind(seq_along(response), response)]
  sum(log(pmax(p, 1e-10)))
}

cl_sample_response <- function(probs) {
  apply(probs, 1, function(p) sample.int(length(p), 1, prob = p))
}
