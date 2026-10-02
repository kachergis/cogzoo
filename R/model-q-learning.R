#' Q-learning with softmax choice
#'
#' The standard reinforcement-learning workhorse behind most modern
#' repeated-choice/bandit modeling (e.g. Daw, O'Doherty, Dayan, Seymour, &
#' Dolan, 2006): on each trial, the chosen option's value moves toward the
#' received reward by a delta rule (Rescorla-Wagner's mechanism applied to
#' one option at a time, rather than letting all present cues compete), and
#' choice probabilities come from a softmax over current values. Bridges
#' the zoo's learning and decision-making domains the way the original
#' literature does.
#'
#' @param alpha Learning rate; in \[0, 1\].
#' @param beta Softmax inverse temperature (choice stochasticity); >= 0, higher = more deterministic.
#' @param q0 Starting value for every option (not fit; a fixed setting).
#' @return A `cogmodel` object of subclass `"q_learning"`.
#' @export
q_learning <- function(alpha = 0.3, beta = 1, q0 = 0) {
  new_cogmodel(
    params = list(alpha = alpha, beta = beta),
    domain = "learning",
    task_types = c("repeated-choice", "n-armed-bandit"),
    subclass = "q_learning",
    q0 = q0
  )
}

#' @param model A `q_learning` model object.
#' @param n_trials Number of trials to simulate.
#' @param reward_probs Numeric vector, one reward probability per option (its length sets the number of options).
#' @param ... Unused.
#' @return A data frame with `trial`, `choice` (chosen option index), and `reward` (0/1).
#' @rdname q_learning
#' @export
cm_simulate.q_learning <- function(model, n_trials, reward_probs, ...) {
  n_options <- length(reward_probs)
  q <- rep(model$q0, n_options)
  choice <- integer(n_trials)
  reward <- numeric(n_trials)

  for (t in seq_len(n_trials)) {
    p_choice <- exp(model$params$beta * q) / sum(exp(model$params$beta * q))  # softmax
    choice[t] <- sample.int(n_options, 1, prob = p_choice)
    reward[t] <- stats::rbinom(1, 1, reward_probs[choice[t]])
    q[choice[t]] <- q[choice[t]] + model$params$alpha * (reward[t] - q[choice[t]])  # delta rule, chosen option only
  }

  data.frame(trial = seq_len(n_trials), choice = choice, reward = reward)
}

#' @param data Data frame with `choice` and `reward`, one row per trial, in trial order.
#' @param n_options Number of available options.
#' @return A data frame with `trial` and `p_chosen`: the model's probability,
#'   at the time of that trial, of the option actually chosen.
#' @rdname q_learning
#' @export
cm_predict.q_learning <- function(model, data, n_options, ...) {
  q <- rep(model$q0, n_options)
  p_chosen <- numeric(nrow(data))

  for (t in seq_len(nrow(data))) {
    p_choice <- exp(model$params$beta * q) / sum(exp(model$params$beta * q))
    p_chosen[t] <- p_choice[data$choice[t]]
    q[data$choice[t]] <- q[data$choice[t]] + model$params$alpha * (data$reward[t] - q[data$choice[t]])
  }

  data.frame(trial = seq_len(nrow(data)), p_chosen = p_chosen)
}

#' @rdname q_learning
#' @export
cm_loglik.q_learning <- function(model, data, n_options, ...) {
  sum(log(cm_predict(model, data, n_options)$p_chosen))
}

register_model(
  name = "q_learning",
  domain = "learning",
  task_types = c("repeated-choice", "n-armed-bandit"),
  constructor = q_learning,
  description = "Delta-rule value learning with softmax choice, the standard model-based-fMRI/RL workhorse.",
  citation = paste(
    "Daw, N. D., O'Doherty, J. P., Dayan, P., Seymour, B., & Dolan, R. J.",
    "(2006). Cortical substrates for exploratory decisions in humans.",
    "Nature, 441(7095), 876-879."
  )
)
