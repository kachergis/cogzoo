# Shepard, Hovland, & Jenkins (1961) problem structures over 3 binary dimensions
# (stimuli ordered 000, 001, 010, ..., 111), as used in catlearn's own examples.
shj_categories <- list(
  c(1, 1, 1, 1, 2, 2, 2, 2),  # Type I
  c(1, 1, 2, 2, 2, 2, 1, 1),  # Type II
  c(1, 1, 1, 2, 2, 1, 2, 2),  # Type III
  c(1, 1, 1, 2, 1, 2, 2, 2),  # Type IV
  c(1, 1, 1, 2, 2, 2, 2, 1),  # Type V
  c(1, 2, 2, 1, 2, 1, 1, 2)   # Type VI
)

# A training sequence for `n_subj` learners: `n_blocks` blocks, each a random
# permutation of the 8 stimuli. `x` holds 0/1 coordinates; add 1 for SUSTAIN's
# nominal levels.
shj_sequence <- function(type, n_subj = 5, n_blocks = 8, seed = 1) {
  set.seed(seed)
  stim <- as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1)))[, 3:1]
  stim <- stim[order(stim[, 1], stim[, 2], stim[, 3]), ]
  idx <- unlist(replicate(n_subj * n_blocks, sample(8), simplify = FALSE))
  list(
    x = stim[idx, , drop = FALSE],
    category = shj_categories[[type]][idx],
    subject = rep(seq_len(n_subj), each = 8 * n_blocks)
  )
}

# Mean probability of the correct category over the first `n_blocks` blocks.
mean_accuracy <- function(probs, category, n_blocks = 4, per_subject = 64) {
  p_correct <- probs[cbind(seq_along(category), category)]
  mean(p_correct[(seq_along(p_correct) - 1) %% per_subject < n_blocks * 8])
}
