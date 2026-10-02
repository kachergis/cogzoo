#' Linear Ballistic Accumulator (Brown & Heathcote, 2008)
#'
#' A tractable alternative to the diffusion model ([ddm()]): each response
#' option has its own accumulator with a random starting point (uniform on
#' `[0, A]`), a trial-varying drift rate (normal with mean `v_i`, SD `s`),
#' and no within-trial noise — evidence rises linearly to a fixed threshold
#' `b`. The resulting finishing-time distribution has a closed form, which
#' is precisely why the LBA is popular when a tractable likelihood matters
#' more than the diffusion model's extra psychological detail.
#'
#' The closed-form density (Brown & Heathcote, 2008, Eqs. 1-3) is delegated
#' entirely to the Suggested package \pkg{rtdists} (`rtdists::dLBA()` /
#' `rtdists::rLBA()`) rather than re-derived here: this model's whole value
#' is numerical correctness of that density, so leaning on a widely used,
#' independently tested implementation is safer than a bespoke one. As with
#' [ebddm()]/[ddm()], this stays a Suggests dependency, gated behind
#' `requireNamespace()`.
#'
#' @param drift1,drift2 Mean drift rate for accumulator 1 and 2.
#' @param drift_sd Trial-to-trial SD of drift rates (shared across accumulators); > 0.
#' @param start_range Upper bound of the uniform starting-point distribution ("A"); >= 0.
#' @param threshold Response threshold ("b"); > `start_range`.
#' @param ndt Non-decision time; >= 0.
#' @return A `cogmodel` object of subclass `"lba"`.
#' @export
lba <- function(drift1 = 1, drift2 = 0.5, drift_sd = 0.5, start_range = 0.5, threshold = 1, ndt = 0.2) {
  new_cogmodel(
    params = list(drift1 = drift1, drift2 = drift2, drift_sd = drift_sd, start_range = start_range, threshold = threshold, ndt = ndt),
    domain = "decision-making",
    task_types = "speeded-binary-classification",
    subclass = "lba"
  )
}

#' @param model An `lba` model object.
#' @param n_trials Number of trials to simulate.
#' @param ... Unused.
#' @return A data frame with `rt` and `response` (1 or 2).
#' @rdname lba
#' @export
cm_simulate.lba <- function(model, n_trials = 100, ...) {
  if (!requireNamespace("rtdists", quietly = TRUE)) {
    stop("Package 'rtdists' is required to simulate from lba. Install it with install.packages('rtdists').", call. = FALSE)
  }
  p <- model$params
  sim <- suppressMessages(rtdists::rLBA(
    n_trials,
    A = p$start_range, b = p$threshold, t0 = p$ndt,
    mean_v = c(p$drift1, p$drift2), sd_v = p$drift_sd
  ))
  data.frame(rt = sim$rt, response = sim$response)
}

#' @param data Data frame with `rt` and `response` (1 or 2), one row per trial.
#' @rdname lba
#' @export
cm_loglik.lba <- function(model, data, ...) {
  if (!requireNamespace("rtdists", quietly = TRUE)) {
    stop("Package 'rtdists' is required for lba's likelihood. Install it with install.packages('rtdists').", call. = FALSE)
  }
  p <- model$params
  dens <- suppressMessages(rtdists::dLBA(
    data$rt, data$response,
    A = p$start_range, b = p$threshold, t0 = p$ndt,
    mean_v = c(p$drift1, p$drift2), sd_v = p$drift_sd
  ))
  sum(log(dens))
}

register_model(
  name = "lba",
  domain = "decision-making",
  task_types = "speeded-binary-classification",
  constructor = lba,
  description = "Race-to-threshold choice/RT model with a closed-form likelihood (no within-trial noise).",
  citation = paste(
    "Brown, S. D., & Heathcote, A. (2008). The simplest complete model of",
    "choice response time: Linear ballistic accumulation. Cognitive",
    "Psychology, 57(3), 153-178."
  )
)
