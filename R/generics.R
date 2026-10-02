#' Construct a cognitive model object
#'
#' Helper for model constructors (e.g. [gcm()]) to build a standard
#' `cogmodel` object. Not usually called directly by users.
#'
#' @param params Named list of model parameters (the values [cm_fit()] optimizes over).
#' @param domain Character scalar, e.g. `"categorization"`, `"memory"`, `"learning"`, `"decision-making"`.
#' @param task_types Character vector of task types the model applies to (free text,
#'   but reuse existing values across models where possible so [list_models()] stays filterable).
#' @param subclass Character scalar naming the model's S3 subclass, e.g. `"gcm"`.
#' @param ... Additional fixed (non-fitted) settings stored on the object, e.g. a distance metric.
#' @return An object of class `c(subclass, "cogmodel")`.
#' @export
new_cogmodel <- function(params, domain, task_types, subclass, ...) {
  structure(
    c(list(params = params, domain = domain, task_types = task_types), list(...)),
    class = c(subclass, "cogmodel")
  )
}

#' Simulate data from a cognitive model
#' @param model A `cogmodel` object.
#' @param ... Passed to methods.
#' @export
cm_simulate <- function(model, ...) UseMethod("cm_simulate")

#' Log-likelihood of data under a cognitive model
#' @param model A `cogmodel` object.
#' @param data A data frame in the model's expected format.
#' @param ... Passed to methods.
#' @export
cm_loglik <- function(model, data, ...) UseMethod("cm_loglik")

#' Generate model predictions
#'
#' The generic deliberately takes only `(model, ...)`: different models need
#' very different auxiliary inputs to predict from (a memory set, a probe
#' matrix, a set of trial events, ...), so methods are free to declare
#' whatever named arguments they need instead of being forced into a
#' one-size-fits-all `newdata` slot. See an individual model's help page
#' (e.g. [gcm()], [minerva2()]) for what it expects.
#'
#' @param model A `cogmodel` object.
#' @param ... Passed to methods.
#' @export
cm_predict <- function(model, ...) UseMethod("cm_predict")

#' Fit a cognitive model to data by maximum likelihood
#'
#' Default method: optimizes [cm_loglik()] over `model$params` with
#' [stats::optim()]. Models with a custom fitting procedure (e.g. a
#' non-likelihood objective, or a Stan/JAGS backend) should define their own
#' `cm_fit.<subclass>` method instead of relying on this default; see
#' [ebrw()] for a worked example. As with [cm_predict()], the generic only
#' fixes `(model, data, ...)` so methods can add whatever auxiliary
#' arguments they need (e.g. a memory set) ahead of `...`.
#'
#' @param model A `cogmodel` object; `model$params` supplies the starting values.
#' @param data A data frame in the model's expected format.
#' @param ... Passed to methods. The default method takes `method`, `lower`,
#'   and `upper` (passed to [stats::optim()]) here, plus anything
#'   [cm_loglik()] needs (e.g. fixed data like a memory set).
#' @return `model` with `params` updated to the fitted values and a `fit`
#'   element holding the raw [stats::optim()] result (for the default method).
#' @export
cm_fit <- function(model, data, ...) UseMethod("cm_fit")

#' @export
cm_fit.default <- function(model, data, method = "Nelder-Mead", lower = -Inf, upper = Inf, ...) {
  skeleton <- model$params
  par0 <- unlist(skeleton)

  objective <- function(par) {
    model$params <- utils::relist(par, skeleton)
    nll <- -cm_loglik(model, data, ...)
    if (!is.finite(nll)) return(1e10)
    nll
  }

  opt <- if (identical(method, "L-BFGS-B")) {
    stats::optim(par0, objective, method = method, lower = lower, upper = upper)
  } else {
    stats::optim(par0, objective, method = method)
  }

  model$params <- utils::relist(opt$par, skeleton)
  model$fit <- opt
  model
}

#' @export
print.cogmodel <- function(x, ...) {
  cat(sprintf("<%s> cognitive model  (domain: %s)\n", class(x)[1], x$domain))
  cat("Parameters:\n")
  print(unlist(x$params))
  if (!is.null(x$fit)) {
    cat(sprintf("Fitted: convergence = %s, value = %.4f\n", x$fit$convergence, x$fit$value))
  }
  invisible(x)
}

#' Calibration plot: observed vs. predicted
#'
#' Default plot method for any `cogmodel`. Models with richer diagnostics
#' should define `plot.<subclass>` instead.
#'
#' @param x A `cogmodel` object.
#' @param observed Numeric vector of observed values.
#' @param predicted Numeric vector of model predictions, same length as `observed`.
#' @param ... Passed to [graphics::plot()].
#' @export
plot.cogmodel <- function(x, observed, predicted, ...) {
  graphics::plot(observed, predicted, xlab = "Observed", ylab = "Predicted", asp = 1, ...)
  graphics::abline(0, 1, lty = 2)
  invisible(x)
}
