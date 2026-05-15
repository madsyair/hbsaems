# R/posterior-methods.R
# =============================================================================
# Posterior / prior draw extraction for hbmfit objects.
# =============================================================================

#' Posterior and Prior Extraction Methods for hbmfit
#'
#' Convenience wrappers around \pkg{brms} draw extractors.
#'
#' @name posterior-methods
NULL


# =============================================================================
# posterior_draws()
# =============================================================================

#' Extract Posterior Draws as a Matrix
#'
#' @param model  An \code{hbmfit} object.
#' @param params Optional character vector of parameter names; \code{NULL}
#'   (default) returns all parameters.
#' @param ...    Additional arguments forwarded to
#'   \code{\link[brms]{as_draws_matrix}}.
#'
#' @return A draws matrix (rows = MCMC iterations, columns = parameters).
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | group),    # area-level random effect
#'              chains = 2, iter = 1000, warmup = 500,
#'              cores = 1, seed = 1, refresh = 0)
#' draws <- posterior_draws(model)
#' dim(draws)
#' }
#' @export
posterior_draws <- function(model, params = NULL, ...) {
  UseMethod("posterior_draws")
}

#' @export
posterior_draws.hbmfit <- function(model, params = NULL, ...) {
  brms::as_draws_matrix(model$model, variable = params, ...)
}


# =============================================================================
# prior_draws()
# =============================================================================

#' Extract Prior Draws
#'
#' Requires the model to have been fit with \code{sample_prior = "yes"} or
#' \code{sample_prior = "only"}.
#'
#' @param model An \code{hbmfit} object.
#' @param ...   Additional arguments forwarded to
#'   \code{\link[brms]{prior_draws}}.
#'
#' @return A \code{data.frame} of prior draws or \code{NULL} if no prior
#'   samples were stored in the model.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | group),    # area-level random effect
#'              sample_prior = "yes",
#'              chains = 2, iter = 1000, warmup = 500,
#'              cores = 1, seed = 1, refresh = 0)
#' pd <- prior_draws(model)
#' head(pd)
#' }
#' @export
prior_draws <- function(model, ...) UseMethod("prior_draws")

#' @export
prior_draws.hbmfit <- function(model, ...) {
  # `sample_prior` is a model attribute -- check the brmsfit metadata
  sp <- tryCatch(model$model$sample_prior, error = function(e) "no")
  if (isTRUE(sp == "no")) {
    warning("No prior draws stored in this model. ",
            "Refit with sample_prior = 'yes' or 'only'.", call. = FALSE)
    return(NULL)
  }
  brms::prior_draws(model$model, ...)
}


# =============================================================================
# posterior_interval()
# =============================================================================

#' Compute Credible Intervals
#'
#' @param model  An \code{hbmfit} object.
#' @param params Optional character vector of parameter names.
#' @param prob   Coverage probability in \eqn{(0, 1)} (default \code{0.95}).
#' @param ...    Additional arguments forwarded to \code{posterior_draws}.
#'
#' @return A matrix with two rows giving lower and upper bounds.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | group),    # area-level random effect
#'              chains = 2, iter = 1000, warmup = 500,
#'              cores = 1, seed = 1, refresh = 0)
#' posterior_interval(model, prob = 0.90)
#' }
#' @export
posterior_interval <- function(model, params = NULL, prob = 0.95, ...) {
  UseMethod("posterior_interval")
}

#' @export
posterior_interval.hbmfit <- function(model, params = NULL,
                                      prob = 0.95, ...) {
  if (!is.numeric(prob) || length(prob) != 1L || prob <= 0 || prob >= 1)
    stop("'prob' must be a single number in (0, 1).", call. = FALSE)

  alpha <- 1 - prob
  draws <- posterior_draws(model, params = params, ...)

  apply(draws, 2L, stats::quantile,
        probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
}


# =============================================================================
# posterior_summary_hbm()
# =============================================================================

#' Comprehensive Posterior Summary
#'
#' Returns fixed effects, random effects, and model-fit statistics in a
#' single named list.
#'
#' @param model An \code{hbmfit} object.
#' @param probs Probability bounds for credible intervals
#'   (default \code{c(0.025, 0.975)}).
#' @param ...   Additional arguments forwarded to the underlying \pkg{brms}
#'   functions.
#'
#' @return A named list with components \code{fixed_effects},
#'   \code{random_effects}, and \code{model_fit} (containing \code{loo},
#'   \code{waic}, and \code{R2}).
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | group),    # area-level random effect
#'              chains = 2, iter = 1000, warmup = 500,
#'              cores = 1, seed = 1, refresh = 0)
#' s <- posterior_summary_hbm(model)
#' s$fixed_effects
#' }
#' @export
posterior_summary_hbm <- function(model, probs = c(0.025, 0.975), ...) {
  UseMethod("posterior_summary_hbm")
}

#' @export
posterior_summary_hbm.hbmfit <- function(model, probs = c(0.025, 0.975),
                                         ...) {
  list(
    fixed_effects  = brms::fixef(model$model, probs = probs, ...),
    random_effects = tryCatch(
      brms::ranef(model$model, probs = probs, ...),
      error = function(e) NULL
    ),
    model_fit      = list(
      loo  = tryCatch(brms::loo(model$model),  error = function(e) NULL),
      waic = tryCatch(brms::waic(model$model), error = function(e) NULL),
      R2   = tryCatch(brms::bayes_R2(model$model), error = function(e) NULL)
    )
  )
}
