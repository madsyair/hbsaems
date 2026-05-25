# R/hbm-info.R
# =============================================================================
# Lightweight inspection helpers that summarise an hbmfit without printing
# the full brmsfit summary.
# =============================================================================

#' Model Inspection Helpers
#'
#' Quick getters for an \code{hbmfit} object: metadata, training data, and
#' diagnostic warnings.
#'
#' @name hbm-info
NULL


# =============================================================================
# hbm_info() -- metadata
# =============================================================================

#' Get Comprehensive Model Information
#'
#' Returns a one-page summary of the fitted model's metadata: number of
#' observations, family, link function, formula, MCMC settings, missing-data
#' strategy, and so on.
#'
#' @param model An \code{hbmfit} object.
#' @return A named list of metadata.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | regency),    # area-level random effect
#'              chains = 4, iter = 2000, warmup = 1000,
#'              cores = 1, seed = 1, refresh = 0)
#' hbm_info(model)
#' }
#' @export
hbm_info <- function(model) {
  if (!is.hbmfit(model))
    stop("'model' must be an hbmfit object.", call. = FALSE)

  fam <- tryCatch(stats::family(model),
                  error = function(e) list(family = "?", link = "?"))

  list(
    n_obs          = nrow(model$data),
    family         = fam$family,
    link           = fam$link,
    formula        = deparse(stats::formula(model)),
    chains         = model$model$fit@sim$chains,
    iter           = model$model$fit@sim$iter,
    warmup         = model$model$fit@sim$warmup,
    missing_method = if (is.null(model$missing_method)) "none"
                     else model$missing_method,
    # brms::ranef() throws when there are no group-level effects -- treat
    # absence as "no random effects" (FALSE) rather than failing.
    has_re         = tryCatch(
      length(brms::ranef(model$model)) > 0L,
      error = function(e) FALSE
    ),
    n_parameters   = length(brms::variables(model$model))
  )
}


# =============================================================================
# hbm_data() -- training data
# =============================================================================

#' Return the Data Used to Fit an hbmfit
#'
#' @param model An \code{hbmfit} object.
#' @return The original \code{data.frame} passed to the fitting function.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | regency),    # area-level random effect
#'              chains = 4, iter = 2000, warmup = 1000,
#'              cores = 1, seed = 1, refresh = 0)
#' head(hbm_data(model))
#' }
#' @export
hbm_data <- function(model) {
  if (!is.hbmfit(model))
    stop("'model' must be an hbmfit object.", call. = FALSE)
  model$data
}


# =============================================================================
# hbm_warnings() -- diagnostic flags
# =============================================================================

#' Get Model Warnings
#'
#' Inspects the fitted model for common convergence problems and returns a
#' character vector of human-readable warnings.  When no warnings apply, the
#' string \code{"No warnings detected."} is returned.
#'
#' @param model An \code{hbmfit} object.
#' @return A character vector of warning messages.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | regency),    # area-level random effect
#'              chains = 4, iter = 2000, warmup = 1000,
#'              cores = 1, seed = 1, refresh = 0)
#' hbm_warnings(model)
#' }
#' @export
hbm_warnings <- function(model) {
  if (!is.hbmfit(model))
    stop("'model' must be an hbmfit object.", call. = FALSE)

  w <- character(0L)

  # R-hat -- guard against the all-NA case (e.g. degenerate fit, no chains
  # actually ran).  na.rm = TRUE on all-NA returns TRUE for all(), giving
  # the false impression of convergence; we treat all-NA as a separate
  # warning so the user is alerted.
  rh <- tryCatch(brms::rhat(model$model), error = function(e) NULL)
  if (!is.null(rh)) {
    finite_rh <- rh[is.finite(rh)]
    if (length(finite_rh) == 0L) {
      w <- c(w, "All R-hat values are NA: model fit appears degenerate.")
    } else if (!all(finite_rh < 1.1)) {
      w <- c(w, "R-hat > 1.1: model may not have converged.")
    }
  }

  # ESS
  nr <- tryCatch(brms::neff_ratio(model$model), error = function(e) NULL)
  if (!is.null(nr)) {
    finite_nr <- nr[is.finite(nr)]
    if (length(finite_nr) == 0L) {
      w <- c(w, "All neff_ratio values are NA: model fit appears degenerate.")
    } else if (any(finite_nr < 0.1)) {
      w <- c(w, "neff_ratio < 0.1: low effective sample size.")
    }
  }

  # Divergent transitions (NUTS only)
  np <- tryCatch(brms::nuts_params(model$model), error = function(e) NULL)
  if (!is.null(np)) {
    div <- np[np$Parameter == "divergent__", "Value"]
    if (length(div) && any(div == 1L))
      w <- c(w, sprintf(
        "Divergent NUTS transitions detected (%d divergences).",
        sum(div == 1L)
      ))
  }

  if (!length(w)) w <- "No warnings detected."
  w
}
