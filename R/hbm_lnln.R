# R/hbm_lnln.R
# =============================================================================
# Convenience wrapper for SAE under a Lognormal-Lognormal model.
#
# Supports two modes for the residual standard deviation `sigma`:
#
#   (1) RANDOM (default)  - sigma is sampled with brms's default prior or a
#       user-supplied prior.
#   (2) FIXED via known sampling variance - if the user supplies
#       `sampling_var` (the column name of psi_i, the known sampling
#       variance per area), sigma is pinned via offset.  This is the
#       Fay-Herriot pattern for a lognormal Fay-Herriot model.
# =============================================================================


#' Small Area Estimation under a Lognormal-Lognormal Model
#'
#' Convenience wrapper that fits a Hierarchical Bayesian Small Area
#' Estimation model with a \strong{lognormal} likelihood.  Internally
#' delegates to \code{\link{hbm_flex}} with
#' \code{family_key = "lognormal"}.
#'
#' The response \eqn{y_i} in area \eqn{i} is assumed to follow
#' \deqn{y_i \mid \theta_i \sim \mathrm{Lognormal}(\theta_i, \sigma^2),}
#' with the log-mean linked to auxiliary variables via
#' \deqn{\log(\theta_i) = x_i^\top \boldsymbol{\beta} + u_i,
#'        \quad u_i \sim \mathcal{N}(0, \sigma_v^2).}
#'
#' When the user supplies \code{sampling_var = "psi_i"} (the column name of
#' the known per-area sampling variance \eqn{\psi_i}), \eqn{\sigma_i =
#' \sqrt{\psi_i}} is pinned for each area via an offset.  This recovers
#' the Fay-Herriot-style lognormal model in which residual variability is
#' fully determined by the survey design.
#'
#' @param response Character.  Name of the response column (must be
#'   strictly positive).
#' @param auxiliary Character vector of auxiliary (fixed-effect) variable
#'   names; corresponds to area-level covariates in SAE literature
#'   (see Rao & Molina 2015 Ch. 4).
#' @param predictors \strong{Deprecated.}  Use \code{auxiliary} instead.
#'   Kept for backward compatibility; will be removed in v2.0.0.
#' @param data A \code{data.frame}.
#' @param sampling_var Optional character.  Name of a column in \code{data}
#'   containing the known per-area sampling variance \eqn{\psi_i} on the
#'   \strong{log scale}, i.e.\ the variance of \eqn{\log(\hat y_i)}.  When
#'   supplied, \eqn{\sigma_i = \sqrt{\psi_i}} is pinned via offset,
#'   recovering the Fay--Herriot lognormal model.  If your survey
#'   software produces \eqn{\mathrm{Var}(\hat y_i)} on the original
#'   scale, convert with the delta-method approximation
#'   \eqn{\psi_i \approx \mathrm{Var}(\hat y_i) / \hat y_i^2} before
#'   passing.  Default: \code{NULL} (sigma is random).
#' @param fixed_params Optional named list pinning distributional
#'   parameters to known values.  See \code{\link{hbm}} for the spec
#'   format.  Allows power-user access to the same machinery used by
#'   \code{sampling_var}.
#' @param ... Additional arguments forwarded to \code{\link{hbm_flex}}
#'   (e.g. \code{group}, \code{sre}, \code{prior_type}, \code{nonlinear},
#'   \code{handle_missing}, sampler controls).
#'
#' @return An object of class \code{hbmfit}.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' data("data_lnln")
#'
#' # -- 1. Standard lognormal SAE with area random effect ----------------------
#' fit1 <- hbm_lnln(
#'   response   = "y_obs",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   group      = "group",
#'   data       = data_lnln,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#'
#' # -- 2. Fay-Herriot style with known sampling variance ----------------------
#' #     (assumes psi_i column is available)
#' fit2 <- hbm_lnln(
#'   response     = "y_obs",
#'   auxiliary    = c("x1", "x2", "x3"),
#'   group        = "group",
#'   sampling_var = "psi_i",
#'   data         = data_lnln,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' }
#'
#' @seealso \code{\link{hbm_flex}}, \code{\link{hbm}}
#' @export
hbm_lnln <- function(response,
                     auxiliary    = NULL,
                     data,
                     sampling_var = NULL,
                     fixed_params = NULL,
                     predictors   = NULL,
                     ...) {

  # -- 0. Deprecated alias 'predictors' (v0.6.1) ----------------------------
  if (!is.null(predictors)) {
    if (!is.null(auxiliary))
      stop("Pass either `auxiliary` (preferred) or `predictors` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("predictors", "auxiliary", "v2.0.0")
    auxiliary <- predictors
  }
  if (is.null(auxiliary))
    stop("`auxiliary` (auxiliary variables) is required.", call. = FALSE)

  # -- 1. If sampling_var provided, translate to fixed_params ----------------
  if (!is.null(sampling_var)) {
    if (!is.character(sampling_var) || length(sampling_var) != 1L)
      stop("`sampling_var` must be a single column name (character).",
           call. = FALSE)
    if (!sampling_var %in% names(data))
      stop(sprintf("`sampling_var = \"%s\"` not found in `data`.",
                    sampling_var),
           call. = FALSE)
    psi <- data[[sampling_var]]
    if (any(is.na(psi) | psi <= 0))
      stop("`sampling_var` must contain finite, strictly positive values.",
           call. = FALSE)

    # Refuse conflict with user-supplied fixed_params on the same param
    if (is.list(fixed_params) && "sigma" %in% names(fixed_params))
      stop("Cannot supply both `sampling_var` and `fixed_params$sigma`. ",
           "Pick one.", call. = FALSE)

    # sigma = sqrt(psi)
    fp <- fixed_params %||% list()
    fp$sigma <- sqrt(psi)
    fixed_params <- fp
  }

  hbm_flex(family_key   = "lognormal",
           response     = response,
           auxiliary    = auxiliary,
           data         = data,
           fixed_params = fixed_params,
           ...)
}
