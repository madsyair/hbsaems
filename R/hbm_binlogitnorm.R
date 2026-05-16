# R/hbm_binlogitnorm.R
# =============================================================================
# Convenience wrapper for SAE under a Binomial Logit-Normal model.
# Delegates to hbm_flex("binomial", ...).
# =============================================================================


#' Small Area Estimation under a Binomial Logit-Normal Model
#'
#' Convenience wrapper that fits a Hierarchical Bayesian Small Area
#' Estimation model with a \strong{binomial} likelihood and logit link.
#' Internally delegates to \code{\link{hbm_flex}} with
#' \code{family_key = "binomial"}.
#'
#' Let \eqn{y_i} denote the number of successes in area \eqn{i} out of
#' \eqn{n_i} trials.  The model is
#' \deqn{y_i \mid p_i, n_i \sim \mathrm{Binomial}(n_i, p_i),}
#' \deqn{\mathrm{logit}(p_i) = x_i^\top \boldsymbol{\beta} + u_i,
#'        \quad u_i \sim \mathcal{N}(0, \sigma_v^2).}
#'
#' @param response Character.  Name of the successes variable
#'   (non-negative integer, \eqn{y \le n}).
#' @param trials Character.  Name of the trials variable (positive integer).
#' @param auxiliary Character vector of auxiliary (fixed-effect) variable
#'   names; corresponds to area-level covariates in SAE literature
#'   (see Rao & Molina 2015 Ch. 4).
#' @param predictors \strong{Deprecated.}  Use \code{auxiliary} instead.
#'   Kept for backward compatibility; will be removed in v2.0.0.
#' @param fixed_params Optional named list pinning distributional
#'   parameters to known values.  See \code{\link{hbm}} for the spec
#'   format.
#' @param data A \code{data.frame}.
#' @param ... Additional arguments forwarded to \code{\link{hbm_flex}}.
#'   See \code{?hbm_flex}.
#'
#' @return An object of class \code{hbmfit}.
#'
#' @section Conflict policy:
#' \itemize{
#'   \item \code{auxiliary} \emph{and} the deprecated \code{predictors}
#'         in the same call are rejected with an informative error.
#'   \item \code{handle_missing = "model"} is not supported (binomial is a
#'         discrete family; see \emph{Notes on missing data}).
#' }
#'
#' @section Notes on missing data:
#' The binomial family does not support \code{handle_missing = "model"}
#' (joint Bayesian imputation via \code{brms::mi()}).  When NA values are
#' detected and \code{handle_missing} is left \code{NULL}, the wrapper
#' auto-selects \code{"multiple"} (multiple imputation via \pkg{mice} on
#' the predictors only).
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_binlogitnorm")
#'
#' # -- 1. Standard binomial logit-normal SAE ---------------------------------
#' fit <- hbm_binlogitnorm(
#'   response   = "y",
#'   trials     = "n",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   group      = "group",            # area-level random effect
#'   data       = data_binlogitnorm,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#'
#' # -- 2. With spatial CAR random effect -------------------------------------
#' data("adjacency_matrix_car")
#' fit_car <- hbm_binlogitnorm(
#'   response   = "y",
#'   trials     = "n",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   sre        = "sre",
#'   sre_type   = "car",
#'   M          = adjacency_matrix_car,
#'   data       = data_binlogitnorm,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' }
#'
#' @seealso \code{\link{hbm_flex}}, \code{\link{hbm}}
#' @export
hbm_binlogitnorm <- function(response,
                             trials,
                             auxiliary    = NULL,
                             data,
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

  hbm_flex(family_key   = "binomial",
           response     = response,
           auxiliary    = auxiliary,
           addition_var = trials,
           data         = data,
           fixed_params = fixed_params,
           ...)
}
