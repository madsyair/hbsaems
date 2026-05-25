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
#' @param data A \code{data.frame}.
#' @param area_var Optional character vector.  Name(s) of column(s) in
#'   \code{data} identifying the small area / domain.  Length 1 fits an
#'   IID area-level random intercept \code{(1 | area_var)}; length
#'   \eqn{\geq} 2 supports hierarchical areas -- see \code{?hbm_flex}
#'   for the nested vs.\ crossed structures.  Default: \code{NULL}.
#' @param area_re_structure Either \code{"nested"} (default) or
#'   \code{"crossed"}; controls how multiple area columns combine.
#' @param spatial_var Optional character.  Name of a column identifying
#'   the spatial cluster.  Must be supplied together with
#'   \code{spatial_model} and \code{M}.  Default: \code{NULL}.
#' @param spatial_model Optional character.  Spatial dependence:
#'   \code{"car"} (default \code{car_type = "icar"}) or \code{"sar"}
#'   (default \code{sar_type = "lag"}).  Default: \code{NULL}.
#' @param car_type Optional character.  CAR sub-type passed to \pkg{brms}:
#'   \code{"escar"}, \code{"esicar"}, \code{"icar"}, or \code{"bym2"}.
#' @param sar_type Optional character.  SAR sub-type:
#'   \code{"lag"} or \code{"error"}.
#' @param M Optional numeric matrix.  Spatial weight matrix.
#'   Required when \code{spatial_model} is supplied.
#' @param fixed_params Optional named list pinning distributional
#'   parameters to known values.  See \code{\link{hbm}} for the spec
#'   format.
#' @param ... Additional arguments forwarded to \code{\link{hbm_flex}}
#'   (e.g.\ \code{prior_type}, \code{handle_missing}, sampler controls).
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
#' @param group \strong{Deprecated.}  Use \code{area_var} instead.
#' @param sre \strong{Deprecated.}  Use \code{spatial_var} instead.
#' @param sre_type \strong{Deprecated.}  Use \code{spatial_model} instead.
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
#'   area_var   = "district",        # area-level random effect (1 | district)
#'   data       = data_binlogitnorm,
#'   chains = 4, iter = 2000, warmup = 1000, refresh = 0
#' )
#'
#' # -- 2. With spatial CAR random effect -------------------------------------
#' data("adjacency_matrix_car_regency")
#' fit_car <- hbm_binlogitnorm(
#'   response      = "y",
#'   trials        = "n",
#'   auxiliary     = c("x1", "x2", "x3"),
#'   spatial_var   = "regency",
#'   spatial_model = "car",
#'   M             = adjacency_matrix_car_regency,
#'   data          = data_binlogitnorm,
#'   chains = 4, iter = 2000, warmup = 1000, refresh = 0
#' )
#' }
#'
#' @seealso \code{\link{hbm_flex}}, \code{\link{hbm}}
#' @export
hbm_binlogitnorm <- function(response,
                             trials,
                             auxiliary     = NULL,
                             data,
                             area_var      = NULL,
                             area_re_structure = c("nested", "crossed"),
                             spatial_var   = NULL,
                             spatial_model = NULL,
                             car_type      = NULL,
                             sar_type      = NULL,
                             M             = NULL,
                             fixed_params  = NULL,
                             # -- DEPRECATED aliases (v1.0.0) ----
                             predictors    = NULL,
                             group         = NULL,
                             sre           = NULL,
                             sre_type      = NULL,
                             ...) {

  # -- 0. Deprecated alias 'predictors' (v1.0.0) ----------------------------
  if (!is.null(predictors)) {
    if (!is.null(auxiliary))
      stop("Pass either `auxiliary` (preferred) or `predictors` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("predictors", "auxiliary", "v2.0.0")
    auxiliary <- predictors
  }
  if (is.null(auxiliary))
    stop("`auxiliary` (auxiliary variables) is required.", call. = FALSE)

  # -- 0a. Deprecated SAE aliases -------------------------------------------
  if (!is.null(group)) {
    if (!is.null(area_var))
      stop("Pass either `area_var` (preferred) or `group` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("group", "area_var", "v2.0.0")
    area_var <- group
  }
  if (!is.null(sre)) {
    if (!is.null(spatial_var))
      stop("Pass either `spatial_var` (preferred) or `sre` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("sre", "spatial_var", "v2.0.0")
    spatial_var <- sre
  }
  if (!is.null(sre_type)) {
    if (!is.null(spatial_model))
      stop("Pass either `spatial_model` (preferred) or `sre_type` ",
           "(deprecated), but not both.", call. = FALSE)
    .deprecate_arg("sre_type", "spatial_model", "v2.0.0")
    spatial_model <- sre_type
  }

  area_re_structure <- match.arg(area_re_structure)

  hbm_flex(family_key    = "binomial",
           response      = response,
           auxiliary     = auxiliary,
           addition_var  = trials,
           data          = data,
           area_var      = area_var,
           area_re_structure = area_re_structure,
           spatial_var   = spatial_var,
           spatial_model = spatial_model,
           car_type      = car_type,
           sar_type      = sar_type,
           M             = M,
           fixed_params  = fixed_params,
           ...)
}
