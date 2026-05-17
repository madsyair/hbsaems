# R/hbm_lnln.R
# =============================================================================
# Convenience wrapper for SAE under a Lognormal-Lognormal model.
#
# Supports two modes for the residual standard deviation `sigma`:
#
#   (1) RANDOM (default)  - sigma is sampled with brms's default prior or a
#       user-supplied prior.
#   (2) FIXED via known sampling variance - if the user supplies
#       `sampling_variance` (the column name of psi_i, the known sampling
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
#' When the user supplies \code{sampling_variance = "psi_i"} (the column name of
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
#' @param area_var Optional character.  Name of a column in \code{data}
#'   identifying the small area / domain.  When supplied, an IID
#'   area-level random effect \code{(1 | area_var)} is added to the
#'   formula.  Default: \code{NULL}.
#' @param spatial_var Optional character.  Name of a column in \code{data}
#'   identifying the spatial cluster (e.g. province).  Must be supplied
#'   together with \code{spatial_model} and \code{M}.  Default: \code{NULL}.
#' @param spatial_model Optional character.  Type of spatial dependence:
#'   \code{"car"} (conditional autoregressive) or \code{"sar"}
#'   (simultaneous autoregressive).  Default: \code{NULL}.
#' @param car_type Optional character.  CAR sub-type passed to \pkg{brms}:
#'   \code{"escar"}, \code{"esicar"}, \code{"icar"} (intrinsic CAR;
#'   default when \code{spatial_model = "car"}), or \code{"bym2"}.
#' @param sar_type Optional character.  SAR sub-type:
#'   \code{"lag"} (spatial-lag, default when \code{spatial_model = "sar"})
#'   or \code{"error"} (spatial-error).
#' @param M Optional numeric matrix.  Spatial weight matrix.  Required
#'   when \code{spatial_model} is supplied.
#' @param sampling_variance Optional character.  Name of a column in \code{data}
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
#'   \code{sampling_variance}.
#' @param ... Additional arguments forwarded to \code{\link{hbm_flex}}
#'   (e.g.\ \code{prior_type}, \code{nonlinear}, \code{handle_missing},
#'   sampler controls such as \code{chains}, \code{iter}, \code{cores},
#'   \code{seed}).
#'
#' @section Conflict policy:
#' When the residual standard deviation \eqn{\sigma_i = \sqrt{\psi_i}}
#' is pinned via \code{sampling_variance} (or via
#' \code{fixed_params$sigma}), the function refuses any additional
#' specification that would also set \eqn{\sigma}.  Specifically, all
#' of the following are rejected with an informative error at
#' construction time:
#' \itemize{
#'   \item \code{sampling_variance} \emph{and} \code{fixed_params$sigma}.
#'   \item \code{sampling_variance} \emph{and} a user \code{prior} on
#'         \code{class = "sigma"}.
#'   \item \code{sampling_variance} \emph{and} a \code{stanvars} sampling
#'         statement involving \code{sigma}.
#'   \item \code{auxiliary} \emph{and} the deprecated \code{predictors}
#'         in the same call.
#' }
#'
#' @param group \strong{Deprecated.}  Use \code{area_var} instead.
#' @param sre \strong{Deprecated.}  Use \code{spatial_var} instead.
#' @param sre_type \strong{Deprecated.}  Use \code{spatial_model} instead.
#' @param sampling_var \strong{Deprecated.}  Use \code{sampling_variance}
#'   instead.  Kept for backward compatibility; will be removed in v2.0.0.
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
#'   area_var   = "district",
#'   data       = data_lnln,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#'
#' # -- 2. Fay-Herriot style with known sampling variance ----------------------
#' #     (assumes psi_i column is available)
#' fit2 <- hbm_lnln(
#'   response     = "y_obs",
#'   auxiliary    = c("x1", "x2", "x3"),
#'   area_var     = "district",
#'   sampling_variance = "psi_i",
#'   data         = data_lnln,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' }
#'
#' @seealso \code{\link{hbm_flex}}, \code{\link{hbm}}
#' @export
hbm_lnln <- function(response,
                     auxiliary         = NULL,
                     data,
                     area_var          = NULL,
                     spatial_var       = NULL,
                     spatial_model     = NULL,
                     car_type          = NULL,
                     sar_type          = NULL,
                     M                 = NULL,
                     sampling_variance = NULL,
                     fixed_params      = NULL,
                     # -- DEPRECATED aliases (v1.0.0) ----
                     predictors        = NULL,
                     sampling_var      = NULL,
                     group             = NULL,
                     sre               = NULL,
                     sre_type          = NULL,
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

  # -- 0a. Deprecated SAE aliases (group/sre/sre_type) -----------------------
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

  # -- 0b. Deprecated alias 'sampling_var' (v1.0.0) -------------------------
  # The argument was renamed to disambiguate the "_var" suffix elsewhere in
  # the package (e.g. `area_var`, `spatial_var`) which conventionally means
  # "variable name (= column name)".  Here "var" abbreviated "variance",
  # which was confusing.  `sampling_variance` is the SAE-canonical spelling.
  if (!is.null(sampling_var)) {
    if (!is.null(sampling_variance))
      stop("Pass either `sampling_variance` (preferred) or `sampling_var` ",
           "(deprecated), but not both.", call. = FALSE)
    .deprecate_arg("sampling_var", "sampling_variance", "v2.0.0")
    sampling_variance <- sampling_var
  }

  # -- 1. If sampling_variance provided, translate to fixed_params ----------
  if (!is.null(sampling_variance)) {
    if (!is.character(sampling_variance) || length(sampling_variance) != 1L)
      stop("`sampling_variance` must be a single column name (character).",
           call. = FALSE)
    if (!sampling_variance %in% names(data))
      stop(sprintf("`sampling_variance = \"%s\"` not found in `data`.",
                    sampling_variance),
           call. = FALSE)
    psi <- data[[sampling_variance]]
    if (any(is.na(psi) | psi <= 0))
      stop("`sampling_variance` must contain finite, strictly positive values.",
           call. = FALSE)

    # Refuse conflict with user-supplied fixed_params on the same param
    if (is.list(fixed_params) && "sigma" %in% names(fixed_params))
      stop("Cannot supply both `sampling_variance` and `fixed_params$sigma`. ",
           "Pick one.", call. = FALSE)

    # sigma = sqrt(psi)
    fp <- fixed_params %||% list()
    fp$sigma <- sqrt(psi)
    fixed_params <- fp
  }

  hbm_flex(family_key    = "lognormal",
           response      = response,
           auxiliary     = auxiliary,
           data          = data,
           area_var      = area_var,
           spatial_var   = spatial_var,
           spatial_model = spatial_model,
           car_type      = car_type,
           sar_type      = sar_type,
           M             = M,
           fixed_params  = fixed_params,
           ...)
}
