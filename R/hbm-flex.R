# R/hbm-flex.R
# =============================================================================
# Flexible HBSAE model factory.
#
# This file replaces ~80% of the boilerplate in hbm_lnln.R, hbm_binlogitnorm.R,
# and hbm_betalogitnorm.R.  Each distribution-specific wrapper now reduces to
# a thin signature shim that calls hbm_flex() with its family key.
#
# Why "flex"?  This is the multi-feature builder: family registry +
# spatial random effects (CAR/SAR) + shrinkage priors (horseshoe/R2D2)
# + smooth terms (spline/GP) + missing-data strategy, all configurable
# in a single call.  Distribution-specific wrappers are presets for the
# most common cases; hbm_flex() is the full-power entry point.
#
# Note: in pre-v1.0.0 development this function went through several name
# changes (`hbsae_wrapper`, `hbm_generic`).  Those names were never on
# CRAN -- v1.0.0 is the first stable release and exports only the final
# name `hbm_flex()`.
# =============================================================================


#' Fit a Flexible HBSAE Model with Any Registered Family
#'
#' Flexible factory that fits an HBSAE model using any distribution
#' currently registered in the family registry, together with the full
#' set of cross-cutting features: spatial random effects (CAR/SAR),
#' shrinkage priors (horseshoe, R2D2), smooth terms (penalised splines,
#' Gaussian processes), auxiliary-parameter hyperpriors, and
#' missing-data strategies.  Distribution-specific wrappers
#' (\code{\link{hbm_lnln}}, \code{\link{hbm_binlogitnorm}},
#' \code{\link{hbm_betalogitnorm}}) are thin signature shims around this
#' function with preset \code{family_key} values.  Advanced users can
#' also call it directly once a custom family has been registered via
#' \code{\link{register_hbsae_model}}.
#'
#' @param family_key Character.  The registry key of the desired family
#'   (e.g.\ \code{"lognormal"}, \code{"binomial"}, \code{"gamma_log"}).
#' @param response Character.  Name of the response variable column.
#' @param auxiliary Character vector.  Names of auxiliary (fixed-effect)
#'   variables; corresponds to area-level covariates in SAE literature.
#' @param predictors \strong{Deprecated.}  Use \code{auxiliary} instead.
#'   Kept for backward compatibility; will be removed in v2.0.0.
#' @param fixed_params Optional named list pinning distributional
#'   parameters to known values.  See \code{\link{hbm}} for the spec
#'   format.  Allows power-user access to the generic fixed-parameter
#'   machinery (works with custom families too).
#' @param sampling_variance Optional character.  Name of a column in
#'   \code{data} containing the \strong{known} sampling variance
#'   \eqn{D_i} for each area (the Fay-Herriot sugar).  When supplied,
#'   \eqn{\sigma_i = \sqrt{D_i}} is pinned via offset.  Forwarded to
#'   \code{\link{hbm}}, where a family-compatibility check ensures the
#'   family exposes a residual SD parameter named \code{sigma}
#'   (gaussian, lognormal, student, skew_normal, exgaussian,
#'   asym_laplace).  Incompatible families (beta, binomial, poisson,
#'   etc.) raise an explicit error pointing at the family-specific
#'   alternative.  See \code{?hbm} for details.
#' @param data A \code{data.frame}.
#' @param addition_var Character or \code{NULL}.  Name of the addition term
#'   variable (e.g.\ trials for binomial).  Required when the family spec
#'   has \code{has_addition_term = TRUE}.
#' @param area_var Character vector or \code{NULL}.  Name(s) of the
#'   column(s) in \code{data} identifying the areas.  Three usage modes:
#'   \itemize{
#'     \item Length 1 (default behaviour): a single area-level random
#'           intercept \code{(1 | area_var)}.
#'     \item Length \eqn{\geq} 2 with \code{area_re_structure = "nested"}
#'           (default): a hierarchy of areas given from the
#'           \emph{highest} to the \emph{lowest} level, e.g.
#'           \code{c("province", "regency")} yields
#'           \code{(1 | province / regency)} which brms expands to
#'           \code{(1 | province) + (1 | province:regency)}.  This is
#'           the canonical multi-stage SAE setup.
#'     \item Length \eqn{\geq} 2 with \code{area_re_structure = "crossed"}:
#'           non-nested levels, e.g.\ adding separate effects for
#'           \code{(1 | province) + (1 | urbanrural)}.  Use only when
#'           the levels are truly crossed rather than hierarchically
#'           nested.
#'   }
#' @param area_re_structure Either \code{"nested"} (default) or
#'   \code{"crossed"}.  Only consulted when \code{area_var} has length
#'   \eqn{\geq} 2.  See above.
#' @param spatial_var,spatial_model,car_type,sar_type,M Spatial
#'   random-effect arguments forwarded to \code{\link{hbm}}.  See
#'   \code{?hbm} for a full description.
#' @param group \strong{Deprecated.}  Use \code{area_var} instead.
#' @param sre \strong{Deprecated.}  Use \code{spatial_var} instead.
#' @param sre_type \strong{Deprecated.}  Use \code{spatial_model} instead.
#' @param prior,prior_type,hs_df,hs_df_global,hs_df_slab,hs_scale_global,hs_scale_slab,hs_par_ratio,hs_autoscale,r2d2_mean_R2,r2d2_prec_R2,r2d2_cons_D2,r2d2_autoscale
#'   Shrinkage prior arguments forwarded to \code{\link{hbm}}.  When the
#'   formula contains \code{s()} or \code{gp()} terms, the prior is
#'   automatically cascaded to the corresponding parameter classes
#'   (\code{"sds"}, \code{"sdgp"}) via the brms \code{main = TRUE} pattern.
#' @param nonlinear Character vector of variable names to include as
#'   smooth/nonlinear terms (rather than linear).  Variables listed here
#'   that also appear in \code{auxiliary} are modelled nonlinearly only.
#' @param nonlinear_type Character.  \code{"spline"} (penalised regression
#'   spline via \pkg{mgcv}, default) or \code{"gp"} (Gaussian process via
#'   \pkg{brms}).
#' @param spline_k Integer.  Spline basis dimension passed to
#'   \code{mgcv::s(..., k = ...)}.  \code{-1} (default) lets \pkg{mgcv}
#'   choose automatically.  For SAE typically \code{k = 8} to \code{15}.
#' @param spline_bs Character.  Spline basis type passed to
#'   \code{mgcv::s(..., bs = ...)}.  Defaults to \code{"tp"} (thin-plate
#'   regression spline).  Set \code{"cr"} (cubic regression) for better
#'   numerical stability with correlated auxiliary variables.  Other
#'   choices: \code{"cs"} (cubic with shrinkage), \code{"ps"} (P-splines).
#' @param gp_k Integer or \code{NA}.  Number of basis functions for the
#'   Hilbert-space approximate GP (Riutort-Mayol et al.\ 2020).
#'   \code{NA} (default) = exact GP, scales \eqn{O(n^3)} and is
#'   \strong{not recommended for} \eqn{n > 100} areas.  Integer
#'   \code{gp_k = 10}--\code{25} is typical for SAE applications and
#'   dramatically improves convergence and runtime.
#' @param gp_cov Character.  Covariance function: \code{"exp_quad"}
#'   (squared exponential / RBF, default), \code{"matern15"}
#'   (Matern 3/2), \code{"matern25"} (Matern 5/2, often more numerically
#'   stable than RBF), \code{"exponential"}.
#' @param gp_c Numeric.  Hilbert-space GP boundary-scale factor passed
#'   to \code{brms::gp(c = ...)}.  Default brms value is 5/4 (= 1.25);
#'   increase if the GP appears truncated at domain boundaries.  Only
#'   relevant when \code{gp_k} is set.
#' @param gp_scale \strong{Deprecated.}  Use \code{gp_c} instead.  The
#'   old name suggested a length-scale interpretation but actually
#'   mapped to the boundary-scale factor.
#' @param handle_missing,m,mice_args Missing-data arguments.  When
#'   \code{handle_missing = NULL} the wrapper auto-selects a strategy
#'   based on the family registry's \code{supports_mi} flag.
#' @param control,chains,iter,warmup,cores,sample_prior,link Sampler and
#'   model-spec arguments forwarded to \code{\link{hbm}}.
#' @param aux_args Optional named list of family-specific auxiliary
#'   arguments (e.g.\ \code{list(n = "n", deff = "deff")} for the Beta
#'   family's phi hyperprior).  Forwarded to the family's
#'   \code{aux_param_hyperprior} callback if it has one.
#' @param stanvars Optional \code{\link[brms]{stanvar}} object passed
#'   through to \pkg{brms}.  When the family has an
#'   \code{aux_param_hyperprior} callback that returns its own
#'   \code{stanvars}, the two are concatenated.
#' @param ... Additional arguments forwarded to \code{\link[brms]{brm}}.
#'
#' @return An object of class \code{hbmfit}.
#'
#' @details
#' The factory performs five duties that were previously duplicated across
#' wrappers:
#' \enumerate{
#'   \item Validate that \code{response}, \code{auxiliary}, and optional
#'     variables exist in \code{data}.
#'   \item Run the family's \code{response_check} on the response and
#'     report a human-readable error on failure.
#'   \item Auto-select a missing-data strategy that respects the family's
#'     \code{supports_mi} flag (e.g.\ binomial cannot use \code{"model"}).
#'   \item Build the brms formula with optional addition terms and apply
#'     spline/GP transformations.
#'   \item Invoke the family's \code{aux_param_hyperprior} callback (if
#'     defined) so distributions with a hyperprior on auxiliary parameters
#'     -- e.g.\ phi for the Beta family, shape for Gamma, nu for
#'     Student-t -- can inject Stan code without writing a thick wrapper
#'     file.
#' }
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' data("data_lnln")
#' # Equivalent to hbm_lnln(...)
#' fit <- hbm_flex(
#'   family_key = "lognormal",
#'   response   = "y_obs",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   area_var   = "district",         # area-level random effect: (1 | district)
#'   data       = data_lnln,
#'   chains = 4, iter = 2000, refresh = 0
#' )
#' }
#'
#' @seealso \code{\link{register_hbsae_model}},
#'   \code{\link{list_hbsae_models}}, \code{\link{hbm}}
#' @export
hbm_flex <- function(family_key,
                          response,
                          auxiliary       = NULL,
                          data,
                          addition_var    = NULL,
                          area_var        = NULL,
                          area_re_structure = c("nested", "crossed"),
                          spatial_var     = NULL,
                          spatial_model   = NULL,
                          car_type        = NULL,
                          sar_type        = NULL,
                          M               = NULL,
                          prior           = NULL,
                          # Fixed-value distributional parameters
                          fixed_params    = NULL,
                          # Fay-Herriot sampling variance (continuous families)
                          sampling_variance = NULL,
                          # Shrinkage priors
                          prior_type      = "default",
                          hs_df           = 1,
                          hs_df_global    = 1,
                          hs_df_slab      = 4,
                          hs_scale_global = NULL,
                          hs_scale_slab   = 2,
                          hs_par_ratio    = NULL,
                          hs_autoscale    = TRUE,
                          r2d2_mean_R2    = 0.5,
                          r2d2_prec_R2    = 2,
                          r2d2_cons_D2    = NULL,
                          r2d2_autoscale  = TRUE,
                          # Nonlinear terms
                          nonlinear       = NULL,
                          nonlinear_type  = "spline",
                          spline_k        = -1L,
                          spline_bs       = "tp",
                          gp_k            = NA_integer_,
                          gp_cov          = "exp_quad",
                          gp_c            = NULL,
                          # Deprecated nonlinear alias (v1.0.0)
                          gp_scale        = NULL,
                          # Missing data
                          handle_missing  = NULL,
                          m               = 5L,
                          mice_args       = list(),
                          # Sampler
                          control         = list(),
                          chains          = 4L,
                          iter            = 4000L,
                          warmup          = floor(iter / 2),
                          cores           = 1L,
                          sample_prior    = "no",
                          link            = NULL,
                          # Auxiliary parameter hyperprior (Tier 2)
                          aux_args        = NULL,
                          stanvars        = NULL,
                          # DEPRECATED: kept for backward compatibility (v1.0.0 -> v2.0.0)
                          predictors      = NULL,
                          group           = NULL,
                          sre             = NULL,
                          sre_type        = NULL,
                          ...) {

  # -- 0. Deprecated alias 'predictors' -----------------------------
  # The argument was renamed from `predictors` (brms idiom) to `auxiliary`
  # (Small Area Estimation idiom; see Rao & Molina 2015 Ch. 4).  Emit a
  # one-time soft deprecation warning if the old name is used, then map to
  # the new name.  Scheduled for removal scheduled for v2.0.0.
  if (!is.null(predictors)) {
    if (!is.null(auxiliary))
      stop("Pass either `auxiliary` (preferred) or `predictors` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("predictors", "auxiliary", "v2.0.0")
    auxiliary <- predictors
  }
  if (is.null(auxiliary))
    stop("`auxiliary` (auxiliary variables) is required.", call. = FALSE)

  area_re_structure <- match.arg(area_re_structure)

  # -- 0b. Deprecated aliases (v1.0.0): group -> area_var,
  #         sre -> spatial_var, sre_type -> spatial_model
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

  # -- 1. Look up family spec ------------------------------------------------
  spec <- .get_model(family_key)
  if (is.null(spec))
    stop("Unknown family key: '", family_key,
         "'. Use list_hbsae_models() to see registered keys.",
         call. = FALSE)

  # Allow link override per-call; otherwise use family default
  use_link <- link %||% spec$link %||% "identity"

  # -- 2. Variable presence --------------------------------------------------
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.", call. = FALSE)
  if (!(response %in% names(data)))
    stop(sprintf("Response variable '%s' not found in 'data'.", response),
         call. = FALSE)

  missing_aux <- auxiliary[!auxiliary %in% names(data)]
  if (length(missing_aux) > 0L)
    stop(sprintf("Auxiliary variable(s) not found in 'data': %s",
                 paste(missing_aux, collapse = ", ")),
         call. = FALSE)

  # area_var may be a length-1 OR length>=2 character vector (the latter
  # for nested/crossed area structures), so use set-difference rather than
  # the scalar `%in%` check which would fail with R 4.2+ on length>1
  # logical coercion in `&&`.
  if (!is.null(area_var)) {
    missing_area <- setdiff(area_var, names(data))
    if (length(missing_area) > 0L)
      stop(sprintf("Area variable(s) not found in 'data': %s",
                   paste(shQuote(missing_area), collapse = ", ")),
           call. = FALSE)
  }
  if (!is.null(spatial_var)) {
    # spatial_var is always length-1 by API design, but use the same
    # robust idiom defensively.
    missing_sp <- setdiff(spatial_var, names(data))
    if (length(missing_sp) > 0L)
      stop(sprintf("Spatial variable(s) not found in 'data': %s",
                   paste(shQuote(missing_sp), collapse = ", ")),
           call. = FALSE)
  }

  # -- 3. Addition-term variable (e.g. trials for binomial) ------------------
  if (isTRUE(spec$has_addition_term)) {
    if (is.null(addition_var))
      stop(sprintf(
        "Family '%s' requires an addition-term variable (e.g. 'trials').",
        family_key), call. = FALSE)
    if (!(addition_var %in% names(data)))
      stop(sprintf("Addition variable '%s' not found in 'data'.",
                   addition_var), call. = FALSE)
  }

  # -- 4. Response domain check ---------------------------------------------
  if (is.function(spec$response_check) &&
      !isTRUE(spec$response_check(data[[response]]))) {
    msg <- spec$response_check_msg %||%
      sprintf("Response variable '%s' fails the domain check for family '%s'.",
              response, family_key)
    stop(msg, call. = FALSE)
  }

  # Binomial-style: also validate addition var (positive integer)
  if (isTRUE(spec$has_addition_term) && family_key == "binomial") {
    n_vals <- data[[addition_var]]
    if (any(n_vals <= 0, na.rm = TRUE))
      stop(sprintf("Trials variable '%s' must be strictly positive.",
                   addition_var), call. = FALSE)
    if (any(abs(n_vals - round(n_vals)) > .Machine$double.eps^0.5,
            na.rm = TRUE))
      stop(sprintf("Trials variable '%s' must contain integer values.",
                   addition_var), call. = FALSE)
    y_vals <- data[[response]]
    both   <- !is.na(y_vals) & !is.na(n_vals)
    if (any(y_vals[both] > n_vals[both]))
      stop(sprintf("Successes ('%s') must not exceed trials ('%s').",
                   response, addition_var), call. = FALSE)
  }

  # -- 5. Validate handle_missing against family capability ------------------
  if (!is.null(handle_missing) &&
      handle_missing == "model" &&
      !isTRUE(spec$supports_mi)) {
    stop(sprintf(paste0(
      "handle_missing = 'model' (joint Bayesian imputation via mi()) is ",
      "not supported for the '%s' family. Use 'multiple' or 'deleted' ",
      "instead."), family_key), call. = FALSE)
  }

  # -- 6. Auto-select handle_missing when NULL ------------------------------
  if (is.null(handle_missing)) {
    has_y_na <- anyNA(data[[response]])
    has_x_na <- any(vapply(auxiliary,
                            function(v) anyNA(data[[v]]),
                            logical(1L)))
    if (has_y_na || has_x_na) {
      handle_missing <- if (isTRUE(spec$supports_mi)) "model" else "multiple"
      message(sprintf(paste0(
        "Missing values detected. Auto-selecting handle_missing = '%s' ",
        "(family '%s', supports_mi = %s)."),
        handle_missing, family_key, spec$supports_mi))
    }
  }

  # -- 7. Nonlinear validation -----------------------------------------------
  if (!is.null(nonlinear) && length(nonlinear) > 0L) {
    nonlinear_type <- match.arg(nonlinear_type, c("spline", "gp"))
    .validate_nonlinear(nonlinear, auxiliary, data)
    if (nonlinear_type == "spline" &&
        !requireNamespace("mgcv", quietly = TRUE))
      stop("Package 'mgcv' is required for nonlinear_type = 'spline'. ",
           "Install with: install.packages('mgcv')", call. = FALSE)
  }

  # -- 7b. Deprecated `gp_scale` alias (v1.0.0) -----------------------------
  # gp_scale was a misnomer (suggested length-scale, actually mapped to brms
  # `c` parameter = HSGP boundary-scale factor).  Renamed for clarity.
  if (!is.null(gp_scale)) {
    if (!is.null(gp_c))
      stop("Pass either `gp_c` (preferred) or `gp_scale` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("gp_scale", "gp_c", "v2.0.0")
    gp_c <- gp_scale
  }

  # -- 8. Build formula ------------------------------------------------------
  rhs_str <- .build_wrapper_rhs(
    auxiliary      = auxiliary,
    nonlinear      = nonlinear,
    nonlinear_type = nonlinear_type,
    spline_k       = spline_k,
    spline_bs      = spline_bs,
    gp_k           = gp_k,
    gp_cov         = gp_cov,
    gp_c           = gp_c
  )

  if (isTRUE(spec$has_addition_term)) {
    fml_str <- sprintf(spec$addition_template,
                        response, addition_var, rhs_str)
  } else {
    fml_str <- paste(response, "~", rhs_str)
  }
  base_formula <- brms::bf(stats::as.formula(fml_str))

  # -- 9. Random-effect formula ---------------------------------------------
  # `area_var` may be a single column or a vector describing a hierarchy
  # (highest level first, e.g. c("province", "regency")).  See
  # ?hbm_flex's @param area_var for the supported structures.
  .check_area_var_columns(area_var, data)
  formula_re <- .build_area_re_formula(area_var,
                                        structure = area_re_structure)

  # -- 10. Family-specific default priors -----------------------------------
  fam_priors <- if (is.function(spec$default_priors))
    tryCatch(spec$default_priors(), error = function(e) NULL)
  else NULL
  effective_prior <- if (is.null(prior)) fam_priors else prior

  # -- 10b. Auxiliary parameter hyperprior (Tier 2) -------------------------
  # Distributions like Beta have an auxiliary parameter (phi) with a
  # hyperprior expressed via stanvars.  When the family spec defines an
  # aux_param_hyperprior callback AND the user passes aux_args, invoke it
  # to obtain (prior, stanvars) which we merge with the user-supplied
  # versions.
  if (is.function(spec$aux_param_hyperprior) && !is.null(aux_args)) {
    aux_out <- spec$aux_param_hyperprior(args = aux_args, data = data)
    if (!is.null(aux_out)) {
      if (!is.null(aux_out$prior)) {
        effective_prior <- if (is.null(effective_prior))
          aux_out$prior
        else
          c(effective_prior, aux_out$prior)
      }
      if (!is.null(aux_out$stanvars)) {
        stanvars <- if (is.null(stanvars))
          aux_out$stanvars
        else
          stanvars + aux_out$stanvars
      }
    }
  }

  # -- 11. Delegate to hbm() -------------------------------------------------
  hbm(
    formula         = base_formula,
    hb_sampling     = spec$family,
    hb_link         = use_link,
    re              = formula_re,
    spatial_var     = spatial_var,
    spatial_model   = spatial_model,
    car_type        = car_type,
    sar_type        = sar_type,
    M               = M,
    prior           = effective_prior,
    fixed_params    = fixed_params,           # forward fixed params
    sampling_variance = sampling_variance,    # Fay-Herriot sugar — family-validated inside hbm()
    stanvars        = stanvars,
    prior_type      = prior_type,
    hs_df           = hs_df,
    hs_df_global    = hs_df_global,
    hs_df_slab      = hs_df_slab,
    hs_scale_global = hs_scale_global,
    hs_scale_slab   = hs_scale_slab,
    hs_par_ratio    = hs_par_ratio,
    hs_autoscale    = hs_autoscale,
    r2d2_mean_R2    = r2d2_mean_R2,
    r2d2_prec_R2    = r2d2_prec_R2,
    r2d2_cons_D2    = r2d2_cons_D2,
    r2d2_autoscale  = r2d2_autoscale,
    handle_missing  = handle_missing,
    m               = m,
    mice_args       = mice_args,
    data            = data,
    control         = control,
    chains          = chains,
    iter            = iter,
    warmup          = warmup,
    cores           = cores,
    sample_prior    = sample_prior,
    ...
  )
}
