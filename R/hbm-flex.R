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
# Naming history: this function was named `hbsae_wrapper()` in v0.3.x,
# `hbm_generic()` in v0.4.0 -- v0.5.0, and `hbm_flex()` from v0.5.1+.
# The old names were retained as deprecated aliases up to v0.5.0 and
# REMOVED in v0.5.1.  Existing code that called `hbsae_wrapper()` or
# `hbm_generic()` must be updated to `hbm_flex()`.
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
#' @param data A \code{data.frame}.
#' @param addition_var Character or \code{NULL}.  Name of the addition term
#'   variable (e.g.\ trials for binomial).  Required when the family spec
#'   has \code{has_addition_term = TRUE}.
#' @param group,sre,sre_type,car_type,sar_type,M Random/spatial effect
#'   arguments forwarded to \code{\link{hbm}}.
#' @param prior,prior_type,hs_df,hs_df_global,hs_df_slab,hs_scale_global,hs_scale_slab,hs_par_ratio,r2d2_mean_R2,r2d2_prec_R2,r2d2_cons_D2
#'   Shrinkage prior arguments forwarded to \code{\link{hbm}}.
#' @param nonlinear,nonlinear_type,spline_k,gp_scale Nonlinear-term
#'   arguments forwarded to \code{\link{hbm}}.
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
#'   group      = "group",            # area-level random effect: (1 | group)
#'   data       = data_lnln,
#'   chains = 2, iter = 1000, refresh = 0
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
                          group           = NULL,
                          sre             = NULL,
                          sre_type        = NULL,
                          car_type        = NULL,
                          sar_type        = NULL,
                          M               = NULL,
                          prior           = NULL,
                          # v0.6.1: Fixed-value distributional parameters
                          fixed_params    = NULL,
                          # Shrinkage priors
                          prior_type      = "default",
                          hs_df           = 1,
                          hs_df_global    = 1,
                          hs_df_slab      = 4,
                          hs_scale_global = NULL,
                          hs_scale_slab   = 2,
                          hs_par_ratio    = NULL,
                          r2d2_mean_R2    = 0.5,
                          r2d2_prec_R2    = 2,
                          r2d2_cons_D2    = NULL,
                          # Nonlinear terms
                          nonlinear       = NULL,
                          nonlinear_type  = "spline",
                          spline_k        = -1L,
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
                          ...) {

  # -- 0. v0.6.1: deprecated alias 'predictors' -----------------------------
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

  if (!is.null(group) && !(group %in% names(data)))
    stop(sprintf("Group variable '%s' not found in 'data'.", group),
         call. = FALSE)
  if (!is.null(sre) && !(sre %in% names(data)))
    stop(sprintf("Spatial variable '%s' not found in 'data'.", sre),
         call. = FALSE)

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

  # -- 8. Build formula ------------------------------------------------------
  rhs_str <- .build_wrapper_rhs(
    predictors     = auxiliary,
    nonlinear      = nonlinear,
    nonlinear_type = nonlinear_type,
    spline_k       = spline_k,
    gp_scale       = gp_scale
  )

  if (isTRUE(spec$has_addition_term)) {
    fml_str <- sprintf(spec$addition_template,
                        response, addition_var, rhs_str)
  } else {
    fml_str <- paste(response, "~", rhs_str)
  }
  base_formula <- brms::bf(stats::as.formula(fml_str))

  # -- 9. Random-effect formula ---------------------------------------------
  formula_re <- if (!is.null(group))
    stats::as.formula(paste("~", paste0("(1 | ", group, ")")))
  else
    NULL

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
    sre             = sre,
    sre_type        = sre_type,
    car_type        = car_type,
    sar_type        = sar_type,
    M               = M,
    prior           = effective_prior,
    fixed_params    = fixed_params,           # v0.6.1: forward fixed params
    stanvars        = stanvars,
    prior_type      = prior_type,
    hs_df           = hs_df,
    hs_df_global    = hs_df_global,
    hs_df_slab      = hs_df_slab,
    hs_scale_global = hs_scale_global,
    hs_scale_slab   = hs_scale_slab,
    hs_par_ratio    = hs_par_ratio,
    r2d2_mean_R2    = r2d2_mean_R2,
    r2d2_prec_R2    = r2d2_prec_R2,
    r2d2_cons_D2    = r2d2_cons_D2,
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
