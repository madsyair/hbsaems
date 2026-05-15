# R/deprecated.R
# =============================================================================
# DEPRECATED FUNCTIONS  --  retained for backwards compatibility.
#
# All four functions emit a .Deprecated() warning (once per session by R's
# default behaviour) and forward to their primary replacement.  They are
# scheduled for removal scheduled for v2.0.0.
# =============================================================================

#' Deprecated Functions
#'
#' These functions were the primary API in \pkg{hbsaems} \eqn{\le} 0.2.x.
#' They are retained for backwards compatibility but now call the new
#' primary functions and emit a deprecation warning via
#' \code{\link[base]{.Deprecated}}.  They will be removed in
#' \strong{v2.0.0}.
#'
#' @section Migration guide:
#' \tabular{ll}{
#'   \strong{Deprecated} \tab \strong{Replacement} \cr
#'   \code{hbcc(model, ...)} \tab \code{\link{convergence_check}(model, ...)} \cr
#'   \code{hbmc(model, ...)} \tab \code{\link{model_compare}(model, ...)} \cr
#'   \code{hbpc(model, data, response_var)} \tab
#'     \code{\link{prior_check}(model, data, response_var)} \cr
#'   \code{hbsae(model, ...)} \tab \code{\link{sae_predict}(model, ...)} \cr
#' }
#'
#' @param model       An \code{hbmfit} object.
#' @param diag_tests,plot_types,ndraws_ppc,moment_match,moment_match_args,reloo_args,comparison_metrics,run_prior_sensitivity,sensitivity_vars
#'   Forwarded unchanged to the replacement function.
#' @param data        For \code{hbpc}: the data \code{data.frame}.
#' @param response_var For \code{hbpc}: name of the response variable.
#' @param newdata     For \code{hbsae}: optional new data.
#' @param ...         Additional arguments forwarded to the replacement.
#'
#' @return Identical to the corresponding replacement function.
#'
#' @name deprecated
NULL


# -- hbcc ----------------------------------------------------------------------

#' @rdname deprecated
#' @export
hbcc <- function(model,
                 diag_tests = c("rhat", "geweke", "heidel", "raftery"),
                 plot_types = c("trace", "dens", "acf",
                                "nuts_energy", "rhat", "neff"),
                 ...) {
  .Deprecated(
    new     = "convergence_check",
    package = "hbsaems",
    msg     = paste0(
      "'hbcc()' is deprecated as of hbsaems 0.3.0 and will be ",
      "removed in v2.0.0.\n",
      "Please use 'convergence_check()' instead.\n",
      "See help(\"deprecated\", package = \"hbsaems\")."
    )
  )
  convergence_check(model,
                    diag_tests = diag_tests,
                    plot_types = plot_types,
                    ...)
}


# -- hbmc ----------------------------------------------------------------------

#' @rdname deprecated
#' @param model2 For \code{hbmc}: optional second model.
#' @export
hbmc <- function(model,
                 model2                = NULL,
                 ndraws_ppc            = 100,
                 moment_match          = FALSE,
                 moment_match_args     = list(),
                 reloo_args            = list(),
                 plot_types            = c("pp_check", "params"),
                 comparison_metrics    = c("loo", "waic", "bf"),
                 run_prior_sensitivity = FALSE,
                 sensitivity_vars      = NULL,
                 ...) {
  .Deprecated(
    new     = "model_compare",
    package = "hbsaems",
    msg     = paste0(
      "'hbmc()' is deprecated as of hbsaems 0.3.0 and will be ",
      "removed in v2.0.0.\n",
      "Please use 'model_compare()' instead.\n",
      "See help(\"deprecated\", package = \"hbsaems\")."
    )
  )
  model_compare(model,
                model2                = model2,
                ndraws_ppc            = ndraws_ppc,
                moment_match          = moment_match,
                moment_match_args     = moment_match_args,
                reloo_args            = reloo_args,
                plot_types            = plot_types,
                comparison_metrics    = comparison_metrics,
                run_prior_sensitivity = run_prior_sensitivity,
                sensitivity_vars      = sensitivity_vars,
                ...)
}


# -- hbpc ----------------------------------------------------------------------

#' @rdname deprecated
#' @export
hbpc <- function(model, data, response_var, ndraws_ppc = 50, ...) {
  .Deprecated(
    new     = "prior_check",
    package = "hbsaems",
    msg     = paste0(
      "'hbpc()' is deprecated as of hbsaems 0.3.0 and will be ",
      "removed in v2.0.0.\n",
      "Please use 'prior_check()' instead.\n",
      "See help(\"deprecated\", package = \"hbsaems\")."
    )
  )
  prior_check(model,
              data         = data,
              response_var = response_var,
              ndraws_ppc   = ndraws_ppc,
              ...)
}


# -- hbsae ---------------------------------------------------------------------

#' @rdname deprecated
#' @export
hbsae <- function(model, newdata = NULL, ...) {
  .Deprecated(
    new     = "sae_predict",
    package = "hbsaems",
    msg     = paste0(
      "'hbsae()' is deprecated as of hbsaems 0.3.0 and will be ",
      "removed in v2.0.0.\n",
      "Please use 'sae_predict()' instead.\n",
      "See help(\"deprecated\", package = \"hbsaems\")."
    )
  )
  sae_predict(model, newdata = newdata, ...)
}
