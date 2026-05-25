# R/prior-sensitivity.R
# =============================================================================
# Power-scale prior sensitivity diagnostics (Kallioinen et al. 2023)
#
# Thin wrapper around priorsense::powerscale_sensitivity() that handles
# the hbmfit -> brmsfit unwrapping and provides SAE-relevant defaults.
# =============================================================================

#' Power-Scale Prior Sensitivity Diagnostics for Fitted HBMs
#'
#' Computes prior and likelihood power-scaling sensitivity diagnostics for
#' a fitted \code{hbmfit} model using the \pkg{priorsense} package.
#' Useful for assessing whether posterior conclusions are driven by the
#' prior or the data -- a critical step in any principled Bayesian SAE
#' workflow.
#'
#' @details
#' Prior sensitivity analysis answers the question: ``If I had used a
#' slightly different prior, would the substantive conclusions change?''.
#' The power-scaling approach of Kallioinen et al.\ (2023) detects:
#' \itemize{
#'   \item \strong{Prior--likelihood conflict}: the posterior moves
#'     non-negligibly when the prior is up- or down-weighted.  Often
#'     indicates an overly informative or misspecified prior.
#'   \item \strong{Weak likelihood}: the posterior is dominated by the
#'     prior.  Common in SAE for areas with few sampled units.
#' }
#'
#' Reported diagnostics include the Kullback--Leibler divergence between
#' the original posterior and the power-scaled posterior (\code{prior},
#' \code{likelihood}) and a categorical flag (\code{prior-data conflict},
#' \code{strong prior}, \code{-}).
#'
#' \strong{Computational cost.}  No re-sampling is required: importance
#' sampling reuses the existing posterior draws.  Hence a typical run
#' costs only a few seconds even for large hierarchical models.
#'
#' @param model An \code{hbmfit} object returned by \code{\link{hbm}}
#'   (or one of its wrappers) or a \code{brmsfit} object directly.
#' @param ...   Additional arguments forwarded to
#'   \code{priorsense::powerscale_sensitivity()}, e.g.\
#'   \code{variable = c("b_x1", "sd_regency__Intercept")} to restrict
#'   the report to specific parameters.
#'
#' @return A \code{powerscale_sensitivity_summary} object (data frame)
#'   with one row per monitored parameter and columns
#'   \code{variable}, \code{prior}, \code{likelihood}, \code{diagnosis}.
#'   \code{NULL} (with a message) when the \pkg{priorsense} package is
#'   not installed.
#'
#' @section When to run prior sensitivity:
#' Always.  Specifically:
#' \itemize{
#'   \item After every model fit, before drawing substantive conclusions.
#'   \item Whenever convergence diagnostics from
#'         \code{\link{convergence_check}()} are clean but the posterior
#'         seems implausibly narrow or implausibly wide.
#'   \item When comparing models with shrinkage priors -- horseshoe
#'         and R2D2 are both informative, and small differences in
#'         their hyperparameters can move estimates noticeably.
#' }
#'
#' @references
#' Kallioinen, N., Paananen, T., Burkner, P.-C., & Vehtari, A.\ (2024).
#' Detecting and diagnosing prior and likelihood sensitivity with
#' power-scaling.  \emph{Statistics and Computing}, 34, 57.
#' \doi{10.1007/s11222-023-10366-5}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("priorsense", quietly = TRUE)) {
#'   data("data_fhnorm")
#'   fit <- hbm(brms::bf(y ~ x1 + x2),
#'              data = data_fhnorm, re = ~(1 | regency),
#'              chains = 4, iter = 2000, refresh = 0)
#'   ps  <- prior_sensitivity(fit)
#'   print(ps)
#' }
#' }
#'
#' @seealso \code{\link{prior_check}}, \code{\link{convergence_check}},
#'   \code{\link{model_compare}}
#' @export
prior_sensitivity <- function(model, ...) {

  # Input validation first (so tests work regardless of priorsense install)
  if (!is.hbmfit(model) && !inherits(model, "brmsfit"))
    stop("'model' must be an hbmfit or brmsfit object.", call. = FALSE)

  if (!requireNamespace("priorsense", quietly = TRUE)) {
    message("Package 'priorsense' is required for prior sensitivity ",
            "analysis. Install with: install.packages('priorsense'). ",
            "Returning NULL.")
    return(invisible(NULL))
  }

  brms_model <- if (is.hbmfit(model)) model$model else model

  res <- tryCatch(
    priorsense::powerscale_sensitivity(brms_model, ...),
    error = function(e) {
      stop("priorsense::powerscale_sensitivity() failed: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  res
}
