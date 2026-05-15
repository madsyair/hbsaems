# R/hbmfit-methods.R
# =============================================================================
# Standard S3 generics for hbmfit and the four result classes.
# Following the same convention as brmsfit in brms.
# =============================================================================

#' Standard S3 Methods for hbmfit
#'
#' These methods allow \code{hbmfit} objects to be used with familiar base-R
#' generics -- \code{summary()}, \code{plot()}, \code{coef()}, etc. -- in the
#' same way as \code{brmsfit} objects from \pkg{brms}.
#'
#' @param object,x An \code{hbmfit} object.
#' @param type     Plot type.  See Details for available options.
#' @param newdata  Optional new data frame for predictions.
#' @param ...      Additional arguments passed to the underlying \pkg{brms}
#'   method.
#'
#' @return Varies by method; see \pkg{brms} documentation for the underlying
#'   return types.
#'
#' @importFrom stats predict fitted residuals coef vcov formula terms family
#' @importFrom stats nobs logLik update variable.names
#' @importFrom ggplot2 .data
#'
#' @name hbmfit-methods
NULL


# -- Prediction / fitted -------------------------------------------------------

#' @method predict hbmfit
#' @export
predict.hbmfit <- function(object, newdata = NULL, ...) {
  predict(object$model, newdata = newdata, ...)
}

#' @method fitted hbmfit
#' @export
fitted.hbmfit <- function(object, ...) fitted(object$model, ...)

#' @method residuals hbmfit
#' @export
residuals.hbmfit <- function(object, ...) residuals(object$model, ...)


# -- Coefficient extraction ----------------------------------------------------

#' @method fixef hbmfit
#' @importFrom brms fixef
#' @export
fixef.hbmfit <- function(object, ...) brms::fixef(object$model, ...)

#' @method coef hbmfit
#' @export
coef.hbmfit <- function(object, ...) brms::fixef(object$model, ...)

#' @method ranef hbmfit
#' @importFrom brms ranef
#' @export
ranef.hbmfit <- function(object, ...) brms::ranef(object$model, ...)

#' @method vcov hbmfit
#' @export
vcov.hbmfit <- function(object, ...) vcov(object$model, ...)


# -- Formula / model structure -------------------------------------------------

#' @method formula hbmfit
#' @export
formula.hbmfit <- function(x, ...) stats::formula(x$model, ...)

#' @method terms hbmfit
#' @export
terms.hbmfit <- function(x, ...) stats::terms(stats::formula(x$model), ...)

#' @method family hbmfit
#' @export
family.hbmfit <- function(object, ...) stats::family(object$model, ...)

#' @method nobs hbmfit
#' @export
nobs.hbmfit <- function(object, ...) nrow(object$data)

#' @method logLik hbmfit
#' @export
logLik.hbmfit <- function(object, ...) brms::log_lik(object$model, ...)

#' @method variable.names hbmfit
#' @export
variable.names.hbmfit <- function(object, ...) names(object$data)


# -- Update --------------------------------------------------------------------

#' @method update hbmfit
#' @export
update.hbmfit <- function(object, ...) update_hbm(object, ...)


# -- Plot ----------------------------------------------------------------------

#' Plot a Fitted hbmfit Object
#'
#' Wraps \code{brms::mcmc_plot()} and \code{brms::pp_check()} with named plot
#' types.
#'
#' @param x   An \code{hbmfit} object.
#' @param type Plot type, one of: \code{"trace"} (trace plots),
#'   \code{"density"} (posterior densities), \code{"acf"}
#'   (autocorrelation), \code{"nuts_energy"} (NUTS energy),
#'   \code{"rhat"} (R-hat distribution), \code{"neff"} (effective sample
#'   size), \code{"pp_check"} (posterior predictive check).
#' @param ... Additional arguments passed to the underlying \pkg{brms}
#'   plotting function.
#'
#' @return A \code{ggplot} or \code{bayesplot} object.
#'
#' @method plot hbmfit
#' @export
plot.hbmfit <- function(x,
                        type = c("trace", "density", "acf",
                                 "nuts_energy", "rhat", "neff",
                                 "pp_check"),
                        ...) {
  type <- match.arg(type)
  switch(
    type,
    trace       = brms::mcmc_plot(x$model, type = "trace",       ...),
    density     = brms::mcmc_plot(x$model, type = "dens",        ...),
    acf         = brms::mcmc_plot(x$model, type = "acf",         ...),
    nuts_energy = brms::mcmc_plot(x$model, type = "nuts_energy", ...),
    rhat        = brms::mcmc_plot(x$model, type = "rhat",        ...),
    neff        = brms::mcmc_plot(x$model, type = "neff",        ...),
    pp_check    = brms::pp_check(x$model, ...)
  )
}


# -- Print / Summary -----------------------------------------------------------

#' @method print hbmfit
#' @export
print.hbmfit <- function(x, ...) {
  f <- tryCatch(stats::family(x),
                error = function(e) list(family = "?", link = "?"))
  cat("\nFitted Hierarchical Bayesian Model  [hbmfit]\n")
  cat("---------------------------------------------\n")
  cat(" Observations :", nrow(x$data), "\n")
  cat(" Family       :", f$family, "(link:", f$link, ")\n")
  if (!is.null(x$missing_method))
    cat(" Missing data :", x$missing_method, "\n")
  cat("\nUseful next steps:\n")
  cat("  convergence_check(model)   |  sae_predict(model)\n")
  cat("  model_compare(m1, m2)      |  hbm_info(model)\n\n")
  invisible(x)
}

#' @method summary hbmfit
#' @export
summary.hbmfit <- function(object, ...) {
  f <- tryCatch(stats::family(object),
                error = function(e) list(family = "?", link = "?"))
  cat("\n===== Hierarchical Bayesian Model Summary =====\n\n")
  cat(" Observations :", nrow(object$data), "\n")
  cat(" Family       :", f$family, "(link:", f$link, ")\n")
  cat(" Formula      :", deparse(stats::formula(object)), "\n")
  if (!is.null(object$missing_method))
    cat(" Missing data :", object$missing_method, "\n")
  cat("\n----- Parameter Estimates -----\n")
  print(summary(object$model, ...))
  invisible(object)
}


# =============================================================================
# S3 methods for result classes (hbcc_results, hbmc_results, hbpc_results,
# hbsae_results, hbm_table)
# =============================================================================


# -- hbcc_results (convergence diagnostics) -----------------------------------

#' @method plot hbcc_results
#' @export
plot.hbcc_results <- function(x, type = "all", ...) {
  if (type == "all") {
    valid <- x$plots[!vapply(x$plots, is.null, logical(1L))]
    lapply(valid, print)
    return(invisible(x))
  }
  if (type %in% names(x$plots)) {
    print(x$plots[[type]])
    return(invisible(x))
  }
  stop("Type \"", type, "\" not available.  Options: ",
       paste(names(x$plots), collapse = ", "), call. = FALSE)
}

#' @method print hbcc_results
#' @export
print.hbcc_results <- function(x, ...) {
  cat("\nConvergence Diagnostics  [hbcc_results]\n")
  cat("------------------------------------------\n")
  if (!is.null(x$rhat_ess)) {
    ok <- all(x$rhat_ess[, "Rhat"] < 1.1, na.rm = TRUE)
    cat(" R-hat < 1.1 :", if (ok) "PASS" else "FAIL", "\n")
    cat(" Parameters  :", nrow(x$rhat_ess), "\n")
  }
  cat("\nUse summary() for full statistics or plot() to visualise.\n\n")
  invisible(x)
}

#' @method summary hbcc_results
#' @export
summary.hbcc_results <- function(object, ...) {
  cat("\n===== Convergence Diagnostics Summary =====\n\n")
  if (!is.null(object$rhat_ess)) {
    cat("R-hat:\n");   print(summary(object$rhat_ess[, "Rhat"]))
    cat("\nBulk ESS:\n"); print(summary(object$rhat_ess[, "Bulk_ESS"]))
  }
  if (!is.null(object$geweke)) {
    cat("\nGeweke test (Z-scores):\n"); print(object$geweke)
  }
  invisible(object)
}


# -- hbmc_results (model comparison) ------------------------------------------

#' @method print hbmc_results
#' @export
print.hbmc_results <- function(x, ...) {
  cat("\nModel Comparison  [hbmc_results]\n")
  cat("-----------------------------------\n")
  if (!is.null(x$loo1))
    cat(sprintf(" ELPD-LOO  (m1): %.2f\n",
                x$loo1$estimates["elpd_loo", "Estimate"]))
  if (!is.null(x$loo2))
    cat(sprintf(" ELPD-LOO  (m2): %.2f\n",
                x$loo2$estimates["elpd_loo", "Estimate"]))
  if (!is.null(x$waic1))
    cat(sprintf(" ELPD-WAIC (m1): %.2f\n",
                x$waic1$estimates["elpd_waic", "Estimate"]))
  if (!is.null(x$waic2))
    cat(sprintf(" ELPD-WAIC (m2): %.2f\n",
                x$waic2$estimates["elpd_waic", "Estimate"]))
  cat("\n")
  invisible(x)
}

#' @method summary hbmc_results
#' @export
summary.hbmc_results <- function(object, ...) {
  cat("\n===== Model Comparison Summary =====\n\n")
  print(object)
  invisible(object)
}

#' @method plot hbmc_results
#' @export
plot.hbmc_results <- function(x, type = c("pp_check", "params"), ...) {
  type <- match.arg(type)
  switch(
    type,
    pp_check = if (!is.null(x$pp_check)) print(x$pp_check) else
      message("No pp_check plot available."),
    params   = if (!is.null(x$params))   print(x$params)   else
      message("No params plot available.")
  )
  invisible(x)
}


# -- hbpc_results (prior predictive check) ------------------------------------

#' @method plot hbpc_results
#' @export
plot.hbpc_results <- function(x, ...) {
  if (!is.null(x$prior_predictive_plot)) print(x$prior_predictive_plot)
  else message("No plot available.")
  invisible(x)
}

#' @method print hbpc_results
#' @export
print.hbpc_results <- function(x, ...) {
  cat("\nPrior Predictive Check  [hbpc_results]\n")
  cat("----------------------------------------\n")
  cat(" Prior draws  :", nrow(x$prior_draws), "x",
                          ncol(x$prior_draws), "\n")
  cat(" Observations :", length(x$observed), "\n\n")
  invisible(x)
}

#' @method summary hbpc_results
#' @export
summary.hbpc_results <- function(object, ...) {
  cat("\n===== Prior Predictive Check Summary =====\n\n")
  cat("Observed data:\n"); print(summary(object$observed))
  cat("\nPrior predictive (row means):\n")
  print(summary(rowMeans(object$prior_draws, na.rm = TRUE)))
  invisible(object)
}


# -- hbsae_results (small-area estimates) -------------------------------------

#' @method plot hbsae_results
#' @export
plot.hbsae_results <- function(x,
                               type = c("predictions", "uncertainty"),
                               ...) {
  type <- match.arg(type)
  df <- x$result_table
  df$.Area <- seq_len(nrow(df))

  switch(
    type,
    predictions = ggplot2::ggplot(
        df, ggplot2::aes(x = .data$.Area, y = .data$Prediction)
      ) +
      ggplot2::geom_point(colour = "#185FA5", size = 2) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Small Area Estimates",
                    x = "Area", y = "Estimate"),

    uncertainty = ggplot2::ggplot(
        df, ggplot2::aes(x = .data$.Area, y = .data$RSE_percent)
      ) +
      ggplot2::geom_point(colour = "#D85A30", size = 2) +
      ggplot2::geom_hline(yintercept = x$rse_model,
                          linetype = "dashed", colour = "grey60") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Relative Standard Error by Area",
                    x = "Area", y = "RSE (%)")
  )
}

#' @method print hbsae_results
#' @export
print.hbsae_results <- function(x, ...) {
  cat("\nSmall Area Estimates  [hbsae_results]\n")
  cat("--------------------------------------\n")
  cat(" Areas       :", nrow(x$result_table), "\n")
  cat(" Overall RSE :", round(x$rse_model, 2), "%\n")
  cat(" Pred. range :",
      paste(round(range(x$pred, na.rm = TRUE), 3), collapse = " to "), "\n\n")
  invisible(x)
}

#' @method summary hbsae_results
#' @export
summary.hbsae_results <- function(object, ...) {
  cat("\n===== Small Area Estimation Summary =====\n\n")
  cat("Areas       :", nrow(object$result_table), "\n")
  cat("Overall RSE :", round(object$rse_model, 2), "%\n\n")
  cat("Predictions:\n");  print(summary(object$pred))
  cat("\nRSE by area:\n"); print(summary(object$result_table$RSE_percent))
  invisible(object)
}

#' @method as.data.frame hbsae_results
#' @export
as.data.frame.hbsae_results <- function(x, ...) x$result_table


# -- hbm_table (model-comparison table) ---------------------------------------

#' @method print hbm_table
#' @export
print.hbm_table <- function(x, ...) {
  cat("Model Comparison Table  [hbm_table]\n")
  cat("=====================================\n\n")
  print.data.frame(x, row.names = FALSE)
  cat("\nBest model:", x$Model[1L], "\n")
  invisible(x)
}
