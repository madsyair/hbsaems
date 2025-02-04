#' Hierarchical Bayesian Convergence Checks
#' @description Check convergence diagnostics for a brmsfit object
#' @name hbcc
#' @param model A `brmsfit` object
#' @param params Character vector of parameters to check (default: all)
#' @param type Type of diagnostic plot to display (default: "trace")
#' @return Convergence diagnostics including Rhat, effective sample sizes, and diagnostic plots
#' @import brms
#' @importFrom coda as.mcmc gelman.diag geweke.diag raftery.diag heidel.diag
#' @param ... Additional arguments passed to `mcmc_plot`
#'
#' @export
hbcc <- function(model,
                 params = NULL,
                 type = "trace",
                 ...) {
  if (!inherits(model, "brmsfit")) stop("Input model must be a brmsfit object.")

  # Summary diagnostics
  cat("Rhat diagnostics:\n")
  summary_data <- summary(model)$fixed
  if (!is.null(params)) {
    summary_data <- summary_data[rownames(summary_data) %in% params, , drop = FALSE]
  }
  print(summary_data[, c("Rhat", "n_eff")])

  # Plot MCMC diagnostics
  mcmc_plot(model, variable = params, type = type, ...)
  # Extract MCMC samples
  mcmc_samples <- as.mcmc(model)
  # Convergence Tests
  cat("\nRunning convergence tests:\n")
  cat("1. Gelman-Rubin Diagnostic:\n")
  print(gelman.diag(mcmc_samples))

  cat("\n2. Geweke Diagnostic:\n")
  print(geweke.diag(mcmc_samples))

  cat("\n3. Raftery-Lewis Diagnostic:\n")
  print(raftery.diag(mcmc_samples))

  cat("\n4. Heidelberger-Welch Diagnostic:\n")
  print(heidel.diag(mcmc_samples))
}
