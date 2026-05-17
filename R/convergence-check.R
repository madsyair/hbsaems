# R/convergence-check.R
# =============================================================================
# Primary convergence diagnostics function for hbsaems 1.0.0+.
# This file replaces the body of the deprecated `hbcc()` function.
# =============================================================================

#' MCMC Convergence Diagnostics for Fitted HBMs
#'
#' Computes a battery of convergence tests and diagnostic plots for an
#' \code{hbmfit} object.  This is the primary convergence diagnostic function
#' in \pkg{hbsaems} (supersedes the deprecated \code{\link{hbcc}}).
#'
#' @param model       An \code{hbmfit} or \code{brmsfit} object.
#' @param diag_tests  Character vector of tests to run.  Any subset of
#'   \code{c("rhat", "geweke", "heidel", "raftery")}.
#' @param plot_types  Character vector of plot types to generate.  Any subset
#'   of \code{c("trace", "dens", "acf", "nuts_energy", "rhat", "neff")}.
#' @param ...         Additional arguments passed to the underlying
#'   \pkg{brms} or \pkg{coda} routines.
#'
#' @return An \code{hbcc_results} object containing:
#'   \describe{
#'     \item{\code{rhat_ess}}{Matrix with columns \code{Rhat},
#'       \code{Bulk_ESS}, \code{Tail_ESS}.}
#'     \item{\code{geweke,raftery,heidel}}{Outputs from the corresponding
#'       \pkg{coda} routines, or \code{NULL} when the test fails.}
#'     \item{\code{plots}}{Named list of \code{ggplot}/\code{bayesplot} objects.}
#'   }
#'
#' @details
#' For each parameter \eqn{\theta_j}, the Gelman-Rubin statistic is computed
#' as
#' \deqn{\widehat{R}_j = \sqrt{\frac{\widehat{\mathrm{var}}^+(\theta_j)}{W_j}}}
#' where \eqn{W_j} is the within-chain variance and
#' \eqn{\widehat{\mathrm{var}}^+} is the marginal posterior variance estimate.
#' Values close to 1 (typically below 1.1) indicate convergence.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1 + x2 + x3),
#'              data   = data_fhnorm,
#'              re     = ~ (1 | regency),    # area-level random effect
#'              chains = 2, iter = 2000, warmup = 1000,
#'              cores  = 1, seed = 123, refresh = 0)
#'
#' diag <- convergence_check(model)
#' summary(diag)
#' is_converged(model)
#' is_converged(model, threshold = 1.05)
#' }
#'
#' @seealso \code{\link{is_converged}}, \code{\link{diagnostic_summary}},
#'   \code{\link{hbm_warnings}}
#' @export
convergence_check <- function(model,
                              diag_tests = c("rhat", "geweke",
                                             "heidel", "raftery"),
                              plot_types = c("trace", "dens", "acf",
                                             "nuts_energy", "rhat", "neff"),
                              ...) {
  UseMethod("convergence_check")
}

#' @export
convergence_check.hbmfit <- function(model,
                                     diag_tests = c("rhat", "geweke",
                                                    "heidel", "raftery"),
                                     plot_types = c("trace", "dens", "acf",
                                                    "nuts_energy", "rhat",
                                                    "neff"),
                                     ...) {
  brms_model <- model$model
  results    <- list()
  plots      <- list()

  # ---- R-hat and ESS -------------------------------------------------------
  if ("rhat" %in% diag_tests) {
    rhat_vals <- brms::rhat(brms_model)
    # Use posterior::summarise_draws() for true tail ESS when posterior is
    # available (preferred); fall back to neff_ratio approximation otherwise.
    if (requireNamespace("posterior", quietly = TRUE)) {
      sd_tab    <- posterior::summarise_draws(
        brms::as_draws(brms_model),
        posterior::default_convergence_measures()
      )
      results$rhat_ess <- cbind(
        Rhat     = sd_tab$rhat,
        Bulk_ESS = sd_tab$ess_bulk,
        Tail_ESS = sd_tab$ess_tail
      )
      rownames(results$rhat_ess) <- sd_tab$variable
    } else {
      n_iter   <- brms_model$fit@sim$iter
      n_chain  <- brms_model$fit@sim$chains
      ess_bulk <- brms::neff_ratio(brms_model) * n_iter * n_chain / 2
      results$rhat_ess <- cbind(Rhat = rhat_vals,
                                Bulk_ESS = ess_bulk,
                                Tail_ESS = ess_bulk)
    }
  }

  # ---- coda-based tests (Geweke, Raftery, Heidelberger) -------------------
  needs_mcmc <- any(c("geweke", "heidel", "raftery") %in% diag_tests)
  if (needs_mcmc) {
    draws_list <- tryCatch(
      rstan::As.mcmc.list(brms_model$fit),
      error = function(e) NULL
    )
    if (is.null(draws_list)) {
      message("Cannot extract MCMC draws via rstan::As.mcmc.list(); ",
              "skipping coda-based tests.")
    } else {
      if ("geweke" %in% diag_tests)
        results$geweke <- tryCatch(coda::geweke.diag(draws_list),
                                   error = function(e) NULL)
      if ("raftery" %in% diag_tests)
        results$raftery <- tryCatch(coda::raftery.diag(draws_list[[1L]]),
                                    error = function(e) NULL)
      if ("heidel" %in% diag_tests)
        results$heidel <- tryCatch(coda::heidel.diag(draws_list[[1L]]),
                                   error = function(e) NULL)
    }
  }

  # ---- Plots ----------------------------------------------------------------
  for (pt in plot_types) {
    plots[[pt]] <- tryCatch(
      brms::mcmc_plot(brms_model, type = pt, ...),
      error = function(e) NULL
    )
  }

  structure(
    list(rhat_ess = results$rhat_ess,
         geweke   = results$geweke,
         raftery  = results$raftery,
         heidel   = results$heidel,
         plots    = plots),
    class = "hbcc_results"
  )
}

#' @export
convergence_check.brmsfit <- function(model,
                                      diag_tests = c("rhat", "geweke",
                                                     "heidel", "raftery"),
                                      plot_types = c("trace", "dens", "acf",
                                                     "nuts_energy", "rhat",
                                                     "neff"),
                                      ...) {
  # Promote a bare brmsfit to hbmfit so the same method applies.
  tmp <- new_hbmfit(model, missing_method = NULL, data = model$data)
  convergence_check(tmp,
                    diag_tests = diag_tests,
                    plot_types = plot_types,
                    ...)
}


# =============================================================================
# is_converged()  --  binary convergence pass/fail
# =============================================================================

#' Test Whether a Fitted HBM Has Converged
#'
#' Returns a single \code{TRUE} / \code{FALSE} based on the Gelman-Rubin
#' statistic.
#'
#' @param model     An \code{hbmfit} or \code{hbcc_results} object.
#' @param threshold \eqn{\widehat{R}} threshold (default \code{1.1};
#'   use \code{1.05} for a stricter check, as recommended by Vehtari et al.
#'   2021).
#' @param ...       Currently unused.
#' @return A single logical.
#' @export
is_converged <- function(model, threshold = 1.1, ...) {
  UseMethod("is_converged")
}

#' @export
is_converged.hbmfit <- function(model, threshold = 1.1, ...) {
  all(brms::rhat(model$model) < threshold, na.rm = TRUE)
}

#' @export
is_converged.hbcc_results <- function(model, threshold = 1.1, ...) {
  if (is.null(model$rhat_ess)) {
    warning("No R-hat values stored in the hbcc_results object.",
            call. = FALSE)
    return(NA)
  }
  all(model$rhat_ess[, "Rhat"] < threshold, na.rm = TRUE)
}


# =============================================================================
# diagnostic_summary()  --  full numeric summary
# =============================================================================

#' Extract a Diagnostic Summary
#'
#' @param x An \code{hbmfit} or \code{hbcc_results} object.
#' @return A named list of diagnostic statistics.
#' @export
diagnostic_summary <- function(x) UseMethod("diagnostic_summary")

#' @export
diagnostic_summary.hbmfit <- function(x) {
  list(
    rhat        = brms::rhat(x$model),
    neff_ratio  = brms::neff_ratio(x$model),
    nuts_params = tryCatch(brms::nuts_params(x$model),
                           error = function(e) NULL),
    converged   = is_converged(x),
    warnings    = hbm_warnings(x)
  )
}

#' @export
diagnostic_summary.hbcc_results <- function(x) {
  list(
    rhat_ess  = x$rhat_ess,
    geweke    = x$geweke,
    raftery   = x$raftery,
    heidel    = x$heidel,
    converged = is_converged(x)
  )
}
