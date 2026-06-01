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
#' # Uses brms's default MCMC settings (chains = 4, iter = 2000,
#' # warmup = 1000).  For challenging posteriors (e.g. funnel
#' # geometries in Fay-Herriot with small D_i), consider
#' # chains = 4, iter = 4000, warmup = 2000 and
#' # control = list(adapt_delta = 0.99).
#' model <- hbm(brms::bf(y ~ x1 + x2 + x3),
#'              data   = data_fhnorm,
#'              re     = ~ (1 | regency),    # area-level random effect
#'              chains = 4, iter = 2000, warmup = 1000,
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

  # ---- Validate arguments (v1.1.0) -----------------------------------------
  # Previously unknown plot/test names were silently swallowed (the plot loop
  # wrapped each call in tryCatch(..., NULL)); only an incidental brms error
  # made bad input fail.  Validate explicitly so mistakes are caught up front.
  valid_tests <- c("rhat", "geweke", "heidel", "raftery")
  valid_plots <- c("trace", "dens", "acf", "nuts_energy", "rhat", "neff")
  bad_tests <- setdiff(diag_tests, valid_tests)
  if (length(bad_tests))
    stop("Unknown diag_tests: ", paste(shQuote(bad_tests), collapse = ", "),
         ". Valid options: ", paste(valid_tests, collapse = ", "), ".",
         call. = FALSE)
  bad_plots <- setdiff(plot_types, valid_plots)
  if (length(bad_plots))
    stop("Unknown plot_types: ", paste(shQuote(bad_plots), collapse = ", "),
         ". Valid options: ", paste(valid_plots, collapse = ", "), ".",
         call. = FALSE)

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
      # 'posterior' unavailable: we can approximate Bulk ESS via neff_ratio
      # but NOT Tail ESS.  Report Tail_ESS = NA rather than duplicating
      # Bulk_ESS, which would falsely imply the tails were checked (v1.1.0).
      results$rhat_ess <- cbind(Rhat = rhat_vals,
                                Bulk_ESS = ess_bulk,
                                Tail_ESS = NA_real_)
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

  # ---- NUTS sampler diagnostics (v1.1.0) -----------------------------------
  # Divergent transitions are the single most important diagnostic for
  # hierarchical SAE models (they signal biased exploration of the funnel,
  # not mere inefficiency).  Surface them as NUMBERS, not just the energy
  # plot.  E-BFMI < 0.3 (Betancourt 2016) flags poor momentum resampling.
  nuts <- tryCatch(brms::nuts_params(brms_model), error = function(e) NULL)
  n_divergent <- NA_integer_; prop_divergent <- NA_real_
  ebfmi <- NULL; max_td_hit <- NA_real_
  if (!is.null(nuts)) {
    dv <- nuts[nuts$Parameter == "divergent__", "Value"]
    if (length(dv)) {
      n_divergent    <- sum(dv)
      prop_divergent <- mean(dv)
    }
    td <- nuts[nuts$Parameter == "treedepth__", "Value"]
    mx <- tryCatch(brms_model$fit@stan_args[[1]]$control$max_treedepth,
                   error = function(e) NULL)
    if (length(td) && !is.null(mx)) max_td_hit <- mean(td >= mx)
    en <- nuts[nuts$Parameter == "energy__", , drop = FALSE]
    if (nrow(en)) {
      # E-BFMI per chain: sum((E_t - E_{t-1})^2) / sum((E_t - mean E)^2)
      ebfmi <- tapply(en$Value, en$Chain, function(e) {
        sum(diff(e)^2) / sum((e - mean(e))^2)
      })
    }
  }
  if (!is.null(ebfmi) && any(ebfmi < 0.3, na.rm = TRUE))
    warning("Low E-BFMI (< 0.3) in ", sum(ebfmi < 0.3, na.rm = TRUE),
            " chain(s); the posterior may have heavy tails the sampler ",
            "explores poorly. Consider reparameterisation or longer warmup.",
            call. = FALSE)
  if (!is.na(n_divergent) && n_divergent > 0L)
    warning(n_divergent, " divergent transition(s) detected; estimates may ",
            "be biased. Increase adapt_delta (e.g. 0.99) or reparameterise.",
            call. = FALSE)

  structure(
    list(rhat_ess = results$rhat_ess,
         n_divergent    = n_divergent,
         prop_divergent = prop_divergent,
         ebfmi          = ebfmi,
         max_treedepth_hit = max_td_hit,
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
  .check_rhat_threshold(threshold)
  rhats <- brms::rhat(model$model)
  # all(numeric(0)) and all(c(NA, NA), na.rm = TRUE) both return TRUE,
  # which is a degenerate "convergence" signal -- it means we have no
  # Rhat values to evaluate against, not that everything is converged.
  # Treat this as NA with an informative warning.
  finite_rhats <- rhats[is.finite(rhats)]
  if (length(finite_rhats) == 0L) {
    warning("No finite R-hat values available for this model -- ",
            "cannot determine convergence.  Did the chains run at all?",
            call. = FALSE)
    return(NA)
  }
  all(finite_rhats < threshold)
}

#' @export
is_converged.hbcc_results <- function(model, threshold = 1.1, ...) {
  .check_rhat_threshold(threshold)
  if (is.null(model$rhat_ess)) {
    warning("No R-hat values stored in the hbcc_results object.",
            call. = FALSE)
    return(NA)
  }
  rhats <- model$rhat_ess[, "Rhat"]
  finite_rhats <- rhats[is.finite(rhats)]
  if (length(finite_rhats) == 0L) {
    warning("All stored R-hat values are NA/Inf -- ",
            "cannot determine convergence.",
            call. = FALSE)
    return(NA)
  }
  all(finite_rhats < threshold)
}

# Internal helper -- ensures the R-hat threshold is a single finite
# positive numeric.  `threshold = "1.1"` (a string), `NULL`, negative
# values, and length>1 vectors are all rejected explicitly because
# they previously fell through `<` coercion silently (e.g.\ a string
# threshold compared with `<` against a numeric vector coerces and
# produces NA which `all()` treats permissively).
.check_rhat_threshold <- function(threshold) {
  if (is.null(threshold) || length(threshold) != 1L ||
      !is.numeric(threshold) || !is.finite(threshold) ||
      threshold <= 0)
    stop("`threshold` must be a single finite positive number ",
         "(typically 1.05 or 1.1).  Got: ",
         deparse(threshold),
         call. = FALSE)
  invisible(TRUE)
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
