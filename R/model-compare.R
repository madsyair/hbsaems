# R/model-compare.R
# =============================================================================
# Primary model-comparison API for hbsaems v0.3.0+.
#
#   model_compare()      -- 1 or 2 models  (replaces deprecated hbmc())
#   model_compare_all()  -- N models (analogous to brms::loo_compare)
#   model_average()      -- Bayesian model averaging on hbsae_results
# =============================================================================

#' Compare Fitted HBMs
#'
#' Primary model-comparison functions in \pkg{hbsaems}.
#' \code{model_compare} handles one or two models (supersedes deprecated
#' \code{\link{hbmc}}); \code{model_compare_all} handles N models (analogous
#' to \code{\link[brms]{loo_compare}}).
#'
#' @name model-compare
NULL


#' Compare One or Two Fitted HBMs
#'
#' Computes LOO, WAIC, and posterior predictive check diagnostics.  With a
#' single model this gives goodness-of-fit metrics; with two models it adds
#' a pairwise comparison.
#'
#' @param model  An \code{hbmfit} or \code{brmsfit} object.
#' @param model2 Optional second \code{hbmfit} for pairwise comparison.
#' @param ndraws_ppc Number of draws for the posterior-predictive plot
#'   (default \code{100}).
#' @param moment_match Logical; use moment matching for LOO
#'   (default \code{FALSE}).
#' @param moment_match_args Named list of arguments for moment matching.
#' @param reloo_args Named list of arguments for \code{\link[brms]{reloo}}.
#' @param plot_types Character vector.  Any subset of
#'   \code{c("pp_check", "params")}.
#' @param comparison_metrics Character vector.  Any subset of
#'   \code{c("loo", "waic", "bf")}.
#' @param run_prior_sensitivity Logical; run prior sensitivity analysis using
#'   \pkg{priorsense} (default \code{FALSE}).
#' @param sensitivity_vars Variables for the sensitivity analysis.
#' @param ... Additional arguments.
#'
#' @return An \code{hbmc_results} object with components \code{loo1},
#'   \code{waic1}, \code{pp_check}, \code{params}, and -- when \code{model2}
#'   is given -- also \code{loo2}, \code{waic2}, \code{bf}, and
#'   \code{model2}.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' FAST <- list(chains = 2, iter = 2000, warmup = 1000, cores = 1,
#'              seed = 123, refresh = 0)
#'
#' m1 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1 + x2 + x3),
#'                           data = data_fhnorm), FAST))
#' m2 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1 + x2),
#'                           data = data_fhnorm), FAST))
#'
#' model_compare(m1)            # single-model goodness-of-fit
#' model_compare(m1, m2)        # pairwise comparison
#' }
#'
#' @seealso \code{\link{model_compare_all}}, \code{\link{model_average}}
#' @export
model_compare <- function(model, model2 = NULL,
                          ndraws_ppc            = 100,
                          moment_match          = FALSE,
                          moment_match_args     = list(),
                          reloo_args            = list(),
                          plot_types            = c("pp_check", "params"),
                          comparison_metrics    = c("loo", "waic", "bf"),
                          run_prior_sensitivity = FALSE,
                          sensitivity_vars      = NULL,
                          ...) {
  UseMethod("model_compare")
}

#' @export
model_compare.hbmfit <- function(model, model2 = NULL,
                                 ndraws_ppc            = 100,
                                 moment_match          = FALSE,
                                 moment_match_args     = list(),
                                 reloo_args            = list(),
                                 plot_types            = c("pp_check",
                                                           "params"),
                                 comparison_metrics    = c("loo", "waic",
                                                           "bf"),
                                 run_prior_sensitivity = FALSE,
                                 sensitivity_vars      = NULL,
                                 ...) {
  brms1 <- model$model
  brms2 <- if (!is.null(model2)) model2$model else NULL

  # ---- LOO ------------------------------------------------------------------
  loo1 <- if ("loo" %in% comparison_metrics) {
    if (moment_match) {
      do.call(brms::loo,
              c(list(brms1, moment_match = TRUE), moment_match_args))
    } else {
      brms::loo(brms1)
    }
  } else NULL

  loo2 <- if (!is.null(brms2) && "loo" %in% comparison_metrics)
    brms::loo(brms2) else NULL

  # ---- WAIC -----------------------------------------------------------------
  waic1 <- if ("waic" %in% comparison_metrics) brms::waic(brms1) else NULL
  waic2 <- if (!is.null(brms2) && "waic" %in% comparison_metrics)
    brms::waic(brms2) else NULL

  # ---- Bayes Factor (requires bridgesampling) -------------------------------
  bf_result <- if (!is.null(brms2) && "bf" %in% comparison_metrics) {
    if (!requireNamespace("bridgesampling", quietly = TRUE)) {
      message("Package 'bridgesampling' is required to compute Bayes ",
              "factors.  Skipping.")
      NULL
    } else {
      tryCatch(
        bridgesampling::bayes_factor(brms1, brms2),
        error = function(e) {
          message("Bayes factor computation failed: ", e$message)
          NULL
        }
      )
    }
  } else NULL

  # ---- Posterior predictive check -------------------------------------------
  pp_chk <- if ("pp_check" %in% plot_types)
    tryCatch(brms::pp_check(brms1, ndraws = ndraws_ppc),
             error = function(e) NULL)
  else NULL

  # ---- Parameter plot -------------------------------------------------------
  params_plot <- if ("params" %in% plot_types)
    tryCatch(brms::mcmc_plot(brms1, type = "areas"),
             error = function(e) NULL)
  else NULL

  structure(
    list(loo1     = loo1,    waic1     = waic1,
         loo2     = loo2,    waic2     = waic2,
         bf       = bf_result,
         pp_check = pp_chk,  params    = params_plot,
         model2   = model2),
    class = "hbmc_results"
  )
}

#' @export
model_compare.brmsfit <- function(model, model2 = NULL, ...) {
  tmp <- new_hbmfit(model, missing_method = NULL, data = model$data)
  model_compare(tmp, model2 = model2, ...)
}


# =============================================================================
# model_compare_all()  --  rank N models
# =============================================================================

#' Compare Multiple Fitted HBMs
#'
#' Ranks N models by LOO and/or WAIC, returning a sorted \code{hbm_table}.
#' Analogous to \code{\link[brms]{loo_compare}}.
#'
#' @param ... Named \code{hbmfit} objects.
#' @param criterion One of \code{"loo"} (default), \code{"waic"}, or
#'   \code{"both"}.
#'
#' @return A \code{hbm_table} (a sorted \code{data.frame}) with columns
#'   \code{Model}, \code{ELPD_LOO}, \code{LOO_SE}, \code{LOO_rank} (and
#'   analogous \code{*_WAIC*} columns when requested).
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' FAST <- list(chains = 2, iter = 1000, warmup = 500, cores = 1,
#'              seed = 1, refresh = 0)
#' m1 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1),
#'                           data = data_fhnorm), FAST))
#' m2 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1 + x2),
#'                           data = data_fhnorm), FAST))
#' model_compare_all(simple = m1, medium = m2)
#' }
#' @export
model_compare_all <- function(..., criterion = c("loo", "waic", "both")) {
  criterion <- match.arg(criterion)
  models <- list(...)

  if (!length(models))
    stop("At least one model must be supplied.", call. = FALSE)
  if (!all(vapply(models, is.hbmfit, logical(1L))))
    stop("All arguments must be hbmfit objects.", call. = FALSE)

  nms <- names(models)
  if (is.null(nms) || any(!nzchar(nms)))
    nms <- paste0("Model_", seq_along(models))

  res <- data.frame(Model = nms, stringsAsFactors = FALSE)

  if (criterion %in% c("loo", "both")) {
    loo_list <- lapply(models, function(m) brms::loo(m$model))
    res$ELPD_LOO  <- vapply(loo_list, function(L)
      L$estimates["elpd_loo", "Estimate"], numeric(1L))
    res$LOO_SE    <- vapply(loo_list, function(L)
      L$estimates["elpd_loo", "SE"],       numeric(1L))
    res$LOO_rank  <- rank(-res$ELPD_LOO)
  }

  if (criterion %in% c("waic", "both")) {
    waic_list <- lapply(models, function(m) brms::waic(m$model))
    res$ELPD_WAIC <- vapply(waic_list, function(W)
      W$estimates["elpd_waic", "Estimate"], numeric(1L))
    res$WAIC_SE   <- vapply(waic_list, function(W)
      W$estimates["elpd_waic", "SE"],       numeric(1L))
    res$WAIC_rank <- rank(-res$ELPD_WAIC)
  }

  key <- if ("LOO_rank" %in% names(res)) "LOO_rank" else "WAIC_rank"
  res <- res[order(res[[key]]), , drop = FALSE]
  rownames(res) <- NULL
  class(res) <- c("hbm_table", "data.frame")
  res
}


# =============================================================================
# model_average()  --  Bayesian model averaging on predictions
# =============================================================================

#' Bayesian Model Averaging on Small-Area Estimates
#'
#' Averages the area-level predictions across multiple fitted HBMs using a
#' user-supplied weight vector.  Internally calls \code{\link{sae_predict}}
#' on each model and then \code{\link{sae_aggregate}} with
#' \code{method = "weighted"}.
#'
#' @param ...     Two or more \code{hbmfit} objects.
#' @param weights Numeric weights of the same length as the number of models.
#'   \code{NULL} (default) gives equal weights.  Internally normalised to
#'   sum to 1.
#' @param newdata Optional new \code{data.frame} forwarded to
#'   \code{\link{sae_predict}}.
#'
#' @return An \code{hbsae_results} object of averaged predictions.
#'
#' @examples
#' \donttest{
#' # See ?model_compare_all for the full example fitting m1 / m2.
#' # avg <- model_average(m1, m2, weights = c(0.6, 0.4))
#' }
#' @export
model_average <- function(..., weights = NULL, newdata = NULL) {
  models <- list(...)

  if (length(models) < 2L)
    stop("model_average() requires at least two hbmfit objects.",
         call. = FALSE)
  if (!all(vapply(models, is.hbmfit, logical(1L))))
    stop("All arguments must be hbmfit objects.", call. = FALSE)

  n <- length(models)
  if (is.null(weights)) weights <- rep(1 / n, n)
  if (length(weights) != n)
    stop("length(weights) must equal the number of models.", call. = FALSE)
  if (any(weights < 0))
    stop("All weights must be non-negative.", call. = FALSE)

  weights <- weights / sum(weights)
  ests    <- lapply(models, function(m) sae_predict(m, newdata = newdata))
  do.call(sae_aggregate, c(ests, list(method = "weighted",
                                      weights = weights)))
}
