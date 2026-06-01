# R/model-compare.R
# =============================================================================
# Primary model-comparison function for hbsaems 1.0.0+.
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

# Internal (v1.1.0): count high Pareto-k and, if requested, run brms::reloo().
# Returns the (possibly refit) loo object unchanged when not applicable.
.high_pareto_k <- function(loo_obj, threshold = 0.7) {
  if (is.null(loo_obj)) return(integer(0))
  k <- tryCatch(loo_obj$diagnostics$pareto_k, error = function(e) NULL)
  if (is.null(k)) return(integer(0))
  which(k > threshold)
}

.maybe_reloo <- function(loo_obj, brms_model, moment_match, reloo_args) {
  if (is.null(loo_obj) || is.null(brms_model)) return(loo_obj)
  bad <- .high_pareto_k(loo_obj)
  if (length(bad) == 0L) return(loo_obj)

  want_reloo <- is.list(reloo_args) && length(reloo_args) > 0L
  if (want_reloo) {
    # Drop a possible reloo=TRUE flag; brms::reloo() refits unconditionally.
    ra <- reloo_args[setdiff(names(reloo_args), "reloo")]
    refit <- tryCatch(
      do.call(brms::reloo, c(list(x = loo_obj, fit = brms_model), ra)),
      error = function(e) {
        warning("reloo() failed (", conditionMessage(e),
                "); returning the PSIS-LOO with ", length(bad),
                " high Pareto-k point(s) unrefit.", call. = FALSE)
        NULL
      }
    )
    if (!is.null(refit)) return(refit)
    return(loo_obj)
  }

  if (!isTRUE(moment_match))
    warning(length(bad), " observation(s) have Pareto k > 0.7; PSIS-LOO may ",
            "be unreliable. Consider moment_match = TRUE or ",
            "reloo_args = list() to refit those folds.", call. = FALSE)
  loo_obj
}



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
#' FAST <- list(chains = 4, iter = 2000, warmup = 1000, cores = 1,
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

  # PSIS-LOO reliability (v1.1.0).  PSIS-LOO is only trustworthy when all
  # Pareto k < 0.7 (Vehtari, Gelman & Gabry 2017).  If high-k points exist:
  #   * when the user asked for reloo (reloo_args non-empty), actually refit
  #     the problematic folds via brms::reloo() -- previously reloo_args was
  #     accepted but never used;
  #   * otherwise emit a single informative warning recommending a fix.
  loo1 <- .maybe_reloo(loo1, brms1, moment_match, reloo_args)

  loo2 <- if (!is.null(brms2) && "loo" %in% comparison_metrics)
    brms::loo(brms2) else NULL
  loo2 <- .maybe_reloo(loo2, brms2, moment_match, reloo_args)

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
  # For multivariate brms models (joint mi() imputation), pp_check() needs
  # a `resp` argument to disambiguate; we default to the first response
  # if the user did not pass one via `...`.
  pp_chk <- if ("pp_check" %in% plot_types) {
    dot_args <- list(...)
    pp_args  <- list(object = brms1, ndraws = ndraws_ppc)
    is_mv <- !is.null(brms1$formula$forms)
    if (is_mv && is.null(dot_args$resp)) {
      # Pick the first response from the multivariate formula.
      first_form <- brms1$formula$forms[[1L]]$formula
      first_resp <- as.character(first_form[[2L]])[1L]
      pp_args$resp <- first_resp
    }
    tryCatch(do.call(brms::pp_check, pp_args),
             error = function(e) NULL)
  } else NULL

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


# Internal: coerce a single fit object to hbmfit (model_compare_all() only
# accepts hbmfit).  Mirrors model_compare.brmsfit's coercion.
.coerce_fit_to_hbmfit <- function(x) {
  if (is.hbmfit(x)) return(x)
  if (inherits(x, "brmsfit"))
    return(new_hbmfit(x, missing_method = NULL, data = x$data))
  stop("If 'model' is a list, every element must be a brmsfit or ",
       "hbmfit object.", call. = FALSE)
}

#' @export
model_compare.list <- function(model, model2 = NULL,
                               ndraws_ppc            = 100,
                               moment_match          = FALSE,
                               moment_match_args     = list(),
                               reloo_args            = list(),
                               plot_types            = c("pp_check", "params"),
                               comparison_metrics    = c("loo", "waic", "bf"),
                               run_prior_sensitivity = FALSE,
                               sensitivity_vars      = NULL,
                               ...) {

  # -- 1. Validate the list ----------------------------------------------------
  if (length(model) == 0L)
    stop("If 'model' is a list, it must contain at least one ",
         "brmsfit/hbmfit object.", call. = FALSE)

  is_fit <- vapply(
    model,
    function(x) is.hbmfit(x) || inherits(x, "brmsfit"),
    logical(1L)
  )
  if (!all(is_fit))
    stop("If 'model' is a list, every element must be a brmsfit or ",
         "hbmfit object; element(s) ",
         paste(which(!is_fit), collapse = ", "), " are not.",
         call. = FALSE)

  # -- 2. One or two models: keep the rich hbmc_results diagnostics ------------
  #    (preserves the documented model_compare()/legacy hbmc() contract).
  if (length(model) <= 2L) {
    m1 <- model[[1L]]
    m2 <- if (length(model) == 2L) model[[2L]] else model2
    return(model_compare(
      m1, model2            = m2,
      ndraws_ppc            = ndraws_ppc,
      moment_match          = moment_match,
      moment_match_args     = moment_match_args,
      reloo_args            = reloo_args,
      plot_types            = plot_types,
      comparison_metrics    = comparison_metrics,
      run_prior_sensitivity = run_prior_sensitivity,
      sensitivity_vars      = sensitivity_vars,
      ...
    ))
  }

  # -- 3. Three or more models: rank via model_compare_all() -------------------
  #    The pairwise-only arguments (moment_match, reloo_args, plot_types, bf,
  #    prior sensitivity) do not apply to an N-model ranking.
  criterion <- if (all(c("loo", "waic") %in% comparison_metrics)) "both"
               else if ("waic" %in% comparison_metrics)           "waic"
               else                                                "loo"

  models <- lapply(model, .coerce_fit_to_hbmfit)
  if (is.null(names(models)) || any(!nzchar(names(models))))
    names(models) <- paste0("model", seq_along(models))

  do.call(model_compare_all, c(models, list(criterion = criterion)))
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
#' FAST <- list(chains = 4, iter = 2000, warmup = 1000, cores = 1,
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
    # PSIS-LOO reliability per model (v1.1.0): number of Pareto k > 0.7.
    res$n_high_k  <- vapply(loo_list, function(L)
      length(.high_pareto_k(L)), integer(1L))
    res$LOO_rank  <- rank(-res$ELPD_LOO)
    if (any(res$n_high_k > 0L))
      warning(sum(res$n_high_k > 0L), " model(s) have observations with ",
              "Pareto k > 0.7; their LOO comparison may be unreliable. ",
              "Refit with model_compare(..., reloo_args = list()) or ",
              "moment_match = TRUE for the affected models.", call. = FALSE)
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
#' Averages the area-level predictions across multiple fitted HBMs.
#' Weights may be supplied manually \emph{or} computed automatically
#' from leave-one-out cross-validation via
#' \code{\link[loo]{loo_model_weights}} -- the canonical Bayesian
#' stacking / pseudo-BMA approach of Yao et al.\ (2018).
#'
#' @details
#' Three weighting modes are supported:
#' \itemize{
#'   \item \code{method = "manual"} (default when \code{weights} is supplied):
#'     use the user-supplied \code{weights} vector directly.  Internally
#'     normalised to sum to 1.
#'   \item \code{method = "stacking"}:  weights are obtained from
#'     \code{loo::loo_model_weights(loo_list, method = "stacking")},
#'     which optimises a log-score over a simplex.  Recommended when
#'     models are well-specified but capture different features of
#'     the data (Yao et al.\ 2018).
#'   \item \code{method = "pseudobma"}: weights are obtained from
#'     \code{loo::loo_model_weights(loo_list, method = "pseudobma")},
#'     a smoothed pseudo-BMA+ that resembles classical BMA but uses
#'     PSIS-LOO log scores.  Use as a robust default when one model
#'     is much better than the others.
#' }
#' Internally calls \code{\link{sae_predict}} on each model and then
#' \code{\link{sae_aggregate}} with \code{method = "weighted"}.
#'
#' @param ...     Two or more \code{hbmfit} objects.
#' @param weights Numeric weights of the same length as the number of
#'   models, or \code{NULL}.  When \code{NULL} and \code{method = "manual"},
#'   equal weights are used.
#' @param method  Character.  Weighting method: \code{"manual"} (default),
#'   \code{"stacking"} (Yao et al. 2018), or \code{"pseudobma"}.  When
#'   \code{"stacking"} or \code{"pseudobma"}, \code{weights} must be
#'   \code{NULL}; an error is raised otherwise.
#' @param predict_type Passed to \code{\link{sae_predict}} for each model:
#'   one of \code{"epred"} (default), \code{"response"}, \code{"linpred"},
#'   or \code{"proportion"}.
#' @param newdata Optional new \code{data.frame} forwarded to
#'   \code{\link{sae_predict}}.
#'
#' @return An \code{hbsae_results} object of averaged predictions.  The
#'   computed weights are attached as an attribute \code{"weights"}.
#'
#' @references
#' Yao, Y., Vehtari, A., Simpson, D., & Gelman, A. (2018).  Using
#' stacking to average Bayesian predictive distributions (with
#' discussion).  \emph{Bayesian Analysis}, 13(3), 917--1007.
#' \doi{10.1214/17-BA1091}
#'
#' Vehtari, A., Gelman, A., & Gabry, J. (2017).  Practical Bayesian
#' model evaluation using leave-one-out cross-validation and WAIC.
#' \emph{Statistics and Computing}, 27(5), 1413--1432.
#'
#' @examples
#' \donttest{
#' # See ?model_compare_all for the full example fitting m1 / m2.
#' # Manual:    avg <- model_average(m1, m2, weights = c(0.6, 0.4))
#' # Stacking:  avg <- model_average(m1, m2, method = "stacking")
#' # Pseudo-BMA: avg <- model_average(m1, m2, method = "pseudobma")
#' }
#' @export
model_average <- function(..., weights = NULL,
                            method = c("manual", "stacking", "pseudobma"),
                            predict_type = c("epred", "response", "linpred",
                                             "proportion"),
                            newdata = NULL) {
  models <- list(...)
  method <- match.arg(method)
  predict_type <- match.arg(predict_type)

  if (length(models) < 2L)
    stop("model_average() requires at least two hbmfit objects.",
         call. = FALSE)
  if (!all(vapply(models, is.hbmfit, logical(1L))))
    stop("All arguments must be hbmfit objects.", call. = FALSE)

  n <- length(models)

  # ---- Compute or validate weights --------------------------------------
  if (method == "manual") {
    if (is.null(weights)) weights <- rep(1 / n, n)
    if (length(weights) != n)
      stop("length(weights) must equal the number of models.", call. = FALSE)
    if (any(weights < 0))
      stop("All weights must be non-negative.", call. = FALSE)
    weights <- weights / sum(weights)
  } else {
    # Automatic weighting via loo
    if (!is.null(weights))
      stop("`weights` must be NULL when method = \"", method, "\".",
           call. = FALSE)
    if (!requireNamespace("loo", quietly = TRUE))
      stop("Package 'loo' is required for method = '", method,
           "'. Install with: install.packages('loo')", call. = FALSE)
    loo_list <- lapply(models, function(m) brms::loo(m$model))
    weights  <- as.numeric(
      loo::loo_model_weights(loo_list, method = method)
    )
    if (length(weights) != n)
      stop("loo_model_weights() returned ", length(weights),
           " weights for ", n, " models.", call. = FALSE)
  }

  # ---- Average ---------------------------------------------------------
  ests    <- lapply(models, function(m)
                    sae_predict(m, newdata = newdata,
                                predict_type = predict_type))
  out <- do.call(sae_aggregate, c(ests, list(method = "weighted",
                                              weights = weights)))
  attr(out, "weights") <- weights
  attr(out, "weight_method") <- method
  out
}
