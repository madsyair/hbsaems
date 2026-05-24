# R/sae-benchmark.R
# =============================================================================
# Benchmarking small-area estimates to known aggregate (national or
# higher-level) totals.
#
# Three classical methods are implemented:
#   * "ratio"       --  multiplicative adjustment
#   * "difference"  --  additive adjustment (Pfeffermann-Tiller)
#   * "raking"      --  iterative proportional fitting (multi-target)
#
# Now supports FULLY BAYESIAN benchmarking via the `posterior`
# argument.  Each draw is benchmarked independently; SD, quantiles, and
# RSE are recomputed from the adjusted draws -- giving statistically
# correct uncertainty after benchmarking.
# =============================================================================


# ---- Internal: apply a single benchmark to one vector of predictions ------
#
# Returns a list with:
#   $pred       -- adjusted point vector
#   $info       -- list(method, adjustment OR converged+iterations)
.apply_benchmark <- function(pred, target, weights, method,
                              groups = NULL, max_iter = 100L, tol = 1e-8) {

  bm_info <- list(method = method, converged = NA)

  new_pred <- switch(
    method,

    ratio = {
      sw <- sum(weights * pred, na.rm = TRUE)
      if (abs(sw) < .Machine$double.eps)
        stop("Weighted sum of predictions is zero; ratio benchmark ",
             "is undefined.  Use method = 'difference' instead.",
             call. = FALSE)
      r <- target / sw
      bm_info$adjustment <- r
      pred * r
    },

    difference = {
      c_adj <- (target - sum(weights * pred, na.rm = TRUE)) / sum(weights)
      bm_info$adjustment <- c_adj
      pred + c_adj
    },

    raking = {
      grp <- as.factor(groups)
      grp_levels <- levels(grp)
      adj <- pred
      converged <- FALSE
      for (iter in seq_len(max_iter)) {
        new_adj <- adj
        max_chg <- 0
        for (g in seq_along(grp_levels)) {
          idx <- which(grp == grp_levels[g])
          sw  <- sum(weights[idx] * adj[idx], na.rm = TRUE)
          if (abs(sw) < .Machine$double.eps) next
          r   <- target[g] / sw
          new_adj[idx] <- adj[idx] * r
          max_chg <- max(max_chg, abs(r - 1))
        }
        adj <- new_adj
        if (max_chg < tol) { converged <- TRUE; break }
      }
      bm_info$converged  <- converged
      bm_info$iterations <- iter
      adj
    }
  )

  list(pred = new_pred, info = bm_info)
}


#' Benchmark Small-Area Estimates to Known Totals
#'
#' Adjusts area-level predictions so that their weighted sum matches one
#' or more known aggregate \dQuote{benchmark} totals (e.g.\ official
#' provincial or national figures).  Two modes are supported:
#'
#' \describe{
#'   \item{\strong{Point-estimate mode (default)}}{Applies the adjustment
#'     only to the posterior mean.  The RSE column is left unchanged as
#'     a working approximation.}
#'   \item{\strong{Fully Bayesian mode} (\code{posterior = TRUE} or supply
#'     a \code{posterior} matrix)}{Applies the adjustment to every
#'     posterior draw and recomputes SD, quantiles, and RSE from the
#'     adjusted draws.  This is the statistically correct procedure and
#'     produces proper uncertainty intervals after benchmarking.}
#' }
#'
#' @param predictions An \code{hbsae_results} object produced by
#'   \code{\link{sae_predict}}.
#' @param target Numeric.  For \code{method = "ratio"} or
#'   \code{"difference"}: a single benchmark total \eqn{T}.  For
#'   \code{method = "raking"}: a numeric vector of group totals (one per
#'   group; see \code{groups}).
#' @param weights Optional numeric vector of length equal to the number
#'   of areas in \code{predictions}.  In Official-Statistics practice
#'   this is normally the population size \eqn{N_i} of each area, so
#'   that \eqn{\sum_i w_i \hat{\theta}_i} is the implied population
#'   total.  When \code{NULL}, a safe default is chosen based on
#'   \code{target_type}; an informational message is emitted so users
#'   can see exactly which weighting was applied.
#' @param target_type Character.  Either \code{"total"} (default) or
#'   \code{"mean"}.  Consulted ONLY when \code{weights = NULL} to choose
#'   a safe default: \code{"total"} sets \code{weights = rep(1, n)} so
#'   that \eqn{\sum_i w_i \hat{\theta}_i = \sum_i \hat{\theta}_i};
#'   \code{"mean"} sets \code{weights = rep(1/n, n)} so that the
#'   weighted sum equals the mean.  Ignored when \code{weights} is
#'   supplied.  \strong{Always prefer to pass explicit \code{weights}
#'   (typically the population size \eqn{N_i}) in production code:}
#'   the default heuristic is provided only to avoid silent scale
#'   corruption when the user forgets the argument.
#' @param method Character.  One of:
#'   \describe{
#'     \item{\code{"ratio"}}{Multiplicative:
#'       \eqn{\hat{\theta}_i^B = \hat{\theta}_i \times T /
#'         \sum_j w_j \hat{\theta}_j}.}
#'     \item{\code{"difference"}}{Additive:
#'       \eqn{\hat{\theta}_i^B = \hat{\theta}_i +
#'         (T - \sum_j w_j \hat{\theta}_j) / \sum_j w_j}.}
#'     \item{\code{"raking"}}{Iterative proportional fitting to multiple
#'       group totals.  Requires \code{groups}.}
#'   }
#' @param groups Optional integer/character vector of length equal to the
#'   number of areas, assigning each area to a benchmarking group.
#'   Required for \code{method = "raking"}; ignored otherwise.
#' @param posterior Optional logical or matrix.  Controls Bayesian mode:
#'   \describe{
#'     \item{\code{NULL} or \code{FALSE} (default)}{Point-estimate mode --
#'       same behaviour as v1.0.0.}
#'     \item{\code{TRUE}}{Bayesian mode.  Posterior draws are extracted
#'       from \code{predictions} automatically (requires
#'       \code{predictions$model} to be available).}
#'     \item{a numeric \emph{matrix} (\eqn{D \times n})}{Bayesian mode
#'       with user-supplied draws (\eqn{D} draws, \eqn{n} areas).}
#'   }
#' @param probs Numeric vector of quantile probabilities to summarise the
#'   adjusted posterior with (default \code{c(0.025, 0.5, 0.975)}).
#'   Only used in Bayesian mode.
#' @param max_iter Integer.  Maximum iterations for raking (default
#'   \code{100L}).
#' @param tol Numeric.  Convergence tolerance for raking (default
#'   \code{1e-8}).
#'
#' @return An \code{hbsae_results} object with benchmarked
#'   \code{Prediction} values, plus an additional element
#'   \code{$benchmark_info} that records \code{method}, \code{target},
#'   \code{weights}, the implied \code{adjustment} factor, and
#'   \code{converged} (logical, raking only).  In Bayesian mode the
#'   \code{result_table} also contains updated \code{SD} and
#'   \code{RSE_percent} columns reflecting the post-benchmark posterior
#'   uncertainty, plus quantile columns named after \code{probs}.
#'
#' @details
#' \subsection{When to benchmark}{
#'   Benchmarking is widely used in official statistics to ensure
#'   consistency between model-based small-area estimates and published
#'   aggregate totals from a more reliable source.
#' }
#'
#' \subsection{Why fully Bayesian benchmarking matters}{
#'   Applying a deterministic adjustment factor to a posterior point
#'   estimate distorts the uncertainty structure: a multiplicative
#'   factor scales both the mean and the SD by the same amount (so the
#'   RSE percentage is preserved), but an additive shift does \emph{not}
#'   change the SD, so the RSE percentage shrinks at small areas and
#'   grows at large ones.  Neither of these post-hoc fixes is
#'   guaranteed to be correct in general.  In Bayesian mode every draw
#'   is benchmarked independently, so the resulting posterior carries
#'   the right uncertainty.
#' }
#'
#' @references
#' Pfeffermann, D. (2013).  New important developments in small area
#' estimation.  \emph{Statistical Science} 28(1), 40--68.
#'
#' Wang, J., Fuller, W. A., & Qu, Y. (2008).  Small area estimation
#' under a restriction.  \emph{Survey Methodology} 34, 29--36.
#'
#' @examples
#' # Synthetic predictions (point-estimate mode)
#' p <- structure(
#'   list(
#'     result_table = data.frame(Prediction  = c(10, 12, 9, 11),
#'                                SD          = c(1, 1, 1, 1),
#'                                RSE_percent = c(10, 8, 11, 9)),
#'     rse_model    = 9.5,
#'     pred         = c(10, 12, 9, 11)
#'   ),
#'   class = "hbsae_results"
#' )
#' bm1 <- sae_benchmark(p, target = 50, method = "ratio")
#'
#' # Fully Bayesian mode with user-supplied draws
#' set.seed(1)
#' D <- 1000
#' draws <- matrix(rnorm(D * 4, mean = c(10, 12, 9, 11), sd = 1),
#'                  nrow = D, byrow = TRUE)
#' bm2 <- sae_benchmark(p, target = 50, method = "ratio",
#'                       posterior = draws)
#' bm2$result_table              # SD, RSE updated from draws
#'
#' @seealso \code{\link{sae_predict}}, \code{\link{sae_aggregate}},
#'   \code{\link{model_average}}
#' @export
sae_benchmark <- function(predictions,
                          target,
                          weights     = NULL,
                          target_type = c("total", "mean"),
                          method      = c("ratio", "difference", "raking"),
                          groups      = NULL,
                          posterior   = NULL,
                          probs       = c(0.025, 0.5, 0.975),
                          max_iter    = 100L,
                          tol         = 1e-8) {

  method      <- match.arg(method)

  # -- 1. Input validation ---------------------------------------------------
  if (!is.hbsae_results(predictions))
    stop("'predictions' must be an hbsae_results object.", call. = FALSE)
  if (!is.numeric(target))
    stop("'target' must be numeric.", call. = FALSE)
  if (any(!is.finite(target)))
    stop("'target' must be finite (no NA, NaN, or Inf).", call. = FALSE)

  pred <- predictions$pred
  n    <- length(pred)
  if (any(!is.finite(pred)))
    stop("`predictions$pred` contains non-finite value(s) (NA / NaN / Inf). ",
         "Benchmark cannot proceed.  Check the model fit and prediction step.",
         call. = FALSE)

  # ------------------------------------------------------------------
  # (v1.0.0): Default weights must reflect the SEMANTIC of `target`.
  #
  # The previous default `weights = rep(1/n, n)` silently assumed
  # `target` was a population MEAN, because
  #     sum(weights * pred) = mean(pred).
  # When users instead pass `target = <population TOTAL>` (the
  # overwhelmingly common case in BPS / Official Statistics), the
  # adjustment factor `r = target / mean(pred)` is roughly `n` times
  # larger than the correct one `target / sum(pred)`, producing
  # benchmarked estimates that are scale-corrupt by a factor of `n`.
  #
  # Fix: refuse to silently guess.  Require the user to either
  #   (a) pass `weights` explicitly (recommended -- normally population
  #       sizes N_i, then `sum(weights * pred)` is the population total
  #       estimate), or
  #   (b) declare `target_type = "mean"` or `"total"` so we can pick a
  #       safe default.
  # ------------------------------------------------------------------
  if (is.null(weights)) {
    target_type <- match.arg(target_type)
    weights <- switch(
      target_type,
      "total" = rep(1,     n),   # sum(w * pred) = sum(pred) = total
      "mean"  = rep(1 / n, n),   # sum(w * pred) = mean(pred)
      stop("Default `weights` only supported when `target_type` is ",
           "'total' or 'mean'.  Pass explicit weights (typically the ",
           "population size N_i for each area).", call. = FALSE)
    )
    message(sprintf(
      "Using default weights for `target_type = \"%s\"`.\n",
      target_type),
      "  For production use, pass `weights = <population size per area>` ",
      "to be explicit.")
  } else {
    if (!is.numeric(weights) || length(weights) != n)
      stop(sprintf(
        "'weights' must be numeric of length %d (the number of areas).",
        n), call. = FALSE)
    if (any(weights < 0, na.rm = TRUE))
      stop("All weights must be non-negative.", call. = FALSE)
    if (sum(weights) == 0)
      stop("Sum of weights is zero.", call. = FALSE)
  }

  if (method == "raking") {
    if (is.null(groups))
      stop("'groups' is required for method = 'raking'.", call. = FALSE)
    if (length(groups) != n)
      stop(sprintf(
        "'groups' must have length %d (the number of areas).", n),
        call. = FALSE)
    if (length(target) != length(unique(groups)))
      stop(sprintf(
        "Length of 'target' (%d) must equal number of unique groups (%d).",
        length(target), length(unique(groups))),
        call. = FALSE)
  } else {
    if (length(target) != 1L)
      stop("For method = '", method,
           "', 'target' must be a single numeric value.", call. = FALSE)
  }

  # -- 2. Resolve posterior draws (Bayesian mode) ---------------------------
  draws <- NULL
  if (!is.null(posterior)) {
    if (isTRUE(posterior)) {
      # Try to extract draws from the predictions object
      draws <- .extract_draws_from_predictions(predictions, n)
      if (is.null(draws))
        stop("Could not extract posterior draws from 'predictions'. ",
             "Pass them explicitly as a matrix: ",
             "posterior = posterior_predict(fit) (D x n).",
             call. = FALSE)
    } else if (is.matrix(posterior) && is.numeric(posterior)) {
      if (ncol(posterior) != n)
        stop(sprintf(
          "Posterior matrix must have %d columns (areas); got %d.",
          n, ncol(posterior)), call. = FALSE)
      draws <- posterior
    } else if (isFALSE(posterior)) {
      draws <- NULL
    } else {
      stop("'posterior' must be NULL, TRUE/FALSE, or a numeric matrix.",
           call. = FALSE)
    }
  }

  # -- 3. Apply benchmark on point estimate ---------------------------------
  pt <- .apply_benchmark(pred, target, weights, method, groups,
                          max_iter, tol)
  new_pred <- pt$pred
  bm_info  <- pt$info
  bm_info$target  <- target
  bm_info$weights <- weights

  # -- 4. Build result table ------------------------------------------------
  new_table <- predictions$result_table
  new_table$Prediction <- new_pred

  # -- 5. Fully Bayesian mode: apply per-draw + recompute SD/RSE -----------
  if (!is.null(draws)) {
    D <- nrow(draws)
    adj_draws <- matrix(NA_real_, nrow = D, ncol = n)
    for (d in seq_len(D)) {
      adj_draws[d, ] <- .apply_benchmark(
        draws[d, ], target, weights, method, groups, max_iter, tol
      )$pred
    }

    # Recompute summaries from adjusted draws
    new_sd  <- apply(adj_draws, 2L, stats::sd)
    new_rse <- 100 * new_sd / abs(new_pred)
    quants  <- t(apply(adj_draws, 2L, stats::quantile,
                        probs = probs, na.rm = TRUE))
    colnames(quants) <- paste0("Q",
                                formatC(probs * 100,
                                         format = "fg",
                                         flag = "0"))

    new_table$SD          <- new_sd
    new_table$RSE_percent <- new_rse
    new_table <- cbind(new_table, quants)

    bm_info$posterior_used <- TRUE
    bm_info$n_draws        <- D
  } else {
    bm_info$posterior_used <- FALSE
  }

  # -- 6. Assemble return ----------------------------------------------------
  out <- structure(
    list(
      result_table   = new_table,
      rse_model      = if (!is.null(draws))
                          mean(new_table$RSE_percent, na.rm = TRUE)
                       else predictions$rse_model,
      pred           = new_pred,
      benchmark_info = bm_info
    ),
    class = "hbsae_results"
  )
  if (!is.null(draws)) out$posterior_adjusted <- adj_draws
  out
}


# ---- Internal: try to dig posterior draws out of hbsae_results -----------
# Returns NULL if no draws can be located -- caller must error in that case.
.extract_draws_from_predictions <- function(predictions, n) {
  # Common slots where draws may live
  slot <- predictions$posterior_draws %||%
          predictions$draws            %||%
          NULL
  if (is.matrix(slot) && ncol(slot) == n) return(slot)
  NULL
}
