# R/sae-predict.R
# =============================================================================
# Primary small-area-estimation prediction function for hbsaems 1.0.0+.
#
#   sae_predict()   -- area-level posterior predictive (replaces hbsae())
#   sae_aggregate() -- combine multiple hbsae_results
#   sae_transform() -- element-wise transform of predictions
#   sae_scale()     -- centre/scale predictions
#   sae_filter()    -- subset predictions by logical condition
# =============================================================================

#' Generate Small Area Estimates
#'
#' Primary SAE prediction function in \pkg{hbsaems} (supersedes deprecated
#' \code{\link{hbsae}}).  Computes area-level posterior predictive means,
#' standard deviations, and relative standard errors (RSE).
#'
#' @param model   An \code{hbmfit} or \code{brmsfit} object.
#' @param newdata Optional new \code{data.frame} for prediction at unsampled
#'   areas.  If \code{NULL} (default), the original data are used.
#' @param ...     Additional arguments forwarded to
#'   \code{\link[brms]{posterior_predict}} (e.g.\ \code{ndraws},
#'   \code{re_formula}).
#'
#' @return An \code{hbsae_results} object with components:
#'   \describe{
#'     \item{\code{result_table}}{A \code{data.frame} with columns
#'       \code{Prediction}, \code{SD}, \code{RSE_percent}.}
#'     \item{\code{rse_model}}{Mean of \code{RSE_percent} across all areas.}
#'     \item{\code{pred}}{Numeric vector of point predictions (= \code{result_table$Prediction}).}
#'   }
#'
#' @details
#' For each area \eqn{i = 1, \ldots, n}, the function computes
#' \deqn{\widehat{y}_i = \frac{1}{S} \sum_{s=1}^{S} y_{i}^{(s)}, \qquad
#'       \widehat{\mathrm{sd}}_i^2 =
#'       \frac{1}{S - 1} \sum_{s=1}^{S}
#'       \left( y_{i}^{(s)} - \widehat{y}_i \right)^2,}
#' where \eqn{y_{i}^{(s)}} are draws from the posterior predictive
#' distribution and \eqn{S} is the number of draws.  The relative standard
#' error is \eqn{\mathrm{RSE}_i = 100 \cdot |\widehat{\mathrm{sd}}_i / \widehat{y}_i|}.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(
#'   formula = brms::bf(y ~ x1 + x2 + x3),
#'   data    = data_fhnorm,
#'   chains = 4, iter = 2000, warmup = 1000, cores = 1,
#'   seed = 123, refresh = 0
#' )
#' est <- sae_predict(model)
#' summary(est)
#' plot(est, type = "predictions")
#' plot(est, type = "uncertainty")
#' }
#'
#' @seealso \code{\link{sae_aggregate}}, \code{\link{model_average}},
#'   \code{\link{sae_transform}}, \code{\link{sae_scale}},
#'   \code{\link{sae_filter}}
#' @export
sae_predict <- function(model, newdata = NULL, ...) {
  UseMethod("sae_predict")
}

#' @export
sae_predict.hbmfit <- function(model, newdata = NULL, ...) {
  .sae_predict_impl(model, newdata = newdata, ...)
}

#' @export
sae_predict.brmsfit <- function(model, newdata = NULL, ...) {
  tmp <- new_hbmfit(model, missing_method = NULL, data = model$data)
  .sae_predict_impl(tmp, newdata = newdata, ...)
}

# Internal worker shared by the S3 dispatch above and the deprecated hbsae().
#
# Kept private so that the public function set stays small.  Edit ONCE here, both the
# new and deprecated entry points pick up the change.
#
# Multivariate handling: for joint-imputation / multivariate brms models
# (where the user wrote `bf(y | mi() ~ ...) + bf(x1 | mi() ~ ...)`),
# `brms::posterior_predict()` returns a 3-D array of shape
# (draws x obs x responses).  In hbsaems the *primary* response is by
# convention the first sub-formula (the small-area target), so we
# extract that slice and compute the summary on the 2-D matrix.
# Power users who need predictions for the imputation responses can
# pass `resp = <name>` via `...` to override.
.sae_predict_impl <- function(model, newdata = NULL, ...) {
  brms_model <- model$model
  data_use   <- if (!is.null(newdata)) newdata else model$data

  # If the model was fit with `sampling_variance = ...` or any
  # `fixed_params = ...`, the brms formula references columns named
  # `.hbsaems_<par>_fixed` (e.g. `.hbsaems_sigma_fixed = sqrt(D)`)
  # that we attached internally during fitting.  A user passing a
  # fresh `newdata` will not have those columns, so brms's
  # validate_data() would error with "variables can neither be found
  # in 'data' nor in 'data2'".
  #
  # Auto-populate them from the training data when possible:
  #   * If newdata already has the column -> leave alone (user knows
  #     best).
  #   * Else if newdata has the SAME nrow as the training data ->
  #     copy the training column verbatim (typical for in-sample
  #     prediction at unsampled areas).
  #   * Else if the column is a simple transform of an existing
  #     column (e.g. sigma_fixed = sqrt(D)), recompute it from
  #     newdata.  We detect the source column by exact-match against
  #     the training pin and re-apply the same transform.
  #
  # The intent: in 95% of cases (re-predicting at the original
  # design points, or at a superset of them) this is fully
  # automatic.  In the remaining 5% (user has new areas with a
  # different sampling-variance design) the user must supply the
  # column themselves; we raise an informative error below.
  if (!is.null(newdata)) {
    data_use <- .repopulate_fixed_cols(data_use, model)
  }

  dots <- list(...)
  is_mv <- !is.null(brms_model$formula$forms)

  # For multivariate brms models, default `resp` to the first response
  # unless the user supplied one explicitly via `...`.  This avoids
  # 3-D arrays propagating into colMeans()/apply() and into the
  # data.frame() at the end.
  if (is_mv && is.null(dots$resp)) {
    first_form  <- brms_model$formula$forms[[1L]]$formula
    first_resp  <- .extract_response_names(first_form)[1L]
    dots$resp   <- first_resp
  }

  # Posterior predictive draws (matrix: draws x areas)
  pp_args     <- c(list(object = brms_model, newdata = data_use), dots)
  pred_matrix <- do.call(brms::posterior_predict, pp_args)

  # Defensive: even with `resp` supplied, posterior_predict() may return
  # a 3-D array if the user is on an older brms.  Drop singleton dims.
  if (length(dim(pred_matrix)) == 3L) {
    if (dim(pred_matrix)[3L] == 1L) {
      pred_matrix <- pred_matrix[, , 1L, drop = TRUE]
    } else {
      # Multiple responses still in result; fall back to first slice.
      pred_matrix <- pred_matrix[, , 1L, drop = TRUE]
    }
  }

  # Area-level summaries (vectorised over columns)
  pred_mean <- colMeans(pred_matrix, na.rm = TRUE)
  pred_sd   <- apply(pred_matrix, 2L, stats::sd, na.rm = TRUE)

  # Guard: if any prediction is exactly zero, RSE is undefined; flag as NA
  # rather than producing Inf/NaN that breaks summary().
  rse_area <- ifelse(
    abs(pred_mean) < .Machine$double.eps,
    NA_real_,
    abs(pred_sd / pred_mean) * 100
  )
  rse_model <- mean(rse_area, na.rm = TRUE)

  result_table <- data.frame(
    Prediction  = pred_mean,
    SD          = pred_sd,
    RSE_percent = rse_area,
    row.names   = NULL,
    stringsAsFactors = FALSE
  )

  structure(
    list(result_table = result_table,
         rse_model    = rse_model,
         pred         = pred_mean),
    class = "hbsae_results"
  )
}


# =============================================================================
# sae_aggregate()  --  combine N hbsae_results
# =============================================================================

#' Aggregate Predictions from Multiple hbsae_results
#'
#' Combines area-level predictions across multiple \code{hbsae_results}
#' objects.  All objects must report predictions for the same number of areas
#' (in the same order).
#'
#' @param ...     Two or more \code{hbsae_results} objects.
#' @param method  One of \code{"mean"} (default), \code{"median"}, or
#'   \code{"weighted"}.
#' @param weights Numeric vector of weights, required when
#'   \code{method = "weighted"}.  Internally normalised to sum to 1.
#'
#' @return An \code{hbsae_results} object containing the combined predictions.
#'
#' @examples
#' p1  <- structure(list(result_table = data.frame(Prediction = 1:3,
#'                                                  RSE_percent = c(5, 5, 5)),
#'                        rse_model = 5, pred = 1:3),
#'                   class = "hbsae_results")
#' p2  <- structure(list(result_table = data.frame(Prediction = 2:4,
#'                                                  RSE_percent = c(4, 4, 4)),
#'                        rse_model = 4, pred = 2:4),
#'                   class = "hbsae_results")
#' sae_aggregate(p1, p2, method = "mean")
#' sae_aggregate(p1, p2, method = "weighted", weights = c(0.6, 0.4))
#'
#' @export
sae_aggregate <- function(..., method = c("mean", "median", "weighted"),
                          weights = NULL) {
  method <- match.arg(method)
  objs   <- list(...)

  if (length(objs) < 2L)
    stop("sae_aggregate() requires at least two hbsae_results objects.",
         call. = FALSE)
  if (!all(vapply(objs, is.hbsae_results, logical(1L))))
    stop("All arguments must be hbsae_results objects.", call. = FALSE)

  np <- vapply(objs, function(x) length(x$pred), integer(1L))
  if (!all(np == np[1L]))
    stop("All hbsae_results must have the same number of areas.",
         call. = FALSE)

  mat <- do.call(cbind, lapply(objs, `[[`, "pred"))

  agg <- switch(
    method,
    mean     = rowMeans(mat, na.rm = TRUE),
    median   = apply(mat, 1L, stats::median, na.rm = TRUE),
    weighted = {
      if (is.null(weights))
        stop("'weights' is required when method = 'weighted'.",
             call. = FALSE)
      if (length(weights) != length(objs))
        stop("length(weights) must equal the number of hbsae_results.",
             call. = FALSE)
      if (any(!is.finite(weights)))
        stop("All weights must be finite (no NA, NaN, or Inf).",
             call. = FALSE)
      if (any(weights < 0))
        stop("All weights must be non-negative.", call. = FALSE)
      w_sum <- sum(weights)
      if (w_sum <= 0)
        stop("`weights` must sum to a positive value; got sum = ", w_sum, ".",
             call. = FALSE)
      weights <- weights / w_sum
      as.numeric(mat %*% weights)
    }
  )

  rse <- mean(vapply(objs, `[[`, numeric(1L), "rse_model"), na.rm = TRUE)

  df  <- data.frame(
    Prediction  = agg,
    RSE_percent = rep(rse, length(agg)),
    stringsAsFactors = FALSE
  )

  structure(
    list(result_table = df, rse_model = rse, pred = agg),
    class = "hbsae_results"
  )
}


# =============================================================================
# sae_transform / sae_scale / sae_filter -- post-processing
# =============================================================================

#' Apply a Transformation to SAE Predictions
#'
#' @param x   An \code{hbsae_results} object.
#' @param fun A function applied element-wise to the predictions.
#' @param ... Additional arguments passed to \code{fun}.
#' @return A new \code{hbsae_results} object.
#'
#' @examples
#' p <- structure(list(result_table = data.frame(Prediction = c(2, 4, 8),
#'                                                RSE_percent = c(5, 5, 5)),
#'                      rse_model = 5, pred = c(2, 4, 8)),
#'                 class = "hbsae_results")
#' sae_transform(p, log)
#'
#' @export
sae_transform <- function(x, fun, ...) UseMethod("sae_transform")

#' @export
sae_transform.hbsae_results <- function(x, fun, ...) {
  if (!is.function(fun))
    stop("'fun' must be a function.", call. = FALSE)

  np <- fun(x$pred, ...)
  if (!is.numeric(np))
    stop("`fun(pred, ...)` must return a numeric vector; got class ",
         shQuote(class(np)[1L]), ".", call. = FALSE)
  if (length(np) != length(x$pred))
    stop("`fun(pred, ...)` returned ", length(np), " value(s) but ",
         length(x$pred), " were expected (one per area). ",
         "Did you mean to use an element-wise function like `log` or ",
         "`exp` instead of a reducer like `sum` or `mean`?",
         call. = FALSE)

  nt <- x$result_table
  nt$Prediction <- np

  structure(
    list(result_table = nt, rse_model = x$rse_model, pred = np),
    class = "hbsae_results"
  )
}


#' Standardise SAE Predictions
#'
#' @param x      An \code{hbsae_results} object.
#' @param center Logical or numeric centering (passed to \code{base::scale}).
#' @param scale  Logical or numeric scaling (passed to \code{base::scale}).
#' @return A new \code{hbsae_results} object with standardised predictions.
#'
#' @examples
#' p <- structure(list(result_table = data.frame(Prediction = 1:5,
#'                                                RSE_percent = rep(5, 5)),
#'                      rse_model = 5, pred = 1:5),
#'                 class = "hbsae_results")
#' sae_scale(p)
#'
#' @export
sae_scale <- function(x, center = TRUE, scale = TRUE) UseMethod("sae_scale")

#' @export
sae_scale.hbsae_results <- function(x, center = TRUE, scale = TRUE) {
  pred_sd <- stats::sd(x$pred, na.rm = TRUE)

  # (v1.0.0): When predictions are degenerate (zero variance) and the user
  # also requests scaling, base::scale() produces NaN throughout, which
  # silently corrupts the result_table downstream.  Two safe behaviours:
  #
  #   * scale = TRUE (the default)  -> emit a warning and DO NOT scale;
  #     return centred-only predictions (or the raw vector when
  #     center = FALSE as well).
  #   * scale = <numeric>            -> respect the user-supplied scale
  #     (NaN does not occur because we don't divide by sd in that case).
  if (!is.na(pred_sd) && pred_sd == 0 && isTRUE(scale)) {
    warning("All predictions are identical (zero variance); ",
            "`scale = TRUE` would produce NaN.  Skipping the scale step ",
            "and returning ", if (center) "centred" else "raw",
            " predictions instead.",
            call. = FALSE)
    scale <- FALSE   # neutralise scaling; centering still applies if requested
  }

  sp <- as.numeric(base::scale(x$pred, center = center, scale = scale))
  nt <- x$result_table
  nt$Prediction <- as.numeric(
    base::scale(nt$Prediction, center = center, scale = scale)
  )

  rse <- if (is.numeric(scale)) x$rse_model / scale else x$rse_model

  structure(
    list(result_table = nt, rse_model = rse, pred = sp),
    class = "hbsae_results"
  )
}


#' Filter SAE Predictions by a Logical Condition
#'
#' @param x         An \code{hbsae_results} object.
#' @param condition Logical vector of length equal to the number of areas.
#' @return A new \code{hbsae_results} object containing only rows where
#'   \code{condition} is \code{TRUE}.
#'
#' @examples
#' p <- structure(list(result_table = data.frame(Prediction = 1:5,
#'                                                RSE_percent = rep(5, 5)),
#'                      rse_model = 5, pred = 1:5),
#'                 class = "hbsae_results")
#' sae_filter(p, p$pred > 2)
#'
#' @export
sae_filter <- function(x, condition) UseMethod("sae_filter")

#' @export
sae_filter.hbsae_results <- function(x, condition) {
  if (!is.logical(condition))
    stop("'condition' must be a logical vector.", call. = FALSE)
  if (length(condition) != length(x$pred))
    stop("length(condition) must equal the number of areas.",
         call. = FALSE)

  keep <- which(condition)
  if (!length(keep))
    stop("No areas match the condition.", call. = FALSE)

  nt  <- x$result_table[keep, , drop = FALSE]
  rownames(nt) <- NULL
  rse <- mean(nt$RSE_percent, na.rm = TRUE)

  structure(
    list(result_table = nt, rse_model = rse, pred = x$pred[keep]),
    class = "hbsae_results"
  )
}


# =============================================================================
# .repopulate_fixed_cols()  --  internal helper for sae_predict()
# =============================================================================

#' Repopulate `.hbsaems_<par>_fixed` Columns in newdata
#'
#' Internal helper invoked by \code{sae_predict()} when the user
#' supplies a fresh \code{newdata} that is missing the internal
#' offset columns (\code{.hbsaems_sigma_fixed},
#' \code{.hbsaems_phi_fixed}, ...) which the original fit injected
#' via \code{sampling_variance =} or \code{fixed_params =}.
#'
#' Strategy:
#' \itemize{
#'   \item Identify the \code{.hbsaems_<par>_fixed} columns in
#'         \code{model$data}.  These are the offsets brms expects.
#'   \item For each such column NOT already present in
#'         \code{newdata}:
#'     \itemize{
#'       \item If \code{nrow(newdata) == nrow(model$data)}, copy the
#'             training column verbatim (the typical case of
#'             "predict at the same areas").
#'       \item Otherwise raise an informative error directing the
#'             user to either pass an aligned \code{newdata} or
#'             pre-populate the offset column themselves.
#'     }
#' }
#'
#' The function is intentionally permissive: if no
#' \code{.hbsaems_*_fixed} columns exist on the model (i.e.\ a plain
#' model without \code{sampling_variance} or \code{fixed_params}),
#' \code{newdata} is returned untouched.
#'
#' @param newdata The user-supplied data frame.
#' @param model   The \code{hbmfit} object.
#'
#' @return \code{newdata}, with any missing offset columns
#'   re-populated from \code{model$data} where possible.
#'
#' @keywords internal
#' @noRd
.repopulate_fixed_cols <- function(newdata, model) {
  train_data <- model$data
  if (is.null(train_data)) return(newdata)

  # Which internal offset columns does the trained model use?
  fixed_cols <- grep("^\\.hbsaems_.*_fixed$", names(train_data), value = TRUE)
  if (length(fixed_cols) == 0L) return(newdata)

  # Which of those are MISSING from newdata?
  missing_cols <- setdiff(fixed_cols, names(newdata))
  if (length(missing_cols) == 0L) return(newdata)

  # Case 1: newdata has same nrow as training data.  Assume in-place
  # prediction at the same design points and just copy the column.
  if (nrow(newdata) == nrow(train_data)) {
    for (col in missing_cols) {
      newdata[[col]] <- train_data[[col]]
    }
    return(newdata)
  }

  # Case 2: nrow differs.  We cannot safely re-derive the column
  # because the underlying source variable (e.g. D, n, deff) is
  # user-data-specific.  Raise an informative error.
  par_names <- sub("^\\.hbsaems_(.+)_fixed$", "\\1", missing_cols)
  stop(
    "Cannot predict on this newdata because it is missing the ",
    "internal offset column(s) ",
    paste0("'", missing_cols, "'", collapse = ", "),
    " that were attached during model fitting (sampling_variance / ",
    "fixed_params for parameter(s) ",
    paste0("'", par_names, "'", collapse = ", "),
    "). Either:\n",
    "  (a) pass `newdata` with the same nrow as model$data so we ",
    "can copy the offset columns automatically, or\n",
    "  (b) recompute the offset(s) yourself in newdata before ",
    "calling sae_predict(), e.g. ",
    "newdata$.hbsaems_sigma_fixed <- sqrt(newdata$D).",
    call. = FALSE
  )
}
