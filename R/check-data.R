# R/check-data.R
# =============================================================================
# Standalone data validation utility.
#
# check_data() inspects a data.frame against the structural assumptions of an
# HBSAE model -- variable presence, missing-value patterns, dimensional
# consistency with a spatial weight matrix -- and recommends a missing-data
# handling strategy.
# =============================================================================


#' Inspect Data Before Fitting an HBSAE Model
#'
#' Performs three independent checks on the supplied dataset and returns a
#' structured \code{hbsaems_data_check} object that summarises the results.
#' This function is intended to be called \strong{before} \code{\link{hbm}} or
#' any of the distribution-specific wrappers.
#'
#' @param data        A \code{data.frame}.
#' @param response    Character.  Name of the response variable in
#'   \code{data}.
#' @param auxiliary   Character vector.  Names of the auxiliary
#'   variables (the SAE 'X' covariates).
#' @param predictors  \strong{Deprecated.}  Use \code{auxiliary} instead.
#' @param area_var    Optional character.  Name of the area (random-effect
#'   grouping) variable.
#' @param spatial_var Optional character.  Name of the spatial-grouping
#'   variable (column in \code{data} that indexes rows of the spatial
#'   weight matrix \code{M}).
#' @param M           Optional spatial weight matrix to dimension-check
#'   against \code{data}.
#' @param trials      Optional character.  Name of the trials variable
#'   (binomial models).
#' @param n_var       Optional character.  Name of the sample-size variable
#'   (beta / lognormal direct-estimator models).
#' @param group       \strong{Deprecated.}  Use \code{area_var} instead.
#' @param sre         \strong{Deprecated.}  Use \code{spatial_var} instead.
#'
#' @return An object of class \code{hbsaems_data_check} with components:
#'   \describe{
#'     \item{\code{n_obs}}{Number of rows in the data.}
#'     \item{\code{missing_summary}}{Named integer vector: per-variable
#'       count of \code{NA}.}
#'     \item{\code{missing_pattern}}{Character: \code{"none"}, \code{"y_only"},
#'       \code{"x_only"}, or \code{"both"}.}
#'     \item{\code{dimension_check}}{Named list of dimension diagnostics.}
#'     \item{\code{non_sample_warning}}{Character or \code{NULL} -- a hint
#'       to investigate whether NA-Y rows are non-sample (out-of-sample)
#'       areas.}
#'     \item{\code{recommended_method}}{Character: suggested
#'       \code{handle_missing} value (\code{"deleted"}, \code{"multiple"},
#'       \code{"model"}, or \code{NA} when no missing values are present).}
#'     \item{\code{recommendation_text}}{Human-readable rationale.}
#'     \item{\code{issues}}{Character vector of fatal errors (length 0 if OK).}
#'   }
#'
#' @details
#' \subsection{1. Variable presence}{
#'   Verifies that \code{response}, every name in \code{auxiliary}, and
#'   the optional \code{area_var} / \code{spatial_var} / \code{trials} /
#'   \code{n_var} columns exist in \code{data}.  Missing variables are
#'   reported in \code{$issues}.
#' }
#' \subsection{2. Missing-value pattern}{
#'   The pattern is one of:
#'   \describe{
#'     \item{\code{"none"}}{All listed columns are complete.}
#'     \item{\code{"y_only"}}{Only the response has NAs.}
#'     \item{\code{"x_only"}}{Only the auxiliary variables have NAs.}
#'     \item{\code{"both"}}{Both Y and X have NAs.}
#'   }
#'   Based on the pattern, a strategy is recommended:
#'   \describe{
#'     \item{\code{"y_only"}}{\strong{First, check whether the NA-Y rows
#'       are non-sample (out-of-sample) areas} -- domains for which a
#'       prediction is desired but no direct estimate exists.  If yes, do
#'       \emph{not} delete them; fit on the complete-Y subset and pass the
#'       NA-Y rows to \code{\link{sae_predict}} via the \code{newdata}
#'       argument.  If they are merely missing observations within sampled
#'       areas, use \code{handle_missing = "deleted"}.}
#'     \item{\code{"x_only"}}{\code{handle_missing = "multiple"} -- multiple
#'       imputation via \pkg{mice}.}
#'     \item{\code{"both"} (continuous outcome)}{\code{handle_missing = "model"}
#'       -- joint Bayesian imputation via \code{brms::mi()}.}
#'     \item{\code{"both"} (discrete outcome, binomial)}{\code{handle_missing
#'       = "multiple"}.}
#'   }
#' }
#' \subsection{3. Dimension check}{
#'   When \code{M} is supplied, verifies that it is square and that
#'   \code{nrow(M)} matches the number of \emph{distinct} levels in
#'   \code{data[[spatial_var]]} (or \code{nrow(data)} when
#'   \code{spatial_var} is \code{NULL}).
#' }
#'
#' @examples
#' data("data_fhnorm")
#'
#' # 1. Complete data -> no warnings, no recommendation
#' chk <- check_data(data_fhnorm,
#'                   response   = "y",
#'                   auxiliary  = c("x1", "x2", "x3"))
#' print(chk)
#'
#' # 2. Missing-Y pattern -> recommends checking for non-sample areas
#' d <- data_fhnorm
#' d$y[1:5] <- NA
#' chk2 <- check_data(d, response = "y",
#'                       auxiliary  = c("x1", "x2", "x3"))
#' summary(chk2)
#'
#' # 3. Missing-X-only -> recommends multiple imputation
#' d2 <- data_fhnorm
#' d2$x1[10:15] <- NA
#' chk3 <- check_data(d2, response = "y",
#'                       auxiliary  = c("x1", "x2", "x3"))
#' chk3$recommended_method
#'
#' # 4. Spatial dimension check
#' data("adjacency_matrix_car")
#' chk4 <- check_data(data_fhnorm[1:5, ],
#'                    response   = "y",
#'                    auxiliary  = c("x1", "x2", "x3"),
#'                    M          = adjacency_matrix_car)
#' chk4$dimension_check
#'
#' @seealso \code{\link{hbm}}, \code{\link{sae_predict}}
#' @export
check_data <- function(data,
                       response,
                       auxiliary   = NULL,
                       area_var    = NULL,
                       spatial_var = NULL,
                       M           = NULL,
                       trials      = NULL,
                       n_var       = NULL,
                       # -- DEPRECATED aliases (v1.0.0) -----
                       predictors  = NULL,
                       group       = NULL,
                       sre         = NULL) {

  # -- 0. Deprecated alias handling -----------------------------------------
  if (!is.null(predictors)) {
    if (!is.null(auxiliary))
      stop("Pass either `auxiliary` (preferred) or `predictors` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("predictors", "auxiliary", "v2.0.0")
    auxiliary <- predictors
  }
  if (is.null(auxiliary))
    stop("`auxiliary` (auxiliary variables) is required.", call. = FALSE)

  if (!is.null(group)) {
    if (!is.null(area_var))
      stop("Pass either `area_var` (preferred) or `group` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("group", "area_var", "v2.0.0")
    area_var <- group
  }
  if (!is.null(sre)) {
    if (!is.null(spatial_var))
      stop("Pass either `spatial_var` (preferred) or `sre` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("sre", "spatial_var", "v2.0.0")
    spatial_var <- sre
  }

  # -- 1. Argument validation -------------------------------------------------
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.", call. = FALSE)
  if (!is.character(response) || length(response) != 1L)
    stop("'response' must be a single character string.", call. = FALSE)
  if (!is.character(auxiliary) || length(auxiliary) < 1L)
    stop("'auxiliary' must be a non-empty character vector.",
         call. = FALSE)

  issues <- character(0L)

  # -- 2. Variable presence ---------------------------------------------------
  required_vars <- c(response, auxiliary)
  optional_vars <- c(area_var, spatial_var, trials, n_var)
  optional_vars <- optional_vars[!vapply(optional_vars, is.null, logical(1L))]

  missing_required <- setdiff(required_vars, names(data))
  if (length(missing_required) > 0L)
    issues <- c(issues, sprintf(
      "Required variable(s) not found in 'data': %s",
      paste(missing_required, collapse = ", ")
    ))

  missing_optional <- setdiff(optional_vars, names(data))
  if (length(missing_optional) > 0L)
    issues <- c(issues, sprintf(
      "Optional variable(s) not found in 'data': %s",
      paste(missing_optional, collapse = ", ")
    ))

  # If required variables are missing we cannot proceed with the rest of
  # the diagnostics.  Return early.
  if (length(missing_required) > 0L) {
    out <- list(
      n_obs               = nrow(data),
      missing_summary     = NA_integer_,
      missing_pattern     = NA_character_,
      dimension_check     = list(),
      non_sample_warning  = NULL,
      recommended_method  = NA_character_,
      recommendation_text = "Cannot recommend a strategy until variable issues are fixed.",
      issues              = issues
    )
    class(out) <- c("hbsaems_data_check", "hbsaems_check")
    return(out)
  }

  all_vars <- c(required_vars, intersect(optional_vars, names(data)))

  # -- 3. Missing-value summary ----------------------------------------------
  na_counts <- vapply(all_vars, function(v) sum(is.na(data[[v]])),
                       integer(1L))
  names(na_counts) <- all_vars

  has_na_y <- na_counts[response] > 0L
  has_na_x <- any(na_counts[auxiliary] > 0L)

  pattern <- if (!has_na_y && !has_na_x) "none"
             else if ( has_na_y && !has_na_x) "y_only"
             else if (!has_na_y &&  has_na_x) "x_only"
             else                              "both"

  # -- 4. Non-sample-area heuristic ------------------------------------------
  non_sample_warning <- NULL
  if (has_na_y) {
    n_na_y       <- na_counts[response]
    pct_na_y     <- 100 * n_na_y / nrow(data)
    non_sample_warning <- sprintf(
      paste0(
        "Response '%s' has %d missing value(s) (%.1f%% of rows).\n",
        "  IMPORTANT: Inspect these rows -- are they NON-SAMPLE areas\n",
        "  (out-of-sample domains for which you want PREDICTIONS rather\n",
        "  than estimates)?\n",
        "    * If YES (non-sample): keep these rows in the data, fit the\n",
        "      model on the complete-Y subset, and use sae_predict() with\n",
        "      newdata = <NA-Y rows> to obtain area-level predictions.\n",
        "    * If NO  (truly missing within sampled areas): use\n",
        "      handle_missing = 'deleted' (or 'model' for continuous Y\n",
        "      and 'multiple' for discrete Y)."
      ),
      response, n_na_y, pct_na_y
    )
  }

  # -- 5. Recommended method --------------------------------------------------
  rec <- list(method = NA_character_, text = "")
  if (pattern == "none") {
    rec$method <- NA_character_
    rec$text   <- "No missing values detected. handle_missing can stay NULL."
  } else if (pattern == "y_only") {
    rec$method <- "deleted"
    rec$text   <- paste0(
      "Only the response is missing. Recommended: handle_missing = ",
      "'deleted'. BUT first verify whether the NA-Y rows are non-sample\n",
      "  areas (see non_sample_warning)."
    )
  } else if (pattern == "x_only") {
    rec$method <- "multiple"
    rec$text   <- paste0(
      "Only auxiliary variables are missing. Recommended: handle_missing = ",
      "'multiple' (multiple imputation via mice; works for any family)."
    )
  } else {  # both
    # Heuristic: assume continuous outcome is integer-or-zero-one ratio
    y_vals <- data[[response]]
    looks_integer <- is.integer(y_vals) ||
      (is.numeric(y_vals) &&
       all(abs(y_vals[!is.na(y_vals)] -
                round(y_vals[!is.na(y_vals)])) <
            .Machine$double.eps^0.5))
    if (looks_integer && !is.null(trials)) {
      rec$method <- "multiple"
      rec$text   <- paste0(
        "Both Y and X have NAs and Y appears to be integer (likely ",
        "binomial). Recommended: handle_missing = 'multiple'.\n  ",
        "Note: handle_missing = 'model' is NOT supported for the binomial ",
        "family."
      )
    } else {
      rec$method <- "model"
      rec$text   <- paste0(
        "Both Y and X have NAs. Recommended: handle_missing = 'model' ",
        "(joint Bayesian imputation via mi()).\n  ",
        "Alternative: handle_missing = 'multiple' if the outcome is ",
        "discrete or you prefer mice-based imputation."
      )
    }
  }

  # -- 6. Dimension check ----------------------------------------------------
  dim_check <- list()
  if (!is.null(M)) {
    if (!is.matrix(M))
      issues <- c(issues, "'M' must be a matrix.")
    else if (nrow(M) != ncol(M))
      issues <- c(issues, sprintf(
        "Spatial matrix M must be square; got %d x %d.",
        nrow(M), ncol(M)
      ))
    else {
      n_areas <- if (!is.null(spatial_var) && spatial_var %in% names(data))
        length(unique(stats::na.omit(data[[spatial_var]])))
      else
        nrow(data)

      dim_check$n_areas_data <- n_areas
      dim_check$n_areas_M    <- nrow(M)
      dim_check$M_is_square  <- TRUE
      dim_check$dim_match    <- (n_areas == nrow(M))

      if (!dim_check$dim_match)
        issues <- c(issues, sprintf(
          "Dimension mismatch: M is %d x %d but data has %d %s.",
          nrow(M), nrow(M), n_areas,
          if (!is.null(spatial_var)) paste0("unique '", spatial_var, "' levels")
          else                 "rows"
        ))
    }
  }

  # -- 7. Assemble result ----------------------------------------------------
  out <- list(
    n_obs               = nrow(data),
    missing_summary     = na_counts,
    missing_pattern     = pattern,
    dimension_check     = dim_check,
    non_sample_warning  = non_sample_warning,
    recommended_method  = rec$method,
    recommendation_text = rec$text,
    issues              = issues
  )
  class(out) <- c("hbsaems_data_check", "hbsaems_check")
  out
}


# =============================================================================
# Print / summary methods
# =============================================================================

#' @method print hbsaems_data_check
#' @export
print.hbsaems_data_check <- function(x, ...) {
  cat("\nHBSAE Data Check  [hbsaems_data_check]\n")
  cat("---------------------------------------\n")
  cat(" Observations    :", x$n_obs, "\n")

  cat(" Missing pattern :",
      switch(x$missing_pattern %||% "NA",
             none   = "none (data complete)",
             y_only = "Y only",
             x_only = "X only",
             both   = "both Y and X",
             "unknown"),
      "\n")

  if (length(x$issues) > 0L) {
    cat("\n ! ISSUES:\n")
    for (i in x$issues) cat("    -", i, "\n")
  } else {
    cat(" Issues          : none\n")
  }

  if (!is.null(x$non_sample_warning))
    cat("\n", x$non_sample_warning, "\n", sep = "")

  if (!is.na(x$recommended_method)) {
    cat("\n Recommended handle_missing:",
        sQuote(x$recommended_method), "\n")
    cat(" -", x$recommendation_text, "\n")
  } else if (x$missing_pattern == "none") {
    cat("\n -", x$recommendation_text, "\n")
  }

  cat("\nUse summary() for full details.\n\n")
  invisible(x)
}

#' @method summary hbsaems_data_check
#' @export
summary.hbsaems_data_check <- function(object, ...) {
  cat("\n===== HBSAE Data Check Summary =====\n\n")
  cat("Observations:", object$n_obs, "\n")

  if (length(object$missing_summary) > 0L &&
      !all(is.na(object$missing_summary))) {
    cat("\nMissing values per variable:\n")
    df <- data.frame(
      Variable = names(object$missing_summary),
      NA_count = as.integer(object$missing_summary),
      Pct      = sprintf("%5.1f%%",
                          100 * object$missing_summary / object$n_obs),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    print(df, row.names = FALSE)
  }

  if (length(object$dimension_check) > 0L) {
    cat("\nDimension check:\n")
    for (nm in names(object$dimension_check))
      cat("  ", nm, ":", object$dimension_check[[nm]], "\n")
  }

  if (length(object$issues) > 0L) {
    cat("\nIssues:\n")
    for (i in object$issues) cat("  - ", i, "\n")
  }

  if (!is.null(object$non_sample_warning))
    cat("\nNon-sample-area note:\n", object$non_sample_warning, "\n",
        sep = "")

  if (!is.na(object$recommended_method))
    cat("\nRecommendation:\n  handle_missing =",
        sQuote(object$recommended_method), "\n  ",
        object$recommendation_text, "\n", sep = "")

  invisible(object)
}
