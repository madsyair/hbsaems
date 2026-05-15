# =============================================================================
# Internal helper functions for hbm()
#
# Naming convention: prefix "." marks these as internal (non-exported).
# All helpers carry @noRd so roxygen2 never generates a man page for them.
# =============================================================================


# -----------------------------------------------------------------------------
#' Extract response and auxiliary variable names from a formula object
#'
#' Parses either a \code{brmsformula} or a plain \code{formula} and returns
#' the left-hand side (response) and right-hand side (auxiliary/predictor)
#' variable names.  Used internally to avoid duplicating this parsing logic
#' across \code{hbm()}, \code{hbsae()}, and \code{hbpc()}.
#'
#' @param formula A \code{\link[brms]{brmsformula}} or \code{formula} object.
#' @return A named list with two character vectors:
#'   \item{response}{Left-hand side variable name(s).}
#'   \item{auxiliary}{Right-hand side predictor variable name(s).}
#' @noRd
.extract_formula_vars <- function(formula) {
  if (length(class(formula)) > 1L && class(formula)[2L] == "bform") {
    main_formula <- if (!is.null(formula$forms)) {
      as.formula(formula$forms[[1L]])
    } else {
      as.formula(formula$formula)
    }
  } else if (inherits(formula, "formula")) {
    main_formula <- formula
  } else {
    stop(
      "Formula must be a formula() or bf()/brmsformula() object.",
      call. = FALSE
    )
  }

  list(
    response  = all.vars(main_formula[[2L]]),  # LHS: response variable(s)
    auxiliary = all.vars(main_formula[[3L]])   # RHS: predictor variable(s)
  )
}


# -----------------------------------------------------------------------------
#' Detect which variables contain at least one NA
#'
#' Separates missing-value detection into response variables and auxiliary
#' (predictor) variables so that missing-data strategies can be applied
#' selectively.
#'
#' @param data A data frame.
#' @param response_vars Character vector of response variable names.
#' @param auxiliary_vars Character vector of auxiliary (predictor) variable
#'   names.
#' @return A named list:
#'   \item{missing_y}{Character vector of response variables that contain at
#'     least one \code{NA}, or \code{NULL} if none.}
#'   \item{missing_x}{Character vector of auxiliary variables that contain at
#'     least one \code{NA}, or \code{NULL} if none.}
#' @noRd
.detect_missing <- function(data, response_vars, auxiliary_vars) {

  has_na <- function(vars) {
    found <- vars[vapply(vars, function(v) {
      v %in% names(data) && anyNA(data[[v]])
    }, logical(1L))]
    if (length(found) == 0L) NULL else found
  }

  list(
    missing_y = has_na(response_vars),
    missing_x = has_na(auxiliary_vars)
  )
}


# -----------------------------------------------------------------------------
#' Multiple imputation for auxiliary (predictor) variables only
#'
#' Applies \code{\link[mice]{mice}} exclusively to predictor variables.
#' The response variable \eqn{Y} is \strong{never} imputed because in a
#' Bayesian hierarchical model missing outcomes are naturally integrated out
#' from the posterior via
#' \deqn{p(\theta \mid Y_{\text{obs}}, X) =
#'   \int p(\theta \mid Y_{\text{obs}}, Y_{\text{mis}}, X)\,
#'         p(Y_{\text{mis}} \mid Y_{\text{obs}}, X, \theta)\,
#'         \mathrm{d}Y_{\text{mis}}.}
#' Imputing \eqn{Y} before fitting would replace this integral with a point
#' substitute, deflate posterior uncertainty, and introduce bias if the
#' imputation model is misspecified.
#'
#' @details
#' The function proceeds in three steps:
#' \enumerate{
#'   \item Rows where any response variable is \code{NA} are removed.
#'         Those rows are used only for prediction (via \code{\link{hbsae}}),
#'         not for parameter estimation.
#'   \item A logical \code{where} matrix is built for
#'         \code{\link[mice]{mice}}: every cell corresponding to a response
#'         variable column is set to \code{FALSE}, ensuring those columns are
#'         never touched by the imputer.
#'   \item \code{mice} generates \code{m} complete data frames, each suitable
#'         as one element of the list accepted by
#'         \code{\link[brms]{brm_multiple}}.
#' }
#'
#' @param data A data frame (the full original data including rows with
#'   missing \eqn{Y}).
#' @param response_vars Character vector of response variable names.
#' @param auxiliary_vars Character vector of auxiliary variable names.
#' @param m Integer. Number of imputations (default \code{5}).
#' @param mice_args A named list of additional arguments forwarded verbatim to
#'   \code{\link[mice]{mice}} (e.g. \code{method = "pmm"}, \code{seed = 42}).
#' @return A named list:
#'   \item{mids}{A \code{mids} object from \code{mice::mice()} with only X
#'     variables imputed.  Pass directly to \code{brms::brm_multiple()}, which
#'     calls \code{mice::complete()} internally.  \code{NULL} when no X
#'     variables required imputation.}
#'   \item{data_train}{The data frame used for imputation (rows with observed
#'     response).}
#'   \item{any_x_imputed}{Logical; \code{TRUE} when \code{mice} actually had
#'     missing predictor values to impute.  When \code{FALSE}, \code{mids} is
#'     \code{NULL} and the caller should downgrade to \code{brms::brm()}.}
#' @noRd
.mice_impute_x_only <- function(data,
                                response_vars,
                                auxiliary_vars,
                                m         = 5L,
                                mice_args = list()) {

  # Step 1 -- Remove rows with missing response (not used for parameter
  # estimation; they remain in data_complete for prediction).
  complete_y_idx <- stats::complete.cases(data[, response_vars, drop = FALSE])
  data_train     <- data[complete_y_idx, , drop = FALSE]

  # Step 2 -- Check whether any auxiliary variable still has NA after removing
  # the response-missing rows.
  any_x_missing <- any(vapply(auxiliary_vars, function(v) {
    v %in% names(data_train) && anyNA(data_train[[v]])
  }, logical(1L)))

  if (!any_x_missing) {
    # No X is missing: nothing to impute.
    # Return a NULL mids so the caller can decide whether to downgrade to
    # brm() or pass m identical copies of the training data to brm_multiple().
    return(list(
      mids          = NULL,
      data_train    = data_train,
      any_x_imputed = FALSE
    ))
  }

  # Step 3 -- Build the `where` matrix.
  #   TRUE  = "mice should impute this cell"
  #   FALSE = "leave this cell alone"
  where_mat <- is.na(data_train)

  # Explicitly set ALL response columns to FALSE.
  # This is the core guarantee: Y is NEVER imputed by mice.
  response_cols <- intersect(response_vars, colnames(where_mat))
  where_mat[, response_cols] <- FALSE

  # Step 4 -- Run mice, forwarding any user-supplied extra arguments.
  #
  # Use modifyList() instead of c() to avoid duplicate argument names.
  # If the user passes e.g. list(printFlag = FALSE, seed = 42) in mice_args,
  # c() would produce two printFlag entries and mice() would error with
  # "formal argument matched by multiple actual arguments".
  # modifyList() merges the two lists and deduplicates keys, with mice_args
  # values winning over the defaults for any shared key.
  #
  # The critical arguments (data, m, where) are re-asserted last so the user
  # cannot accidentally override the imputation scope.
  mice_call_args <- utils::modifyList(
    list(printFlag = FALSE),   # safe default: suppress mice console output
    as.list(mice_args)         # user overrides (e.g. method, seed, maxit)
  )
  # Non-overridable: these three define the imputation scope and must be ours
  mice_call_args$data  <- data_train
  mice_call_args$m     <- m
  mice_call_args$where <- where_mat

  # Step 5 -- Run mice.  We return the raw mids object, NOT a pre-extracted list.
  #
  # brms::brm_multiple() accepts a mids object directly and calls
  # mice::complete() internally (see brms/R/brm_multiple.R).  Returning the
  # mids object means:
  #   (a) brms controls the complete()-extraction step consistently;
  #   (b) imputation metadata (seed, chain info) is preserved in the brmsfit;
  #   (c) we avoid an unnecessary intermediate object.
  #
  # The only exception is the binomial post-processing step in hbm(), which
  # requires a list of data frames to enforce y <= n.  That caller is
  # responsible for calling mice::complete() itself when needed.
  mids_obj <- do.call(mice::mice, mice_call_args)

  list(
    mids          = mids_obj,   # raw mids -- pass directly to brm_multiple()
    data_train    = data_train,
    any_x_imputed = TRUE
  )
}


#' Add \code{| mi()} to the response variable LHS of a brmsformula
#'
#' Used when \code{hbm()} auto-converts \code{handle_missing = "multiple"} to
#' \code{"model"} because only the response \eqn{Y} is missing and \eqn{X} is
#' complete.  The modification adds \code{| mi()} to the LHS of the main
#' formula so that brms jointly estimates \eqn{Y_\text{mis}} with the model
#' parameters.
#'
#' The helper modifies the formula \emph{string} rather than the parsed object
#' to preserve any existing LHS terms (e.g., \code{y | trunc(lb = 0)}).  It
#' will NOT add \code{| mi()} a second time if \code{mi()} is already present.
#'
#' @param all_formulas A \code{brmsformula} object -- the current value of
#'   \code{all_formulas} inside \code{hbm()}, before random and spatial effects
#'   have been appended.
#' @param response_var Character scalar.  The name of the response variable
#'   whose LHS must be modified.  Only single-response formulas are supported.
#' @return The modified \code{brmsformula}.
#' @noRd
.add_mi_to_lhs <- function(all_formulas, response_var) {

  # Only safe for single-response models.  Multi-response continuous SAE models
  # are rare; the user should write mi() manually in those cases.
  if (length(response_var) != 1L) {
    stop(
      "Automatic formula modification (adding '| mi()') is only supported ",
      "for single-response models.  Please set handle_missing = 'model' ",
      "explicitly and write '", paste(response_var, "| mi()", collapse = ", "),
      "' in your formula.",
      call. = FALSE
    )
  }

  resp <- response_var[1L]

  .patch_formula <- function(fml) {
    fml_str <- paste(deparse(fml), collapse = " ")

    # Do nothing if | mi() is already present on the LHS.
    if (grepl(paste0("\\b", resp, "\\s*\\|.*mi\\s*\\(\\s*\\)"), fml_str, perl = TRUE))
      return(fml)

    # Replace the pattern  "<resp> ~"  with  "<resp> | mi() ~".
    # The regex anchors to the very start of the expression so that it does
    # not accidentally match resp when it appears as a predictor name (e.g.
    # if a predictor happened to share the same name as the response, which
    # would be a user error but should not cause a silent wrong replacement).
    new_str <- sub(
      paste0("^(\\s*)", resp, "(\\s*~)"),
      paste0("\\1", resp, " | mi()\\2"),
      fml_str,
      perl = TRUE
    )

    if (identical(new_str, fml_str)) {
      # Substitution failed -- fall back with an actionable error.
      stop(
        "Could not automatically add '| mi()' to the formula LHS for '",
        resp, "'. ",
        "Please set handle_missing = 'model' explicitly and write '",
        resp, " | mi()' in your formula.",
        call. = FALSE
      )
    }

    tryCatch(
      stats::as.formula(new_str),
      error = function(e) {
        stop(
          "Formula modification failed when adding '| mi()' for '", resp, "': ",
          e$message, "\n",
          "Please set handle_missing = 'model' explicitly and write '",
          resp, " | mi()' in your formula.",
          call. = FALSE
        )
      }
    )
  }

  if (!is.null(all_formulas$forms)) {
    all_formulas$forms[[1L]]$formula <- .patch_formula(
      all_formulas$forms[[1L]]$formula
    )
  } else {
    all_formulas$formula <- .patch_formula(all_formulas$formula)
  }

  all_formulas
}
