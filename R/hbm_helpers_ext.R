# R/hbm_helpers_ext.R
# =============================================================================
# Internal helper functions for two new modelling features:
#
#   A) Global-local shrinkage priors: Horseshoe and R2D2
#   B) Nonlinear predictor terms:     Splines (mgcv::s) and Gaussian processes
#                                     (brms::gp)
#
# None of these functions are exported; all are prefixed with "." as per the
# package convention for internal helpers.
# =============================================================================


# =============================================================================
# A.  SHRINKAGE PRIOR HELPERS
# =============================================================================

#' Build a global-local shrinkage prior for regression coefficients
#'
#' Returns a \code{brmsprior} object targeting \code{class = "b"}, or
#' \code{NULL} when \code{prior_type = "default"}.
#'
#' @param prior_type      Character. \code{"default"}, \code{"horseshoe"},
#'   or \code{"r2d2"}.
#' @param hs_df           Numeric > 0. Local half-t degrees of freedom
#'   (default 1 = half-Cauchy).
#' @param hs_df_global    Numeric > 0. Global half-t degrees of freedom
#'   (default 1).
#' @param hs_df_slab      Numeric > 0. Slab half-t degrees of freedom
#'   (default 4).
#' @param hs_scale_global Numeric > 0 or NULL. Global scale.  NULL lets brms
#'   compute it automatically from the number of predictors.
#' @param hs_scale_slab   Numeric > 0. Slab scale (default 2).
#' @param hs_par_ratio    Numeric > 0 or NULL. Expected ratio of non-zero to
#'   total coefficients.  NULL assumes all coefficients may be non-zero.
#' @param r2d2_mean_R2    Numeric in (0, 1). Prior mean of R-squared
#'   (default 0.5).
#' @param r2d2_prec_R2    Numeric > 0. Prior precision of R-squared
#'   (default 2); higher values concentrate mass around \code{mean_R2}.
#' @param r2d2_cons_D2    Numeric > 0 or NULL. Dirichlet concentration for
#'   the D2 component.  NULL defaults to 0.5 (uniform over the simplex).
#' @return A \code{brmsprior} object or NULL.
#' @noRd
.build_prior_type <- function(prior_type      = "default",
                               hs_df           = 1,
                               hs_df_global    = 1,
                               hs_df_slab      = 4,
                               hs_scale_global = NULL,
                               hs_scale_slab   = 2,
                               hs_par_ratio    = NULL,
                               r2d2_mean_R2    = 0.5,
                               r2d2_prec_R2    = 2,
                               r2d2_cons_D2    = NULL) {

  if (is.null(prior_type) || prior_type == "default") return(NULL)

  prior_type <- match.arg(prior_type, c("default", "horseshoe", "r2d2"))

  # ---- Horseshoe (Piironen & Vehtari 2017) ----------------------------------
  if (prior_type == "horseshoe") {

    if (!is.numeric(hs_df)        || hs_df <= 0)
      stop("'hs_df' must be a positive number.", call. = FALSE)
    if (!is.numeric(hs_df_global) || hs_df_global <= 0)
      stop("'hs_df_global' must be a positive number.", call. = FALSE)
    if (!is.numeric(hs_df_slab)   || hs_df_slab <= 0)
      stop("'hs_df_slab' must be a positive number.", call. = FALSE)
    if (!is.numeric(hs_scale_slab) || hs_scale_slab <= 0)
      stop("'hs_scale_slab' must be a positive number.", call. = FALSE)
    if (!is.null(hs_scale_global) &&
        (!is.numeric(hs_scale_global) || hs_scale_global <= 0))
      stop("'hs_scale_global' must be NULL or a positive number.", call. = FALSE)
    if (!is.null(hs_par_ratio) &&
        (!is.numeric(hs_par_ratio) || hs_par_ratio <= 0))
      stop("'hs_par_ratio' must be NULL or a positive number.", call. = FALSE)

    args <- paste0(
      "df = ",        hs_df,
      ", df_global = ", hs_df_global,
      ", df_slab = ",   hs_df_slab,
      ", scale_slab = ", hs_scale_slab
    )
    if (!is.null(hs_scale_global))
      args <- paste0(args, ", scale_global = ", hs_scale_global)
    if (!is.null(hs_par_ratio))
      args <- paste0(args, ", par_ratio = ",    hs_par_ratio)

    return(brms::set_prior(paste0("horseshoe(", args, ")"), class = "b"))
  }

  # ---- R2D2 (Zhang et al. 2022) ---------------------------------------------
  if (prior_type == "r2d2") {

    if (!is.numeric(r2d2_mean_R2) || r2d2_mean_R2 <= 0 || r2d2_mean_R2 >= 1)
      stop("'r2d2_mean_R2' must be strictly in (0, 1).", call. = FALSE)
    if (!is.numeric(r2d2_prec_R2) || r2d2_prec_R2 <= 0)
      stop("'r2d2_prec_R2' must be a positive number.", call. = FALSE)
    if (!is.null(r2d2_cons_D2) &&
        (!is.numeric(r2d2_cons_D2) || r2d2_cons_D2 <= 0))
      stop("'r2d2_cons_D2' must be NULL or a positive number.", call. = FALSE)

    args <- paste0(
      "mean_R2 = ", r2d2_mean_R2,
      ", prec_R2 = ", r2d2_prec_R2
    )
    if (!is.null(r2d2_cons_D2))
      args <- paste0(args, ", cons_D2 = ", r2d2_cons_D2)

    return(brms::set_prior(paste0("R2D2(", args, ")"), class = "b"))
  }
}


#' Merge a shrinkage prior with user-supplied priors
#'
#' If the user has already specified a global \code{class = "b"} prior, the
#' shrinkage prior is skipped and a warning is issued.  Otherwise the two
#' \code{brmsprior} objects are combined with \code{c()}.
#'
#' @param user_prior A \code{brmsprior} object or NULL.
#' @param type_prior A \code{brmsprior} object from \code{.build_prior_type()}
#'   or NULL.
#' @return A \code{brmsprior} object or NULL.
#' @noRd
.merge_prior_type <- function(user_prior, type_prior) {

  if (is.null(type_prior)) return(user_prior)
  if (is.null(user_prior)) return(type_prior)

  # A global class="b" prior in user_prior would conflict with the shrinkage
  # prior, so we warn and let the user's explicit specification win.
  user_has_global_b <- any(
    user_prior$class == "b" &
      (is.na(user_prior$coef) | user_prior$coef == "")
  )

  if (user_has_global_b) {
    warning(
      "A global prior for class = 'b' is already present in 'prior'. ",
      "The 'prior_type' shrinkage prior is IGNORED for class = 'b'. ",
      "Remove the class = 'b' entry from 'prior' if you want 'prior_type' to apply.",
      call. = FALSE
    )
    return(user_prior)
  }

  # Combine: shrinkage prior covers class="b" globally; user_prior handles
  # all other classes or coefficient-specific overrides.
  c(type_prior, user_prior)
}


# =============================================================================
# B.  NONLINEAR TERM HELPERS
# =============================================================================

#' Build a single nonlinear term string for a brms formula
#'
#' @param var      Character. Variable name.
#' @param type     Character. \code{"spline"} or \code{"gp"}.
#' @param spline_k Integer. Spline basis dimension.  \code{-1} lets mgcv
#'   choose automatically.
#' @param gp_scale Numeric or NULL. GP length-scale (\code{c} in
#'   \code{brms::gp()}).  NULL uses the brms default.
#' @return A character string such as \code{"s(x1, k = 8)"} or
#'   \code{"gp(x1)"}.
#' @noRd
.build_nonlinear_term <- function(var,
                                   type     = c("spline", "gp"),
                                   spline_k = -1L,
                                   gp_scale = NULL) {
  type <- match.arg(type)

  if (type == "spline") {
    if (isTRUE(spline_k == -1L) || is.null(spline_k))
      return(paste0("s(", var, ")"))
    k <- as.integer(spline_k)
    if (k < 3L)
      stop("'spline_k' must be >= 3 (or -1 for automatic).", call. = FALSE)
    return(paste0("s(", var, ", k = ", k, ")"))
  }

  # Gaussian process
  if (!is.null(gp_scale))
    return(paste0("gp(", var, ", c = ", gp_scale, ")"))
  paste0("gp(", var, ")")
}


#' Validate nonlinear variable names
#'
#' Stops if any element of \code{nonlinear} is absent from \code{data}.
#' Warns when a variable appears in both \code{aux_vars} (linear) and
#' \code{nonlinear}, because in that case the variable is modelled
#' nonlinearly only.
#'
#' @param nonlinear Character vector or NULL.
#' @param aux_vars  Character vector of linear predictor names or NULL.
#' @param data      A data.frame.
#' @noRd
.validate_nonlinear <- function(nonlinear, aux_vars, data) {

  if (is.null(nonlinear) || length(nonlinear) == 0L) return(invisible(NULL))

  if (!is.character(nonlinear))
    stop("'nonlinear' must be a character vector of column names.", call. = FALSE)

  # Every nonlinear variable must exist in data
  absent <- setdiff(nonlinear, names(data))
  if (length(absent) > 0L)
    stop(
      "Nonlinear variable(s) not found in data: ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )

  # Warn about overlap: the variable will be treated as nonlinear only
  if (!is.null(aux_vars)) {
    overlap <- intersect(nonlinear, aux_vars)
    if (length(overlap) > 0L)
      warning(
        "Variable(s) ", paste(overlap, collapse = ", "),
        " appear in both 'predictors' (linear) and 'nonlinear'. ",
        "They will be modelled nonlinearly ONLY. ",
        "Remove them from 'predictors' to suppress this warning.",
        call. = FALSE
      )
  }

  invisible(NULL)
}


#' Modify a formula by replacing listed variables with smooth terms
#'
#' Replaces each variable in \code{nonlinear} with its corresponding
#' \code{s(var)} or \code{gp(var)} term in the RHS of the formula.
#' The LHS is never modified.  Handles both plain \code{formula} and
#' \code{brmsformula} objects.
#'
#' @param formula       A \code{formula} or \code{brmsformula}.
#' @param nonlinear     Character vector of variable names to replace.
#' @param nonlinear_type Character. \code{"spline"} or \code{"gp"}.
#' @param spline_k      Integer.  Passed to \code{.build_nonlinear_term()}.
#' @param gp_scale      Numeric or NULL.
#' @return The modified \code{brmsformula}.
#' @noRd
.apply_nonlinear_to_formula <- function(formula,
                                         nonlinear,
                                         nonlinear_type = "spline",
                                         spline_k       = -1L,
                                         gp_scale       = NULL) {

  if (is.null(nonlinear) || length(nonlinear) == 0L) return(formula)

  is_bform <- length(class(formula)) > 1L && class(formula)[2L] == "bform"

  if (is_bform) {
    if (!is.null(formula$forms)) {
      formula$forms[[1L]]$formula <- .replace_nl_in_formula(
        formula$forms[[1L]]$formula, nonlinear, nonlinear_type, spline_k, gp_scale
      )
    } else {
      formula$formula <- .replace_nl_in_formula(
        formula$formula, nonlinear, nonlinear_type, spline_k, gp_scale
      )
    }
    return(formula)
  }

  if (inherits(formula, "formula")) {
    new_f <- .replace_nl_in_formula(formula, nonlinear, nonlinear_type, spline_k, gp_scale)
    return(brms::bf(new_f))
  }

  formula   # fallback: return unchanged
}


#' Replace bare variable names in a formula RHS with smooth terms
#'
#' Uses a Perl word-boundary pattern to avoid partial substitutions
#' (e.g. replacing "x1" inside "x10" or "mi(x1)").
#'
#' @noRd
.replace_nl_in_formula <- function(fml, nonlinear, nonlinear_type, spline_k, gp_scale) {

  fml_str   <- paste(deparse(fml), collapse = " ")
  tilde_pos <- regexpr("~", fml_str, fixed = TRUE)
  lhs       <- substr(fml_str, 1L, tilde_pos - 1L)
  rhs       <- substr(fml_str, tilde_pos + 1L, nchar(fml_str))

  for (var in nonlinear) {
    nl_term <- .build_nonlinear_term(var, nonlinear_type, spline_k, gp_scale)
    # Word-boundary replacement in RHS only to avoid touching LHS or mi() terms
    pattern <- paste0("(?<![a-zA-Z0-9_.])", var, "(?![a-zA-Z0-9_.])")
    rhs     <- gsub(pattern, nl_term, rhs, perl = TRUE)
  }

  stats::as.formula(paste(trimws(lhs), "~", trimws(rhs)))
}


#' Build a formula RHS string combining linear and nonlinear predictors
#'
#' Used by \code{hbm_*()} wrappers.  Variables that appear in both
#' \code{predictors} and \code{nonlinear} are treated as nonlinear only
#' (removed from the linear part).
#'
#' @param predictors     Character vector of linear predictor names or NULL.
#' @param nonlinear      Character vector of nonlinear predictor names or NULL.
#' @param nonlinear_type Character. \code{"spline"} or \code{"gp"}.
#' @param spline_k       Integer.
#' @param gp_scale       Numeric or NULL.
#' @return A single character string, e.g. \code{"x2 + x3 + s(x1, k = 8)"}.
#' @noRd
.build_wrapper_rhs <- function(predictors     = NULL,
                                nonlinear      = NULL,
                                nonlinear_type = "spline",
                                spline_k       = -1L,
                                gp_scale       = NULL) {

  # Remove from the linear set any variable also listed as nonlinear
  linear_vars <- setdiff(predictors, nonlinear)

  parts <- character(0L)

  if (length(linear_vars) > 0L)
    parts <- c(parts, paste(linear_vars, collapse = " + "))

  if (!is.null(nonlinear) && length(nonlinear) > 0L) {
    nl_terms <- vapply(
      nonlinear,
      function(v) .build_nonlinear_term(v, nonlinear_type, spline_k, gp_scale),
      character(1L)
    )
    parts <- c(parts, nl_terms)
  }

  if (length(parts) == 0L) "1"            # intercept-only model
  else paste(parts, collapse = " + ")
}


# =============================================================================
# v0.4.1: Internal helpers extracted from hbm() to slim down the main fitting
# function and improve testability.
# =============================================================================


# .validate_hbm_data() -- Section 0 of hbm()
#
# Accepts any data.frame-like object (tibble, data.table) and returns a
# plain data.frame.  Rejects everything else with a helpful error.
.validate_hbm_data <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "'data' must be a data.frame (or a tibble / data.frame-like object). ",
      "Received an object of class: ",
      paste(class(data), collapse = ", "),
      call. = FALSE
    )
  }
  as.data.frame(data)
}


# .validate_spatial_matrix() -- Section 9 of hbm()
#
# Wraps check_spatial_weight() with the error/warning emission policy used
# by hbm() in v0.4.0.  Returns the matrix unchanged on success; stops on
# fatal incompatibility; issues warnings for soft violations.
.validate_spatial_matrix <- function(M, sre_type) {
  if (is.null(M))
    stop("Spatial matrix `M` must be provided when `sre_type` is specified.",
         call. = FALSE)

  spat_chk <- check_spatial_weight(M, sre_type = sre_type, verbose = FALSE)
  if (!spat_chk$compatible)
    stop(
      "Spatial weight matrix is incompatible with sre_type = '",
      sre_type, "':\n  ",
      paste(spat_chk$issues, collapse = "\n  "),
      "\nRun check_spatial_weight(M, sre_type = '", sre_type,
      "') for full details.",
      call. = FALSE
    )
  for (w in spat_chk$warnings)
    warning(w, call. = FALSE, immediate. = TRUE)
  M
}


# .parse_hbm_formula() -- Section 1 of hbm() (v0.5.0)
#
# Normalises any of formula(), bf(), brmsformula(), bform() into the four
# objects required by the rest of hbm():
#   $main_formula   -- a base R formula (used for var extraction)
#   $all_formulas   -- a bf() object (used by brms)
#   $response_var   -- character of LHS variable(s)
#   $auxiliary_vars -- character of RHS variable(s)
.parse_hbm_formula <- function(formula) {
  if (length(class(formula)) > 1L && class(formula)[2L] == "bform") {
    if (!is.null(formula$forms)) {
      main_formula <- stats::as.formula(formula$forms[[1L]])
      all_formulas <- formula
    } else {
      main_formula <- stats::as.formula(formula$formula)
      all_formulas <- formula
    }
  } else if (inherits(formula, "formula")) {
    main_formula <- formula
    all_formulas <- brms::bf(formula)
  } else {
    stop("Formula must be specified as formula() or bf()/brmsformula().",
         call. = FALSE)
  }

  list(
    main_formula   = main_formula,
    all_formulas   = all_formulas,
    response_var   = all.vars(main_formula[[2L]]),
    auxiliary_vars = all.vars(main_formula[[3L]])
  )
}
