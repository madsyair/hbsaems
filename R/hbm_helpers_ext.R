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
#' \code{NULL} when \code{prior_type = "default"}.  When \code{main = TRUE},
#' the prior can be cascaded across multiple parameter classes (smooth
#' SDs, GP SDs, etc.) by combining with additional \code{set_prior()}
#' calls -- see \code{?brms::R2D2}.  Use \code{.build_shrinkage_priors_full()}
#' to obtain the combined prior object that spans \code{"b"} +
#' \code{"sds"} (splines) + \code{"sdgp"} (GPs) automatically.
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
#'   compute it automatically from the number of predictors (in
#'   combination with autoscale = TRUE).
#' @param hs_scale_slab   Numeric > 0. Slab scale (default 2).
#' @param hs_par_ratio    Numeric > 0 or NULL. Expected ratio of non-zero to
#'   total coefficients.  Supplied to the horseshoe directly; overrides
#'   \code{hs_scale_global} when set.
#' @param hs_autoscale    Logical. brms autoscale flag (default TRUE).
#'   When TRUE brms automatically scales the prior using the residual SD
#'   sigma (suitable for continuous responses).  Should be FALSE for
#'   discrete responses (binomial, Poisson, ...).
#' @param r2d2_mean_R2    Numeric in (0, 1). Prior mean of R-squared
#'   (default 0.5).
#' @param r2d2_prec_R2    Numeric > 0. Prior precision of R-squared
#'   (default 2); higher values concentrate mass around \code{mean_R2}.
#' @param r2d2_cons_D2    Numeric > 0 or NULL. Dirichlet concentration for
#'   the D2 component.  NULL uses the brms default 0.5.
#' @param r2d2_autoscale  Logical. brms autoscale flag for R2D2 (default TRUE).
#' @param main            Logical (default \code{FALSE}).  When \code{TRUE},
#'   sets \code{main = TRUE} inside the \code{horseshoe()} / \code{R2D2()}
#'   call.  Only relevant when the same shrinkage prior is also supplied
#'   for additional parameter classes (e.g.\ \code{"sds"} for splines,
#'   \code{"sdgp"} for Gaussian processes).  See
#'   \code{\link[brms]{horseshoe}} and \code{\link[brms]{R2D2}}.
#' @return A \code{brmsprior} object or NULL.
#' @noRd
.build_prior_type <- function(prior_type      = "default",
                              hs_df           = 1,
                              hs_df_global    = 1,
                              hs_df_slab      = 4,
                              hs_scale_global = NULL,
                              hs_scale_slab   = 2,
                              hs_par_ratio    = NULL,
                              hs_autoscale    = TRUE,
                              r2d2_mean_R2    = 0.5,
                              r2d2_prec_R2    = 2,
                              r2d2_cons_D2    = NULL,
                              r2d2_autoscale  = TRUE,
                              main            = FALSE,
                              class           = "b") {
  
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
    if (!is.logical(hs_autoscale) || length(hs_autoscale) != 1L)
      stop("'hs_autoscale' must be a single logical.", call. = FALSE)
    if (!is.logical(main) || length(main) != 1L)
      stop("'main' must be a single logical.", call. = FALSE)
    
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
    if (!isTRUE(hs_autoscale))    # only emit if non-default
      args <- paste0(args, ", autoscale = FALSE")
    if (isTRUE(main))
      args <- paste0(args, ", main = TRUE")
    
    return(brms::set_prior(paste0("horseshoe(", args, ")"), class = class))
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
    if (!is.logical(r2d2_autoscale) || length(r2d2_autoscale) != 1L)
      stop("'r2d2_autoscale' must be a single logical.", call. = FALSE)
    if (!is.logical(main) || length(main) != 1L)
      stop("'main' must be a single logical.", call. = FALSE)
    
    args <- paste0(
      "mean_R2 = ", r2d2_mean_R2,
      ", prec_R2 = ", r2d2_prec_R2
    )
    if (!is.null(r2d2_cons_D2))
      args <- paste0(args, ", cons_D2 = ", r2d2_cons_D2)
    if (!isTRUE(r2d2_autoscale))
      args <- paste0(args, ", autoscale = FALSE")
    if (isTRUE(main))
      args <- paste0(args, ", main = TRUE")
    
    return(brms::set_prior(paste0("R2D2(", args, ")"), class = class))
  }
}


#' Build a shrinkage prior that spans regression coefficients,
#' spline SDs, and GP SDs (cascaded brms `main = TRUE` pattern)
#'
#' For models that combine regression coefficients with smooth (\code{s()})
#' or Gaussian-process (\code{gp()}) terms, the user almost always wants
#' the shrinkage prior to apply jointly to ALL of them, not just to the
#' linear \code{b} class.  This helper assembles the brms-canonical
#' "main + companion" prior pattern:
#'
#' \preformatted{
#' set_prior(horseshoe(..., main = TRUE), class = "b") +
#'   set_prior(horseshoe(),                class = "sds")  # if splines
#'   set_prior(horseshoe(),                class = "sdgp") # if GPs
#' }
#'
#' Returns a \code{brmsprior} object or NULL.
#'
#' @param formula \code{brmsformula} or \code{formula}.  Used to detect
#'   the presence of \code{s()} or \code{gp()} terms.
#' @param ...     Forwarded to \code{.build_prior_type()}.
#' @noRd
.build_shrinkage_priors_full <- function(formula, prior_type, ...) {
  
  if (is.null(prior_type) || prior_type == "default") return(NULL)
  
  # Detect smooth / GP terms in the formula
  rhs_str <- if (inherits(formula, "brmsformula")) {
    deparse(formula$formula)
  } else if (inherits(formula, "formula")) {
    deparse(formula)
  } else {
    return(.build_prior_type(prior_type = prior_type, ..., class = "b"))
  }
  rhs_str <- paste(rhs_str, collapse = " ")
  has_s   <- grepl("\\bs\\s*\\(",  rhs_str)
  has_gp  <- grepl("\\bgp\\s*\\(", rhs_str)
  
  # Main prior on coefficients (class = "b")
  main_flag <- has_s || has_gp
  prior_b <- .build_prior_type(prior_type = prior_type,
                               main       = main_flag,
                               class      = "b",
                               ...)
  
  # Companion priors on smooth / GP SD classes
  out <- prior_b
  if (has_s) {
    prior_sds <- .build_prior_type(prior_type = prior_type,
                                   main       = FALSE,
                                   class      = "sds",
                                   ...)
    out <- if (is.null(out)) prior_sds else out + prior_sds
  }
  if (has_gp) {
    prior_sdgp <- .build_prior_type(prior_type = prior_type,
                                    main       = FALSE,
                                    class      = "sdgp",
                                    ...)
    out <- if (is.null(out)) prior_sdgp else out + prior_sdgp
  }
  
  out
}


#' Merge a shrinkage prior with user-supplied priors
#'
#' Handles potential conflicts between an explicitly user-supplied
#' \code{brmsprior} object (\code{user_prior}) and the shrinkage prior
#' automatically built from \code{prior_type}.  Now also handles the
#' cascade case where \code{type_prior} may contain entries for
#' \code{class = "b"}, \code{"sds"}, and \code{"sdgp"} simultaneously.
#'
#' The rule is: for each \code{class} the user-supplied prior takes
#' precedence; the shrinkage prior is silently dropped for that class
#' but kept for any class the user did NOT specify.  A warning is
#' issued only when the user's \code{prior} contains a \emph{global}
#' entry (no coefficient name) that fully overrides the shrinkage
#' prior on that class.
#'
#' @param user_prior A \code{brmsprior} object or NULL.
#' @param type_prior A \code{brmsprior} object from
#'   \code{.build_shrinkage_priors_full()} or NULL.
#' @return A \code{brmsprior} object or NULL.
#' @noRd
.merge_prior_type <- function(user_prior, type_prior) {
  
  if (is.null(type_prior)) return(user_prior)
  if (is.null(user_prior)) return(type_prior)
  
  # For each class that the user has supplied a GLOBAL entry (no coef name),
  # remove the corresponding row(s) from type_prior so the user's prior wins.
  cascade_classes <- c("b", "sds", "sdgp")
  conflict_classes <- character(0L)
  
  for (cls in cascade_classes) {
    user_has_global <- any(
      user_prior$class == cls &
        (is.na(user_prior$coef) | user_prior$coef == "")
    )
    if (user_has_global)
      conflict_classes <- c(conflict_classes, cls)
  }
  
  if (length(conflict_classes) > 0L) {
    warning(
      "A global prior for class = ",
      paste(shQuote(conflict_classes), collapse = ", "),
      " is already present in 'prior'.  The 'prior_type' shrinkage prior ",
      "is IGNORED for ", if (length(conflict_classes) > 1L) "those classes."
      else "that class.",
      "  Remove the corresponding entry from 'prior' if you want ",
      "'prior_type' to apply uniformly.",
      call. = FALSE
    )
    # Drop conflicting rows from type_prior
    type_prior <- type_prior[!type_prior$class %in% conflict_classes, ,
                             drop = FALSE]
    if (nrow(type_prior) == 0L) return(user_prior)
  }
  
  # Combine: shrinkage prior covers its remaining classes; user_prior handles
  # the conflicting classes and any other coefficient-specific overrides.
  c(type_prior, user_prior)
}


# =============================================================================
# B.  NONLINEAR TERM HELPERS
# =============================================================================

#' Build a single nonlinear term string for a brms formula
#'
#' @param var       Character. Variable name.
#' @param type      Character. \code{"spline"} or \code{"gp"}.
#' @param spline_k  Integer. Spline basis dimension.  \code{-1} lets mgcv
#'   choose automatically.
#' @param spline_bs Character.  Spline basis type passed to \pkg{mgcv}
#'   via \code{s(..., bs = ...)}.  Common choices: \code{"tp"} (thin-plate,
#'   default), \code{"cr"} (cubic regression, often more stable for SAE),
#'   \code{"cs"} (cubic with shrinkage), \code{"ps"} (P-splines).
#' @param gp_k      Integer or NA. Number of basis functions for the
#'   Hilbert-space approximate GP (Riutort-Mayol et al.\ 2020).
#'   \code{NA} = exact GP (slow, O(n^3); not recommended for
#'   n > ~100 areas).  Integer = HSGP (recommended).
#' @param gp_cov    Character.  Covariance function: \code{"exp_quad"}
#'   (default), \code{"matern15"}, \code{"matern25"}, \code{"exponential"}.
#' @param gp_c      Numeric or NULL. HSGP boundary-scale factor.  Default
#'   brms = 5/4 (= 1.25).  Only used when \code{gp_k} is supplied.
#' @return A character string such as \code{"s(x1, k = 8)"} or
#'   \code{"gp(x1, k = 25, cov = 'matern25')"}.
#' @noRd
.build_nonlinear_term <- function(var,
                                  type      = c("spline", "gp"),
                                  spline_k  = -1L,
                                  spline_bs = "tp",
                                  gp_k      = NA_integer_,
                                  gp_cov    = "exp_quad",
                                  gp_c      = NULL) {
  type <- match.arg(type)
  
  # ---- Spline branch ------------------------------------------------------
  if (type == "spline") {
    bs_arg <- if (!is.null(spline_bs) && spline_bs != "tp")
      paste0(", bs = \"", spline_bs, "\"") else ""
    if (isTRUE(spline_k == -1L) || is.null(spline_k))
      return(paste0("s(", var, bs_arg, ")"))
    k <- as.integer(spline_k)
    if (k < 3L)
      stop("'spline_k' must be >= 3 (or -1 for automatic).", call. = FALSE)
    return(paste0("s(", var, ", k = ", k, bs_arg, ")"))
  }
  
  # ---- Gaussian process branch -------------------------------------------
  valid_cov <- c("exp_quad", "matern15", "matern25", "exponential")
  if (!is.null(gp_cov) && !gp_cov %in% valid_cov)
    stop("`gp_cov` must be one of: ",
         paste(shQuote(valid_cov), collapse = ", "), ".", call. = FALSE)
  
  parts <- character(0L)
  if (!is.na(gp_k)) {
    k_int <- as.integer(gp_k)
    if (k_int < 3L)
      stop("'gp_k' must be >= 3 (or NA for exact GP).", call. = FALSE)
    parts <- c(parts, paste0("k = ", k_int))
    # brms requires `c` to be set when `k` is supplied (approximate GP).
    # We auto-supply the brms-recommended default 5/4 if the user did not
    # specify gp_c.  This produces a runnable HSGP formula.
    if (is.null(gp_c)) gp_c <- 5 / 4
  }
  if (!is.null(gp_cov) && gp_cov != "exp_quad")
    parts <- c(parts, paste0("cov = \"", gp_cov, "\""))
  if (!is.null(gp_c)) {
    if (!is.numeric(gp_c) || gp_c <= 0)
      stop("'gp_c' must be a positive numeric.", call. = FALSE)
    parts <- c(parts, paste0("c = ", gp_c))
  }
  args_str <- if (length(parts) == 0L) "" else paste0(", ", paste(parts, collapse = ", "))
  paste0("gp(", var, args_str, ")")
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
        " appear in both 'auxiliary' (linear) and 'nonlinear'. ",
        "They will be modelled nonlinearly ONLY. ",
        "Remove them from 'auxiliary' to suppress this warning.",
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
#' @param spline_bs     Character. mgcv basis type.
#' @param gp_k          Integer or NA. HSGP basis dimension.
#' @param gp_cov        Character. Covariance function.
#' @param gp_c          Numeric or NULL. HSGP boundary-scale factor.
#' @return The modified \code{brmsformula}.
#' @noRd
.apply_nonlinear_to_formula <- function(formula,
                                        nonlinear,
                                        nonlinear_type = "spline",
                                        spline_k       = -1L,
                                        spline_bs      = "tp",
                                        gp_k           = NA_integer_,
                                        gp_cov         = "exp_quad",
                                        gp_c           = NULL) {
  
  if (is.null(nonlinear) || length(nonlinear) == 0L) return(formula)
  
  is_bform <- length(class(formula)) > 1L && class(formula)[2L] == "bform"
  
  args <- list(nonlinear      = nonlinear,
               nonlinear_type = nonlinear_type,
               spline_k       = spline_k,
               spline_bs      = spline_bs,
               gp_k           = gp_k,
               gp_cov         = gp_cov,
               gp_c           = gp_c)
  
  if (is_bform) {
    if (!is.null(formula$forms)) {
      formula$forms[[1L]]$formula <- do.call(
        .replace_nl_in_formula,
        c(list(fml = formula$forms[[1L]]$formula), args)
      )
    } else {
      formula$formula <- do.call(
        .replace_nl_in_formula,
        c(list(fml = formula$formula), args)
      )
    }
    return(formula)
  }
  
  if (inherits(formula, "formula")) {
    new_f <- do.call(.replace_nl_in_formula,
                     c(list(fml = formula), args))
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
.replace_nl_in_formula <- function(fml, nonlinear, nonlinear_type,
                                   spline_k  = -1L,
                                   spline_bs = "tp",
                                   gp_k      = NA_integer_,
                                   gp_cov    = "exp_quad",
                                   gp_c      = NULL) {
  
  fml_str   <- paste(deparse(fml), collapse = " ")
  tilde_pos <- regexpr("~", fml_str, fixed = TRUE)
  lhs       <- substr(fml_str, 1L, tilde_pos - 1L)
  rhs       <- substr(fml_str, tilde_pos + 1L, nchar(fml_str))
  
  for (var in nonlinear) {
    nl_term <- .build_nonlinear_term(var, nonlinear_type, spline_k,
                                     spline_bs, gp_k, gp_cov, gp_c)
    # Word-boundary replacement in RHS only to avoid touching LHS or mi() terms
    pattern <- paste0("(?<![a-zA-Z0-9_.])", var, "(?![a-zA-Z0-9_.])")
    rhs     <- gsub(pattern, nl_term, rhs, perl = TRUE)
  }
  
  stats::as.formula(paste(trimws(lhs), "~", trimws(rhs)))
}


#' Build a formula RHS string combining linear and nonlinear predictors
#'
#' Used by \code{hbm_*()} wrappers.  Variables that appear in both
#' \code{auxiliary} and \code{nonlinear} are treated as nonlinear only
#' (removed from the linear part).
#'
#' @param auxiliary      Character vector of linear auxiliary variable
#'   names or NULL.
#' @param nonlinear      Character vector of nonlinear predictor names or NULL.
#' @param nonlinear_type Character. \code{"spline"} or \code{"gp"}.
#' @param spline_k       Integer. Default \code{-1} (auto).
#' @param spline_bs      Character. Default \code{"tp"} (thin-plate).
#' @param gp_k           Integer or NA. Default NA (exact GP).
#' @param gp_cov         Character. Default \code{"exp_quad"}.
#' @param gp_c           Numeric or NULL. HSGP boundary scale.
#' @return A single character string, e.g. \code{"x2 + x3 + s(x1, k = 8)"}.
#' @noRd
.build_wrapper_rhs <- function(auxiliary      = NULL,
                               nonlinear      = NULL,
                               nonlinear_type = "spline",
                               spline_k       = -1L,
                               spline_bs      = "tp",
                               gp_k           = NA_integer_,
                               gp_cov         = "exp_quad",
                               gp_c           = NULL) {
  
  # Remove from the linear set any variable also listed as nonlinear
  linear_vars <- setdiff(auxiliary, nonlinear)
  
  parts <- character(0L)
  
  if (length(linear_vars) > 0L)
    parts <- c(parts, paste(linear_vars, collapse = " + "))
  
  if (!is.null(nonlinear) && length(nonlinear) > 0L) {
    nl_terms <- vapply(
      nonlinear,
      function(v) .build_nonlinear_term(v, nonlinear_type, spline_k,
                                        spline_bs, gp_k, gp_cov, gp_c),
      character(1L)
    )
    parts <- c(parts, nl_terms)
  }
  
  if (length(parts) == 0L) "1"            # intercept-only model
  else paste(parts, collapse = " + ")
}


# =============================================================================
# Internal helpers extracted from hbm() to slim down the main fitting
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
# by hbm() in v1.0.0.  Returns the matrix unchanged on success; stops on
# fatal incompatibility; issues warnings for soft violations.
.validate_spatial_matrix <- function(M, spatial_model) {
  if (is.null(M))
    stop("Spatial matrix `M` must be provided when `spatial_model` is specified.",
         call. = FALSE)
  
  spat_chk <- check_spatial_weight(M, spatial_model = spatial_model,
                                   verbose = FALSE)
  if (!spat_chk$compatible)
    stop(
      "Spatial weight matrix is incompatible with spatial_model = '",
      spatial_model, "':\n  ",
      paste(spat_chk$issues, collapse = "\n  "),
      "\nRun check_spatial_weight(M, spatial_model = '", spatial_model,
      "') for full details.",
      call. = FALSE
    )
  for (w in spat_chk$warnings)
    warning(w, call. = FALSE, immediate. = TRUE)
  M
}


# .parse_hbm_formula() -- Section 1 of hbm() (v1.0.0)
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