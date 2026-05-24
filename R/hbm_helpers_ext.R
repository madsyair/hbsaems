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


#' Replace bare variable names in a formula RHS with smooth terms (AST-based)
#'
#' \strong{Implementation note.}  Earlier development versions used a
#' Perl-flavoured regular expression on a deparsed RHS string.  That
#' approach was fragile against legal but unusual brms / R syntax such
#' as \code{I(x1 / 1000)}, interactions \code{x1:x2}, offsets
#' \code{offset(log(pop))}, and condition-on syntax \code{y | trials(n)}.
#'
#' We now manipulate the formula via its \emph{abstract syntax tree}:
#' the RHS is decomposed using \code{stats::terms()}, every top-level
#' term whose stripped expression equals the target variable name is
#' rewritten via \code{stats::reformulate()}.  Terms that contain the
#' target inside a function call (e.g.\ \code{I(x1/1000)},
#' \code{s(x1)}, \code{mi(x1, se_x1)}) are LEFT ALONE -- the user has
#' explicitly wrapped them and we do not second-guess.  Interaction
#' terms (\code{x1:x2}, \code{x1*x2}) are likewise preserved bare since
#' brms cannot fit \code{s(x1):x2} or \code{s(x1:x2)} reliably.
#'
#' @noRd
.replace_nl_in_formula <- function(fml, nonlinear, nonlinear_type,
                                    spline_k  = -1L,
                                    spline_bs = "tp",
                                    gp_k      = NA_integer_,
                                    gp_cov    = "exp_quad",
                                    gp_c      = NULL) {

  if (is.null(nonlinear) || length(nonlinear) == 0L) return(fml)

  # Decompose RHS into top-level terms.  `terms()` returns each term's
  # full expression unchanged (it does NOT expand interactions or
  # transformations); we use it purely as a robust tokeniser.
  tt <- tryCatch(stats::terms(fml, keep.order = TRUE),
                  error = function(e) NULL)
  if (is.null(tt)) {
    # If terms() can't parse (e.g. on a multivariate brmsformula),
    # fall back to leaving the formula untouched -- safer than a
    # silently corrupt rewrite.
    return(fml)
  }

  term_labels <- attr(tt, "term.labels")
  has_intercept <- attr(tt, "intercept") == 1L
  response_lang <- if (length(fml) == 3L) fml[[2L]] else NULL
  env <- environment(fml)

  # `terms()` strips offset() terms into a separate `offset` attribute
  # (integer positions into `variables`), so they would otherwise be
  # dropped on reassembly.  Pull them back out as character strings to
  # paste into the RHS verbatim.  Without this, a formula like
  # `y ~ x1 + offset(log(pop))` would lose the offset on rewrite.
  offset_idx <- attr(tt, "offset")
  offset_strs <- character(0L)
  if (!is.null(offset_idx) && length(offset_idx) > 0L) {
    vars <- attr(tt, "variables")  # a call: list(...)
    # `vars` is `list(y, x1, offset(log(pop)))`; index 1 is `list`,
    # response is at 2 (if any), so offset positions are absolute.
    offset_strs <- vapply(offset_idx, function(i)
      deparse(vars[[i + 1L]], width.cutoff = 500L),
      character(1L))
  }

  # For each top-level term, decide whether to rewrite.
  new_term_labels <- vapply(term_labels, function(tl) {
    # The simplest case: term IS the bare variable name.
    if (tl %in% nonlinear) {
      return(.build_nonlinear_term(tl, nonlinear_type, spline_k,
                                     spline_bs, gp_k, gp_cov, gp_c))
    }
    # Otherwise: leave alone.  Either it doesn't reference the target,
    # or it does so inside a wrapper (I(), mi(), s(), :, *, etc.).
    tl
  }, character(1L))

  # Reassemble.  reformulate() handles intercept, response, environment.
  # We splice the preserved offset terms back into the RHS.
  all_labels <- c(new_term_labels, offset_strs)
  rhs_str <- if (length(all_labels) == 0L) {
    if (has_intercept) "1" else "0"
  } else {
    paste(all_labels, collapse = " + ")
  }
  if (!has_intercept) rhs_str <- paste(rhs_str, "- 1")

  if (!is.null(response_lang)) {
    new_fml <- stats::as.formula(
      paste(deparse(response_lang, width.cutoff = 500L), "~", rhs_str),
      env = env
    )
  } else {
    new_fml <- stats::as.formula(paste("~", rhs_str), env = env)
  }
  new_fml
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
#' @param measurement_error A named list mapping auxiliary variable names
#'   to the data column holding their standard error
#'   (\code{list(x1 = "se_x1")}).  Each listed variable is wrapped with
#'   \code{mi(<var>, <se_col>)} so brms treats it as a latent
#'   measurement-error covariate (Ybarra and Lohr 2008).  Variables not
#'   listed are passed through unchanged.  Default \code{NULL}.
#' @return A single character string, e.g. \code{"x2 + x3 + s(x1, k = 8)"}.
#' @noRd
.build_wrapper_rhs <- function(auxiliary         = NULL,
                                nonlinear         = NULL,
                                nonlinear_type    = "spline",
                                spline_k          = -1L,
                                spline_bs         = "tp",
                                gp_k              = NA_integer_,
                                gp_cov            = "exp_quad",
                                gp_c              = NULL,
                                measurement_error = NULL) {

  # Remove from the linear set any variable also listed as nonlinear
  linear_vars <- setdiff(auxiliary, nonlinear)

  parts <- character(0L)

  if (length(linear_vars) > 0L) {
    # (v1.0.0): variables listed in `measurement_error` are wrapped with
    # mi(var, se_col); the rest pass through bare.
    me_names <- names(measurement_error)
    linear_terms <- vapply(linear_vars, function(v) {
      if (!is.null(me_names) && v %in% me_names) {
        sprintf("mi(%s, %s)", v, measurement_error[[v]])
      } else {
        v
      }
    }, character(1L))
    parts <- c(parts, paste(linear_terms, collapse = " + "))
  }

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


#' Extract the bare response-variable name(s) from a formula LHS
#'
#' Walks the LHS expression and returns only the names that play the
#' role of a response.  brms LHS syntax is a mini-DSL with several
#' "addition" wrappers (\code{trials()}, \code{mi()}, \code{cens()},
#' \code{weights()}, \code{vreal()}, \code{vint()}, \code{rate()},
#' \code{trunc()}, etc.) whose arguments name OTHER columns -- those
#' should NOT be picked up as responses.
#'
#' The walker handles:
#' \itemize{
#'   \item Bare names: \code{y} -> \code{"y"}.
#'   \item Transformations: \code{log(y)} -> \code{"y"}.
#'   \item Addition terms: \code{y | mi()}, \code{y | trials(n)}
#'         -> only \code{"y"} (NOT \code{"n"}).
#'   \item Multiple addition terms chained with \code{+}.
#' }
#'
#' Multivariate brmsformulas (\code{bf(y1 ~ ...) + bf(y2 ~ ...)}) are
#' handled by the caller, which calls this helper once per sub-formula.
#'
#' @param lhs A language object (the LHS of a formula).
#' @return Character vector of response-column names.
#' @keywords internal
#' @noRd
.extract_response_names <- function(lhs) {
  if (is.null(lhs)) return(character(0L))

  # Strip the addition operator `|` recursively, keeping only the LEFT
  # branch each time (the right branch contains addition wrappers like
  # mi(), trials(), cens(), ... whose arguments are NOT responses).
  while (is.call(lhs) && identical(lhs[[1L]], as.name("|"))) {
    lhs <- lhs[[2L]]
  }

  # What remains may be a bare name, a transformation, or a sum of
  # responses (rare but legal in brms via cbind() for multinomial / etc.).
  all.vars(lhs)
}


# .parse_hbm_formula() -- Section 1 of hbm() (v1.0.0)
#
# Normalises any of formula(), bf(), brmsformula(), bform() into the four
# objects required by the rest of hbm():
#   $main_formula   -- a base R formula (used for var extraction)
#   $all_formulas   -- a bf() object (used by brms)
#   $response_var   -- character of LHS variable(s) -- ONLY the response
#                       columns (addition-arg columns like trials(n) are
#                       NOT included here)
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

  # Aggregate responses across all sub-formulas of an mvbrmsformula so
  # that downstream missing-value detection sees every response column.
  if (inherits(all_formulas, "mvbrmsformula") &&
      !is.null(all_formulas$forms)) {
    response_var <- unique(unlist(lapply(all_formulas$forms, function(f) {
      lhs <- if (length(f$formula) == 3L) f$formula[[2L]] else NULL
      .extract_response_names(lhs)
    })))
  } else {
    lhs <- if (length(main_formula) == 3L) main_formula[[2L]] else NULL
    response_var <- .extract_response_names(lhs)
  }

  list(
    main_formula   = main_formula,
    all_formulas   = all_formulas,
    response_var   = response_var,
    auxiliary_vars = all.vars(main_formula[[length(main_formula)]])
  )
}


# =============================================================================
# G.  HIERARCHICAL AREA RANDOM-EFFECT BUILDER
# =============================================================================
#
# Construct a brms / lme4-style random-effects formula from a hierarchy of
# area identifiers.  Used by hbm_flex() and all hbm_* wrappers so that
# users can pass either a single area column ("regency") or a vector
# describing nested or crossed hierarchies (e.g. c("province", "regency"))
# without manually writing out the RE formula.
# =============================================================================

#' Build a random-effects formula from area-level columns
#'
#' Translates a character vector of column names into the appropriate
#' brms / lme4 random-intercept syntax.  Supports two structures:
#'
#' \itemize{
#'   \item \code{structure = "nested"} (default for length \eqn{\geq} 2):
#'         lower levels are nested within higher ones.  For
#'         \code{c("province", "regency")} the helper produces
#'         \code{~ (1 | province / regency)}, which brms expands to
#'         \code{(1 | province) + (1 | province:regency)}.
#'   \item \code{structure = "crossed"}: each level contributes an
#'         independent IID random intercept:
#'         \code{~ (1 | province) + (1 | regency)}.
#' }
#'
#' For a single-element \code{area_var} both structures collapse to
#' \code{~ (1 | area_var)}.
#'
#' @param area_var Character vector of column names, ordered from the
#'   \strong{highest} level down to the \strong{lowest} (e.g.
#'   \code{c("province", "regency", "district")}).  \code{NULL} returns
#'   \code{NULL}.
#' @param structure Either \code{"nested"} or \code{"crossed"}.
#'   For a single \code{area_var} the argument has no effect.
#'
#' @return A one-sided \code{formula} or \code{NULL}.
#' @keywords internal
#' @noRd
.build_area_re_formula <- function(area_var, structure = c("nested", "crossed")) {
  if (is.null(area_var) || length(area_var) == 0L) return(NULL)
  if (!is.character(area_var))
    stop("`area_var` must be a character vector of column names.",
         call. = FALSE)
  if (any(!nzchar(area_var)))
    stop("`area_var` must not contain empty strings.", call. = FALSE)
  structure <- match.arg(structure)

  if (length(area_var) == 1L) {
    expr <- paste0("(1 | ", area_var, ")")
  } else if (structure == "nested") {
    expr <- paste0("(1 | ", paste(area_var, collapse = " / "), ")")
  } else {  # crossed
    expr <- paste(sprintf("(1 | %s)", area_var), collapse = " + ")
  }
  stats::as.formula(paste("~", expr))
}


#' Validate hierarchical area_var columns exist and are convertible to factors
#'
#' Issues a clean error message when any column is missing from `data`
#' and a warning when a column contains many levels relative to row count
#' (heuristic for typos like passing a continuous variable as an area).
#'
#' @param area_var Character vector of column names.
#' @param data data.frame.
#' @param max_levels_ratio Numeric.  When \code{n_levels(col) / nrow(data)}
#'   exceeds this ratio, warn -- often a sign the column was meant to be
#'   a continuous covariate, not an area identifier.  Default 0.5.
#' @keywords internal
#' @noRd
.check_area_var_columns <- function(area_var, data, max_levels_ratio = 0.5) {
  if (is.null(area_var) || length(area_var) == 0L) return(invisible(NULL))

  missing_cols <- area_var[!area_var %in% names(data)]
  if (length(missing_cols) > 0L)
    stop("`area_var` column(s) not found in `data`: ",
         paste(shQuote(missing_cols), collapse = ", "),
         call. = FALSE)

  for (col in area_var) {
    n_lev <- length(unique(stats::na.omit(data[[col]])))
    if (n_lev > max_levels_ratio * nrow(data) && n_lev > 5L)
      warning(sprintf(
        "Area column '%s' has %d unique levels for %d rows -- looks ",
        col, n_lev, nrow(data)),
        "more like a continuous covariate than a grouping factor. ",
        "Did you mean to put this in `auxiliary` instead?",
        call. = FALSE)
  }
  invisible(NULL)
}


# =============================================================================
# H.  FAMILY-SPECIFIC SUGAR -> fixed_params TRANSLATORS
# =============================================================================
#
# These helpers translate user-facing sugar arguments (e.g. `sampling_variance`,
# `n + deff`) into entries of the universal `fixed_params` list.  Centralising
# the translations here ensures:
#
#   1.  Single source of truth for each translation rule
#       (no duplicated logic across hbm_flex / hbm_lnln / hbm_betalogitnorm).
#   2.  Consistent conflict checks against user-supplied `fixed_params`.
#   3.  Consistent error messages.
#
# Each helper takes the current `fixed_params` list plus the sugar inputs
# and returns the (possibly augmented) list.  Calls upstream of the
# universal `.process_fixed_params()` machinery in hbm().
# =============================================================================

#' Translate `sampling_variance = "<col>"` to `fixed_params$sigma`.
#'
#' Continuous-family Fay-Herriot sugar.  Pins
#' \eqn{\sigma_i = \sqrt{D_i}} via offset, where \eqn{D_i} is the
#' supplied column.  Errors if a conflicting \code{fixed_params$sigma}
#' is already set.
#'
#' @param fixed_params Current fixed_params list (or NULL).
#' @param sampling_variance Character column name or NULL.
#' @param data data.frame.
#' @return Updated fixed_params list.
#' @keywords internal
#' @noRd
.translate_sampling_variance <- function(fixed_params,
                                          sampling_variance,
                                          data) {
  if (is.null(sampling_variance)) return(fixed_params)

  if (!is.character(sampling_variance) || length(sampling_variance) != 1L)
    stop("`sampling_variance` must be a single column name (character).",
         call. = FALSE)
  if (!sampling_variance %in% names(data))
    stop(sprintf("`sampling_variance = \"%s\"` not found in `data`.",
                  sampling_variance),
         call. = FALSE)

  psi <- data[[sampling_variance]]
  if (any(is.na(psi)) || any(psi <= 0))
    stop("`sampling_variance` must contain finite, strictly positive values.",
         call. = FALSE)

  if (is.list(fixed_params) && "sigma" %in% names(fixed_params))
    stop("Cannot supply both `sampling_variance` and `fixed_params$sigma`. ",
         "Pick one.", call. = FALSE)

  fp <- fixed_params %||% list()
  fp$sigma <- sqrt(psi)
  fp
}


#' Translate `n` + `deff` to `fixed_params$phi` (Beta sugar).
#'
#' Beta Fay-Herriot sugar following Liu (2009).  Computes
#' \eqn{\phi_i = n_i / \mathrm{deff}_i - 1} and pins it via
#' \code{fixed_params$phi}.  Errors if a conflicting
#' \code{fixed_params$phi} is already set, or if either input column
#' contains missing / non-positive values.
#'
#' @param fixed_params Current fixed_params list (or NULL).
#' @param n            Character column name for sample size, or NULL.
#' @param deff         Character column name for design effect, or NULL.
#' @param data         data.frame.
#' @return Updated fixed_params list.
#' @keywords internal
#' @noRd
.translate_n_deff_to_phi <- function(fixed_params, n, deff, data) {
  if (is.null(n) || is.null(deff)) return(fixed_params)

  if (!is.character(n) || length(n) != 1L)
    stop("`n` must be a single column name (character).", call. = FALSE)
  if (!is.character(deff) || length(deff) != 1L)
    stop("`deff` must be a single column name (character).", call. = FALSE)
  if (!n %in% names(data))
    stop(sprintf("`n = \"%s\"` not found in `data`.", n), call. = FALSE)
  if (!deff %in% names(data))
    stop(sprintf("`deff = \"%s\"` not found in `data`.", deff), call. = FALSE)

  if (is.list(fixed_params) && "phi" %in% names(fixed_params))
    stop("Cannot supply both `n` + `deff` (which pin phi via survey ",
         "design) and `fixed_params$phi`.  Pick one.",
         call. = FALSE)

  nvec    <- data[[n]]
  deffvec <- data[[deff]]
  if (anyNA(nvec) || anyNA(deffvec))
    stop("Missing values detected in `n` or `deff`; cannot fix phi.",
         call. = FALSE)
  if (any(nvec <= 0 | deffvec <= 0))
    stop("`n` and `deff` must be strictly positive.", call. = FALSE)
  phi_vec <- nvec / deffvec - 1
  if (any(phi_vec <= 0))
    stop("Computed phi = n/deff - 1 contains non-positive values; ",
         "check `n` and `deff`.", call. = FALSE)

  fp <- fixed_params %||% list()
  fp$phi <- phi_vec
  fp
}


# =============================================================================
# I.  MEASUREMENT-ERROR VALIDATION (Ybarra-Lohr 2008)
# =============================================================================
#
# Validates the structure and contents of the `measurement_error` argument
# used by hbm() and the hbm_* wrappers.  Called once near the top of the
# fit function so that user-facing errors are clear and consistent.
# =============================================================================

#' Validate a measurement_error specification
#'
#' Checks that:
#' \enumerate{
#'   \item \code{measurement_error} is a named list whose names refer to
#'         columns in \code{auxiliary} (the linear predictors).
#'   \item Every listed standard-error column actually exists in
#'         \code{data}.
#'   \item Every standard-error column is non-negative and free of
#'         missing values.
#' }
#'
#' @param measurement_error Named list (\code{list(var = "se_col")}) or
#'   \code{NULL}.
#' @param auxiliary Character vector of auxiliary variable names.
#' @param data data.frame.
#' @return Returns \code{measurement_error} invisibly; called for its
#'   side effect of stopping on invalid input.
#' @keywords internal
#' @noRd
.validate_measurement_error <- function(measurement_error, auxiliary, data) {
  if (is.null(measurement_error)) return(invisible(NULL))

  if (!is.list(measurement_error) || is.null(names(measurement_error)) ||
      any(!nzchar(names(measurement_error))))
    stop("`measurement_error` must be a named list, e.g. ",
         "list(x1 = \"se_x1\", x2 = \"se_x2\").",
         call. = FALSE)

  bad_vars <- setdiff(names(measurement_error), auxiliary)
  if (length(bad_vars) > 0L)
    stop("Variables in `measurement_error` must also appear in ",
         "`auxiliary`. Unmatched: ",
         paste(shQuote(bad_vars), collapse = ", "), ".",
         call. = FALSE)

  for (v in names(measurement_error)) {
    se_col <- measurement_error[[v]]
    if (!is.character(se_col) || length(se_col) != 1L)
      stop(sprintf(
        "measurement_error[['%s']] must be a single column name (character).",
        v), call. = FALSE)
    if (!(se_col %in% names(data)))
      stop(sprintf(
        "Standard-error column \"%s\" (for variable '%s') not found in `data`.",
        se_col, v), call. = FALSE)
    se_vals <- data[[se_col]]
    if (!is.numeric(se_vals))
      stop(sprintf(
        "Standard-error column \"%s\" must be numeric.", se_col),
        call. = FALSE)
    if (anyNA(se_vals))
      stop(sprintf(
        "Standard-error column \"%s\" contains NA values.", se_col),
        call. = FALSE)
    if (any(se_vals < 0))
      stop(sprintf(
        "Standard-error column \"%s\" contains negative values; standard errors must be non-negative.",
        se_col),
        call. = FALSE)
  }
  invisible(measurement_error)
}


#' Detect mi() or me() in a brmsformula
#'
#' Returns TRUE if the formula (or any of its bf() components) syntactically
#' references the brms missing-indicator / measurement-error functions.
#' Used by hbm() to bypass the eager NA-handling logic so that users
#' running joint or ME models are not forced into row-deletion or
#' `handle_missing = "model"` mode.
#'
#' @param formula A brmsformula object.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.formula_has_mi <- function(formula) {
  if (is.null(formula)) return(FALSE)
  # Render every form's main + auxiliary formulas
  rendered <- character(0L)
  one_form_str <- function(x) {
    paste(deparse(x), collapse = " ")
  }
  if (inherits(formula, "brmsformula")) {
    rendered <- c(rendered, one_form_str(formula$formula))
    if (!is.null(formula$pforms))
      rendered <- c(rendered,
                     vapply(formula$pforms, one_form_str, character(1L)))
  } else if (inherits(formula, "mvbrmsformula")) {
    rendered <- vapply(formula$forms, function(f) one_form_str(f$formula),
                       character(1L))
  } else {
    rendered <- one_form_str(formula)
  }
  any(grepl("\\bmi\\s*\\(|\\bme\\s*\\(", rendered, perl = TRUE))
}


#' Rewrite a brmsformula to wrap listed variables with mi(var, se_var)
#'
#' Used by the \code{measurement_error} sugar in \code{hbm()} and the
#' wrapper functions.  The rewrite is AST-based for robustness against
#' interactions, transformations (\code{I(...)}, \code{log(...)},
#' \code{poly(...)}), explicit \code{mi()} or \code{me()} wrappers,
#' and any other syntax legal in a brmsformula.
#'
#' Rewrite rules per top-level term (decomposed via
#' \code{stats::terms()}):
#'
#' \itemize{
#'   \item If the term IS exactly the bare variable name listed in
#'         \code{measurement_error}, rewrite to \code{mi(var, se_col)}.
#'   \item Otherwise (the term is a function call, an interaction,
#'         already wrapped in \code{mi()} / \code{me()}, etc.) leave
#'         the term unchanged.  The user's explicit syntax wins.
#' }
#'
#' @param formula A formula, brmsformula, or mvbrmsformula.
#' @param measurement_error Named list (\code{list(var = "se_col")}).
#' @return The same class as the input, with the RHS rewritten.
#' @keywords internal
#' @noRd
.apply_measurement_error <- function(formula, measurement_error) {
  if (is.null(measurement_error) || length(measurement_error) == 0L)
    return(formula)
  me_names <- names(measurement_error)

  rewrite_one <- function(fml) {
    tt <- tryCatch(stats::terms(fml, keep.order = TRUE),
                    error = function(e) NULL)
    if (is.null(tt)) return(fml)   # safer to skip than corrupt

    term_labels    <- attr(tt, "term.labels")
    has_intercept  <- attr(tt, "intercept") == 1L
    response_lang  <- if (length(fml) == 3L) fml[[2L]] else NULL
    env            <- environment(fml)

    # Preserve offset() terms which terms() strips into a separate
    # attribute (see comment in .replace_nl_in_formula).
    offset_idx <- attr(tt, "offset")
    offset_strs <- character(0L)
    if (!is.null(offset_idx) && length(offset_idx) > 0L) {
      vars <- attr(tt, "variables")
      offset_strs <- vapply(offset_idx, function(i)
        deparse(vars[[i + 1L]], width.cutoff = 500L),
        character(1L))
    }

    new_labels <- vapply(term_labels, function(tl) {
      if (tl %in% me_names)
        sprintf("mi(%s, %s)", tl, measurement_error[[tl]])
      else
        tl
    }, character(1L))

    all_labels <- c(new_labels, offset_strs)
    rhs_str <- if (length(all_labels) == 0L) {
      if (has_intercept) "1" else "0"
    } else {
      paste(all_labels, collapse = " + ")
    }
    if (!has_intercept) rhs_str <- paste(rhs_str, "- 1")

    if (!is.null(response_lang)) {
      stats::as.formula(
        paste(deparse(response_lang, width.cutoff = 500L), "~", rhs_str),
        env = env
      )
    } else {
      stats::as.formula(paste("~", rhs_str), env = env)
    }
  }

  if (inherits(formula, "brmsformula")) {
    formula$formula <- rewrite_one(formula$formula)
    return(formula)
  }
  if (inherits(formula, "mvbrmsformula")) {
    formula$forms <- lapply(formula$forms, function(f) {
      f$formula <- rewrite_one(f$formula)
      f
    })
    return(formula)
  }
  rewrite_one(formula)
}
