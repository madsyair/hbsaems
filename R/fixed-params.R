# R/fixed-params.R
# =============================================================================
# Helpers for the `fixed_params` argument in hbm() / hbm_flex().
#
# `fixed_params` lets the user pin a distributional parameter (e.g. phi for
# Beta, sigma for lognormal) to known values that are either constant or
# computed from the data.  It is the generic counterpart of the convenience
# arguments `n` + `deff` in hbm_betalogitnorm() and `sampling_var` in
# hbm_lnln().
#
# Specification format (passed by the user)
# -----------------------------------------
#   fixed_params = list(
#     <par_name> = <spec>,
#     ...
#   )
#
# <spec> may be:
#   * character (length 1)  : name of a column in `data`
#   * numeric   (length 1)  : a scalar, broadcast to all rows
#   * numeric   (length n)  : a vector of length nrow(data)
#   * formula   (one-sided) : an expression evaluated against `data`,
#                              e.g. `~ I(n / deff - 1)`
#
# All four forms resolve to a numeric vector of length nrow(data) which is
# attached to `data` as `.hbsaems_<par>_fixed` and threaded into the brms
# formula as `<par> ~ 0 + offset(.hbsaems_<par>_fixed)`.
#
# This file is internal: nothing here is exported.
# =============================================================================


# Resolve one fixed-param spec to a numeric vector of length nrow(data).
# Throws an informative error on bad input.
.resolve_fixed_param <- function(par, spec, data) {
  n_rows <- nrow(data)

  # 1. Character column name
  if (is.character(spec) && length(spec) == 1L && !is.na(spec)) {
    if (!spec %in% names(data))
      stop(sprintf(
        "fixed_params$%s = \"%s\" is not a column in `data`.",
        par, spec
      ), call. = FALSE)
    vec <- data[[spec]]
    if (!is.numeric(vec))
      stop(sprintf(
        "fixed_params$%s: column \"%s\" must be numeric.",
        par, spec
      ), call. = FALSE)
    return(as.numeric(vec))
  }

  # 2. Numeric vector (scalar or length n)
  if (is.numeric(spec)) {
    if (length(spec) == 1L) return(rep(as.numeric(spec), n_rows))
    if (length(spec) == n_rows) return(as.numeric(spec))
    stop(sprintf(
      "fixed_params$%s: numeric spec must be length 1 (scalar) or length %d (nrow(data)); got length %d.",
      par, n_rows, length(spec)
    ), call. = FALSE)
  }

  # 3. Formula (one-sided expression)
  if (inherits(spec, "formula")) {
    if (length(spec) != 2L)
      stop(sprintf(
        "fixed_params$%s: formula must be one-sided (e.g. ~ I(n/deff - 1)).",
        par
      ), call. = FALSE)
    expr <- spec[[2L]]
    val <- tryCatch(
      eval(expr, envir = data, enclos = environment(spec)),
      error = function(e) {
        stop(sprintf(
          "fixed_params$%s: failed to evaluate formula `%s`:\n  %s",
          par, deparse(spec), conditionMessage(e)
        ), call. = FALSE)
      }
    )
    if (!is.numeric(val))
      stop(sprintf(
        "fixed_params$%s: formula must evaluate to a numeric vector.",
        par
      ), call. = FALSE)
    if (length(val) == 1L) val <- rep(val, n_rows)
    if (length(val) != n_rows)
      stop(sprintf(
        "fixed_params$%s: formula resulted in length %d; expected 1 or %d.",
        par, length(val), n_rows
      ), call. = FALSE)
    return(as.numeric(val))
  }

  stop(sprintf(
    "fixed_params$%s: spec must be a column name, numeric value/vector, or one-sided formula.",
    par
  ), call. = FALSE)
}


# Validate the full fixed_params list and resolve each entry.  Returns a
# list with structure:
#   list(
#     resolved = list(<par> = numeric_vector, ...),
#     col_names = c(<par> = ".hbsaems_<par>_fixed", ...)
#   )
.process_fixed_params <- function(fixed_params, data) {

  if (is.null(fixed_params)) return(list(resolved = list(), col_names = character(0)))

  if (!is.list(fixed_params))
    stop("`fixed_params` must be a named list.", call. = FALSE)
  if (length(fixed_params) == 0L)
    return(list(resolved = list(), col_names = character(0)))
  if (is.null(names(fixed_params)) || any(!nzchar(names(fixed_params))))
    stop("`fixed_params` must have non-empty names for every entry.",
         call. = FALSE)

  resolved  <- list()
  col_names <- character(0)
  for (par in names(fixed_params)) {
    vec <- .resolve_fixed_param(par, fixed_params[[par]], data)

    # Sanity checks
    if (anyNA(vec))
      stop(sprintf(
        "fixed_params$%s contains NA values; all fixed values must be finite.",
        par
      ), call. = FALSE)
    if (any(!is.finite(vec)))
      stop(sprintf(
        "fixed_params$%s contains non-finite values (Inf/-Inf).",
        par
      ), call. = FALSE)

    resolved[[par]]  <- vec
    col_names[[par]] <- paste0(".hbsaems_", par, "_fixed")
  }
  list(resolved = resolved, col_names = col_names)
}


# Attach the resolved vectors as columns of `data` (so brms can find them).
# Returns the augmented data frame.
.attach_fixed_columns <- function(data, processed) {
  for (par in names(processed$resolved)) {
    col <- processed$col_names[[par]]
    if (col %in% names(data) && !identical(data[[col]], processed$resolved[[par]]))
      stop(sprintf(
        "Internal naming collision: column %s already exists in `data` with different values.",
        col
      ), call. = FALSE)
    data[[col]] <- processed$resolved[[par]]
  }
  data
}


# Augment a brms::bf() object with `<par> ~ 0 + offset(<col>)` for each
# fixed parameter.  Returns the augmented bform.
.add_fixed_pforms <- function(bform, processed) {
  if (length(processed$resolved) == 0L) return(bform)
  for (par in names(processed$resolved)) {
    col <- processed$col_names[[par]]
    rhs <- stats::as.formula(
      paste0(par, " ~ 0 + offset(", col, ")")
    )
    bform <- bform + brms::lf(rhs)
  }
  bform
}


# Quick "is this parameter pinned via fixed_params?" lookup.
.is_fixed <- function(par, processed) {
  par %in% names(processed$resolved)
}


# Inspect a brmsstanvars object and return the set of variable names that
# appear on the LHS of a sampling statement (`<var> ~ <distribution>`) in
# the "model" block.  Used by hbm() to detect a conflict between a pinned
# dpar (set via fixed_params) and a user-supplied stanvars sampling
# statement targeting that same dpar.
#
# Similar in spirit to .extract_model_block_vars() in hbm_betalogitnorm.R,
# but extracted here for general reuse.
#
# @noRd
.extract_stanvar_model_targets <- function(stanvars) {
  if (is.null(stanvars)) return(character(0))
  if (!inherits(stanvars, c("stanvars", "stanvar"))) return(character(0))
  sv_list <- if (inherits(stanvars, "stanvar")) list(stanvars) else unclass(stanvars)
  vars <- character(0)
  for (sv in sv_list) {
    if (!is.null(sv$block) && identical(sv$block, "model")) {
      # Each scode may contain multiple lines; scan each for `<name> ~ ...`
      lines <- strsplit(sv$scode, "[;\n]")[[1L]]
      for (ln in lines) {
        m  <- regmatches(ln,
                          regexec("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*~", ln))[[1L]]
        if (length(m) >= 2L) vars <- c(vars, m[2L])
      }
    }
  }
  unique(vars)
}
