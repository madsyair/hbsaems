# R/check-shiny-deps.R
# =============================================================================
# Internal: classify and check dependencies of the Shiny app.
#
# The Shiny app uses several packages that live in `Suggests` (so they don't
# bloat installation for users who only need the modelling functions).  This
# helper distinguishes between:
#
#   - CRITICAL packages : without them the app cannot start at all.
#                         Missing -> error.
#   - OPTIONAL packages : without them some panels degrade gracefully.
#                         Missing -> warning, listed in app banner.
#
# The classification is exported as a single named-list so that
# `run_sae_app()` and any future agent (e.g. shinytest2 fixtures) can
# consult it consistently.
# =============================================================================


# Single source of truth: which Suggests packages does the app touch, and
# what does each one enable?
.shiny_app_deps <- function() {
  list(
    critical = list(
      shinydashboard = "Dashboard layout (every UI tab)",
      DT             = "Data tables (preview, predictions, benchmarking)"
    ),
    optional = list(
      shinyWidgets   = paste0(
        "Picker inputs in Data Exploration and Diagnostics tabs ",
        "(falls back to selectInput)"),
      readxl         = paste0(
        "Reading .xls / .xlsx uploads (falls back to .csv only)"),
      energy         = paste0(
        "Distance correlation in scatter plot (falls back to Pearson / ",
        "Spearman)"),
      minerva        = paste0(
        "MIC correlation in scatter plot (falls back to Pearson / Spearman)"),
      sf             = paste0(
        "Build spatial weight matrix from shapefile (falls back to ",
        "manual matrix upload)"),
      spdep          = paste0(
        "Neighbour computation for spatial weights (paired with sf)"),
      bridgesampling = paste0(
        "Bayes factor in model_compare (LOO and WAIC remain available)")
    )
  )
}


#' Check Shiny App Dependencies
#'
#' Classifies and inspects the optional packages used by the Shiny dashboard.
#' The dashboard is launched by \code{\link{run_sae_app}} and the dependencies
#' it touches live in \code{Suggests}, not \code{Imports}, so users who only
#' use the modelling functions never have to install them.
#'
#' @param verbose Logical.  When \code{TRUE} (default), prints a formatted
#'   summary of which dependencies are installed and which are missing.
#'
#' @return Invisibly, a named list with components:
#'   \describe{
#'     \item{\code{critical_missing}}{Character vector of critical packages
#'       not installed.  When non-empty, the app cannot start.}
#'     \item{\code{optional_missing}}{Character vector of optional packages
#'       not installed.  When non-empty, the app starts but some panels
#'       degrade.}
#'     \item{\code{install_cmd}}{A ready-to-paste \code{install.packages()}
#'       call covering all missing packages, or \code{NULL} when none are
#'       missing.}
#'   }
#'
#' @examples
#' check_shiny_deps()
#'
#' @seealso \code{\link{run_sae_app}}
#' @export
check_shiny_deps <- function(verbose = TRUE) {

  deps <- .shiny_app_deps()

  is_installed <- function(p) requireNamespace(p, quietly = TRUE)

  crit_status <- vapply(names(deps$critical), is_installed, logical(1L))
  opt_status  <- vapply(names(deps$optional), is_installed, logical(1L))

  crit_missing <- names(deps$critical)[!crit_status]
  opt_missing  <- names(deps$optional)[!opt_status]

  install_cmd <- if (length(crit_missing) + length(opt_missing) > 0L) {
    sprintf(
      'install.packages(c(%s))',
      paste(sprintf('"%s"', c(crit_missing, opt_missing)),
            collapse = ", ")
    )
  } else NULL

  if (verbose) {
    cat("\nhbsaems Shiny App Dependency Check\n")
    cat("====================================\n\n")

    cat("CRITICAL (required to launch the app):\n")
    for (p in names(deps$critical)) {
      ok <- is_installed(p)
      cat(sprintf("  [%s] %-15s -- %s\n",
                  if (ok) "OK " else "X  ",
                  p, deps$critical[[p]]))
    }

    cat("\nOPTIONAL (used by individual panels):\n")
    for (p in names(deps$optional)) {
      ok <- is_installed(p)
      cat(sprintf("  [%s] %-15s -- %s\n",
                  if (ok) "OK " else "x  ",
                  p, deps$optional[[p]]))
    }

    if (length(crit_missing) > 0L) {
      cat("\n!! Critical packages missing -- app will NOT start.\n")
      cat("   Run:\n   ", install_cmd, "\n\n", sep = "")
    } else if (length(opt_missing) > 0L) {
      cat("\n!  Some optional features will be disabled.\n")
      cat("   Run:\n   ", install_cmd, "\n\n", sep = "")
    } else {
      cat("\n   All dependencies installed.  Ready to launch.\n\n")
    }
  }

  out <- list(
    critical_missing = crit_missing,
    optional_missing = opt_missing,
    install_cmd      = install_cmd
  )
  class(out) <- c("hbsaems_shiny_check", "hbsaems_check")
  invisible(out)
}


#' @method print hbsaems_shiny_check
#' @export
print.hbsaems_shiny_check <- function(x, ...) {
  # Re-run the verbose printer to give a fresh view; cheaper than caching.
  check_shiny_deps(verbose = TRUE)
  invisible(x)
}
