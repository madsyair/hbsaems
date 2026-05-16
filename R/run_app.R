# R/run_app.R
# =============================================================================
# run_sae_app() -- Launch the HBSAE interactive Shiny application.
# =============================================================================

#' Launch the HBSAE Shiny Application
#'
#' @title run_sae_app: Interactive Small Area Estimation Application
#'
#' @description
#' Opens the interactive HBSAE application in the default web browser.  The
#' application provides a graphical interface for data upload, exploratory
#' analysis, model specification, fitting, and result visualisation,
#' covering all modelling functions in \pkg{hbsaems}.
#'
#' @details
#' The application is located in \code{inst/shiny/sae_app/app.R} within the
#' installed package.  It depends on several packages that are listed in
#' \code{Suggests} (rather than \code{Imports}) so they are not required for
#' users who only need the modelling functions.
#'
#' Two classes of dependencies are checked at launch:
#' \describe{
#'   \item{\strong{Critical}}{Packages without which the app cannot start
#'     (\pkg{shinydashboard}, \pkg{DT}).  Missing critical packages raise
#'     an error.}
#'   \item{\strong{Optional}}{Packages that enable individual panels and
#'     features (\pkg{shinyWidgets}, \pkg{readxl}, \pkg{energy},
#'     \pkg{minerva}, \pkg{sf}, \pkg{spdep}, \pkg{bridgesampling}).
#'     Missing optional packages produce a warning and an in-app banner;
#'     the corresponding feature degrades gracefully.}
#' }
#'
#' Use \code{\link{check_shiny_deps}()} to inspect dependency status without
#' launching the app.
#'
#' @param check_deps Logical.  Whether to verify dependencies before
#'   launching (default \code{TRUE}).
#'
#' @return Does not return a value; called for its side effect of launching
#'   a Shiny server in the current R session.
#'
#' @examples
#' if (interactive()) {
#'   run_sae_app()
#' }
#'
#' @seealso \code{\link{check_shiny_deps}}
#' @export
run_sae_app <- function(check_deps = TRUE) {

  if (check_deps) {
    dep <- check_shiny_deps(verbose = FALSE)

    # ---- 1. Critical packages: hard stop --------------------------------
    if (length(dep$critical_missing) > 0L) {
      stop(
        "Cannot launch the Shiny app: the following CRITICAL packages are ",
        "not installed:\n  ",
        paste(dep$critical_missing, collapse = ", "), "\n",
        "Install them with:\n  ", dep$install_cmd,
        call. = FALSE
      )
    }

    # ---- 2. Optional packages: warning ----------------------------------
    if (length(dep$optional_missing) > 0L) {
      warning(
        "Some optional packages are not installed -- a few panels will be ",
        "disabled or fall back to simpler alternatives:\n  ",
        paste(dep$optional_missing, collapse = ", "), "\n",
        "To enable all features, install with:\n  ", dep$install_cmd,
        call. = FALSE, immediate. = TRUE
      )
    }

    # ---- 3. Pass the missing-optional list to the running app -----------
    # The app reads this from a package-internal env so it can show an
    # in-UI banner.  Setting a global option keeps it process-local and
    # survives across reactive contexts.
    options(hbsaems.shiny_missing_optional = dep$optional_missing)
  }

  app_dir <- system.file("shiny/sae_app", package = "hbsaems")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "The Shiny app directory was not found inside the installed ",
      "hbsaems package. Please reinstall the package.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
