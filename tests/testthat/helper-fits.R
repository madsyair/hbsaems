# tests/testthat/helper-fits.R
# =============================================================================
# Shared, lazily-built model fixtures for the heavy integration tests.
#
# testthat sources every tests/testthat/helper-*.R file before running the
# suite, so the functions defined here are available to both the CRAN-safe
# tests and the dev-tests in tests/testthat/dev-tests/.  The fits themselves
# are only *built* when a dev-test actually calls the fixture (dev-tests gate
# on .dev_skip() first), so defining these helpers is free on CRAN.
#
# .fit_for_hbcc() is memoised: convergence_check has several integration
# tests that each need a real posterior, and compiling/​sampling once and
# reusing keeps the dev-test runtime down without weakening the checks.
# =============================================================================

# Environment used to cache the fitted model across test_that() blocks.
.hbsaems_fit_cache <- new.env(parent = emptyenv())

# A small, real hbmfit fit on the bundled data_fhnorm dataset, suitable for
# exercising convergence_check() / hbcc().  Uses the v1.0.0 area-variable
# name `regency` and an explicit area-level random effect, i.e. a standard
# Fay-Herriot specification (u_i ~ N(0, sigma_u^2) per area).
.fit_for_hbcc <- function() {
  if (is.null(.hbsaems_fit_cache$hbcc_fit)) {
    utils::data("data_fhnorm", package = "hbsaems", envir = environment())
    .hbsaems_fit_cache$hbcc_fit <- suppressWarnings(
      hbm(
        formula = brms::bf(y ~ x1 + x2 + x3),
        data    = data_fhnorm,
        re      = ~ (1 | regency),
        chains  = 2, iter = 1000, warmup = 500, cores = 1,
        seed    = 123, refresh = 0
      )
    )
  }
  .hbsaems_fit_cache$hbcc_fit
}
