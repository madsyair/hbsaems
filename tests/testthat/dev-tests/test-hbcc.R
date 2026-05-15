# tests/testthat/dev-tests/test-hbcc.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

# Fit a minimal model once for the entire file to amortise compilation cost.
.fit_for_hbcc <- function() {
  suppressWarnings(
    hbm(brms::bf(y ~ x1 + x2),
        data           = data_fhnorm,
        iter           = 100,
        chains         = 2,
        warmup         = 50,
        hb_sampling    = "gaussian",
        hb_link        = "identity",
        handle_missing = "deleted",
        refresh        = 0)
  )
}

test_that("convergence_check runs and returns a list for a valid hbmfit", {
  .dev_skip()
  fit    <- .fit_for_hbcc()
  result <- suppressWarnings(convergence_check(fit))
  expect_type(result, "list")
})

test_that("convergence_check rejects invalid model input", {
  .dev_skip()
  expect_error(convergence_check(model = "not_a_model"))
  expect_error(convergence_check(model = NULL))
})

test_that("convergence_check rejects unknown plot types", {
  .dev_skip()
  fit <- .fit_for_hbcc()
  expect_error(
    suppressWarnings(convergence_check(fit, plot_types = "invalid_plot"))
  )
})

test_that("convergence_check rejects unknown diagnostic tests", {
  .dev_skip()
  fit <- .fit_for_hbcc()
  expect_error(
    suppressWarnings(convergence_check(fit, diag_tests = "wrong_diag"))
  )
})

test_that("hbcc() (deprecated) forwards to convergence_check()", {
  .dev_skip()
  fit <- .fit_for_hbcc()
  # The .Deprecated() warning is expected; we capture-and-ignore it.
  result <- suppressWarnings(hbcc(fit))
  expect_type(result, "list")
})
