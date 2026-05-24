# tests/testthat/dev-tests/test-hbcc.R
# Integration tests requiring real hbm() fits.

test_that("convergence_check runs and returns a list for a valid hbmfit", {
  .dev_skip()
  fit    <- .fit_for_hbcc()
  result <- suppressWarnings(convergence_check(fit))
  expect_type(result, "list")
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
