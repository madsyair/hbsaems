# tests/testthat/test-hbcc-validation.R
# =============================================================================
# Validation-only tests migrated from dev-tests/test-hbcc.R.
# These tests exercise input validation and error paths in hbcc
# wrappers WITHOUT triggering a Stan compile.  CRAN-safe.
#
# Sibling integration tests that DO require Stan remain in
# tests/testthat/dev-tests/test-hbcc.R.
# =============================================================================

test_that("convergence_check rejects invalid model input", {
  expect_error(convergence_check(model = "not_a_model"))
  expect_error(convergence_check(model = NULL))
})
