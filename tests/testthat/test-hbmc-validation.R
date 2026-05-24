# tests/testthat/test-hbmc-validation.R
# =============================================================================
# Validation-only tests migrated from dev-tests/test-hbmc.R.
# These tests exercise input validation and error paths in hbmc
# wrappers WITHOUT triggering a Stan compile.  CRAN-safe.
#
# Sibling integration tests that DO require Stan remain in
# tests/testthat/dev-tests/test-hbmc.R.
# =============================================================================

test_that("hbmc throws an error for non-model input", {
  # hbmc() is deprecated -- suppressWarnings to silence the deprecation
  # message; we are only testing the underlying error path.
  expect_error(
    suppressWarnings(hbmc(model = list("not_a_model")))
  )
})
