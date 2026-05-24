# tests/testthat/test-hbm-lnln-validation.R
# =============================================================================
# Validation-only tests migrated from dev-tests/test-hbm-lnln.R.
# These tests exercise input validation and error paths in hbm-lnln
# wrappers WITHOUT triggering a Stan compile.  CRAN-safe.
#
# Sibling integration tests that DO require Stan remain in
# tests/testthat/dev-tests/test-hbm-lnln.R.
# =============================================================================

# Datasets used by the migrated tests.  Loaded at test-file scope so
# the tests can reference them without depending on helper-dev-setup.R.
utils::data("adjacency_matrix_car", package = "hbsaems", envir = environment())
utils::data("data_lnln", package = "hbsaems", envir = environment())
data <- data_lnln
adjacency_matrix <- adjacency_matrix_car
test_that("Function throws error for invalid prior", {
  expect_error(
    hbm_lnln(
      response = "y_obs",
      auxiliary = c("x1", "x2", "x3"),
      data = data,
      prior = "invalid_prior"
    ),
    "Argument 'prior' must be a 'brmsprior' object."
  )
})
