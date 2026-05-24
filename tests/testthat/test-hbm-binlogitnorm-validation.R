# tests/testthat/test-hbm-binlogitnorm-validation.R
# =============================================================================
# Validation-only tests migrated from dev-tests/test-hbm-binlogitnorm.R.
# These tests exercise input validation and error paths in hbm-binlogitnorm
# wrappers WITHOUT triggering a Stan compile.  CRAN-safe.
#
# Sibling integration tests that DO require Stan remain in
# tests/testthat/dev-tests/test-hbm-binlogitnorm.R.
# =============================================================================

# Datasets used by the migrated tests.  Loaded at test-file scope so
# the tests can reference them without depending on helper-dev-setup.R.
utils::data("adjacency_matrix_car", package = "hbsaems", envir = environment())
utils::data("data_binlogitnorm", package = "hbsaems", envir = environment())
data <- data_binlogitnorm
adjacency_matrix <- adjacency_matrix_car
test_that("Function throws error for invalid prior", {
  expect_error(
    hbm_binlogitnorm(response = "y", 
                     trials = "n", 
                     auxiliary = c("x1", "x2", "x3"),
                     data = data,
                     prior = "invalid"),
    "Argument 'prior' must be a 'brmsprior' object."
  )
})

test_that("Function do not support handles missing data with 'model' method in binlogitnorm model", {
  #Missing at response and predictor
  data_miss_binom2 <- data
  data_miss_binom2$y[1] <- NA
  data_miss_binom2$x1[3] <- NA
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                  trials = "n", 
                                  auxiliary = c("x1", "x2", "x3"),
                                  area_var = "group",
                                  data = data_miss_binom2,
                                  handle_missing = "model"))
})
