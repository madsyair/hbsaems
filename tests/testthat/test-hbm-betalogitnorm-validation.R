# tests/testthat/test-hbm-betalogitnorm-validation.R
# =============================================================================
# Validation-only tests migrated from dev-tests/test-hbm-betalogitnorm.R.
# These tests exercise input validation and error paths in hbm-betalogitnorm
# wrappers WITHOUT triggering a Stan compile.  CRAN-safe.
#
# Sibling integration tests that DO require Stan remain in
# tests/testthat/dev-tests/test-hbm-betalogitnorm.R.
# =============================================================================

# Datasets used by the migrated tests.  Loaded at test-file scope so
# the tests can reference them without depending on helper-dev-setup.R.
utils::data("adjacency_matrix_car", package = "hbsaems", envir = environment())
utils::data("data_betalogitnorm", package = "hbsaems", envir = environment())
data <- data_betalogitnorm
adjacency_matrix <- adjacency_matrix_car
test_that("Function to check the existence of parameters", {
  expect_error(hbm_betalogitnorm(response = "invalid", 
                        auxiliary = c("x1", "x2", "x3"), 
                        data = data),
               "Response variable 'invalid' not found")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        auxiliary = c("invalid1", "invalid2"), 
                        n = "n",
                        deff = "deff",
                        data = data),
               "Predictor.*not found")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        auxiliary = c("x1", "x2", "x3"), 
                        n = "invalid",
                        deff = "deff",
                        data = data),
               "not.*column.*data|not found")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        auxiliary = c("x1", "x2", "x3"), 
                        n = "n",
                        deff = "invalid",
                        data = data),
               "not.*column.*data|not found")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2", "x3"), 
                                 area_var = "invalid",
                                 data = data),
               "not found")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2", "x3"), 
                                 spatial_var = "invalid",
                                 spatial_model = "car",
                                 car_type = "escar",
                                 M = adjacency_matrix,
                                 data = data),
               "not found")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        auxiliary = c("x1", "x2"), 
                        n = "n",
                        data = data),
               "Both.*n.*deff.*supplied together")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2", "x3"), 
                                 deff = "deff",
                                 data = data),
               "Both.*n.*deff.*supplied together")
})

test_that("Function to check response value", {
  data_wrong1 <- data
  data_wrong1$y[1] <- 2
  expect_error(hbm_betalogitnorm(response = "y", 
                        auxiliary = c("x1", "x2"), 
                        data = data_wrong1),
               "Response variable must lie strictly in")
})

test_that("Function stops when response variable not found", {
  data_wrong2 <- data
  data_wrong2$n[1] <- NA
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2"), 
                                 n = "n",
                                 deff = "deff",
                                 data = data_wrong2),
               "Missing values detected in")
  
  data_wrong3 <- data
  data_wrong3$deff[1] <- NA
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2"), 
                                 n = "n",
                                 deff = "deff",
                                 data = data_wrong3),
               "Missing values detected in")
  
  data_wrong4 <- data
  data_wrong4$n[1] <- -1
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2"), 
                                 n = "n",
                                 deff = "deff",
                                 data = data_wrong4),
               "strictly positive")
  
  data_wrong5 <- data
  data_wrong5$deff[1] <- -1
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2"), 
                                 n = "n",
                                 deff = "deff",
                                 data = data_wrong5),
               "strictly positive")
  
  data_wrong6 <- data
  data_wrong6$n[1] <- 1
  data_wrong6$deff[1] <- 2
  expect_error(hbm_betalogitnorm(response = "y", 
                                 auxiliary = c("x1", "x2"), 
                                 n = "n",
                                 deff = "deff",
                                 data = data_wrong6),
               "non-positive values")
  
})
