# Essential tests for CRAN submission
# These tests are lightweight and focus on basic functionality

# Test data creation - simple and fast
create_test_data <- function(n = 20) {
  set.seed(123)
  data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:4, length.out = n)),
    sre = factor(rep(1:4, length.out = n))
  )
}

create_binomial_data <- function(n = 20) {
  set.seed(123)
  data.frame(
    y = rbinom(n, size = 10, prob = 0.5),
    n = rep(10, n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:4, length.out = n))
  )
}

create_beta_data <- function(n = 20) {
  set.seed(123)
  data.frame(
    y = rbeta(n, 2, 3),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:4, length.out = n))
  )
}

# Test 1: Basic hbm function
test_that("hbm basic functionality", {
  data <- create_test_data()
  
  # Test basic formula validation
  expect_error(hbm(123, data = data))
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x4), data = data))
  expect_error(hbm(brms::bf(z ~ x1 + x2), data = data))
  
  # Test prior validation
  expect_error(hbm(brms::bf(y ~ x1 + x2), data = data, prior = "invalid_prior"))
  expect_error(hbm(brms::bf(y ~ x1 + x2), data = data, prior = 123))
})

# Test 2: hbm_binlogitnorm basic functionality
test_that("hbm_binlogitnorm basic functionality", {
  data <- create_binomial_data()
  
  # Test variable existence
  expect_error(hbm_binlogitnorm(response = "z_dir", trials = "n", 
                                predictors = c("x1", "x2"), data = data))
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2", "x4"), data = data))
  expect_error(hbm_binlogitnorm(response = "y", trials = "m_i", 
                                predictors = c("x1", "x2"), data = data))
  
  # Test response validation
  data_wrong1 <- data
  data_wrong1$y[1] <- -1
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), data = data_wrong1))
  
  data_wrong2 <- data
  data_wrong2$y[1] <- 101
  data_wrong2$n[1] <- 100
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), data = data_wrong2))
  
  # Test prior validation
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), data = data, 
                                prior = "invalid"))
})

# Test 3: hbm_betalogitnorm basic functionality
test_that("hbm_betalogitnorm basic functionality", {
  data <- create_beta_data()
  
  # Test variable existence
  expect_error(hbm_betalogitnorm(response = "invalid", 
                                 predictors = c("x1", "x2"), data = data))
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("invalid1", "invalid2"), data = data))
  
  # Test response validation
  data_wrong1 <- data
  data_wrong1$y[1] <- 2
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("x1", "x2"), data = data_wrong1))
  
  # Test prior validation
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("x1", "x2"), data = data, 
                                 prior = "invalid"))
})

# Test 4: hbm_lnln basic functionality
test_that("hbm_lnln basic functionality", {
  data <- create_test_data()
  data$y_obs <- exp(data$y)  # Make positive for log-normal
  
  # Test variable existence
  expect_error(hbm_lnln(response = "invalid_var", 
                        predictors = c("x1", "x2"), data = data))
  expect_error(hbm_lnln(response = "y_obs", 
                        predictors = c("invalid", "x2"), data = data))
  
  # Test response validation
  data_invalid1 <- data
  data_invalid1$y_obs[1] <- -1
  expect_error(hbm_lnln(response = "y_obs", 
                        predictors = c("x1", "x2"), data = data_invalid1))
  
  data_invalid2 <- data
  data_invalid2$y_obs[1] <- 0
  expect_error(hbm_lnln(response = "y_obs", 
                        predictors = c("x1", "x2"), data = data_invalid2))
  
  # Test prior validation
  expect_error(hbm_lnln(response = "y_obs", 
                        predictors = c("x1", "x2"), data = data, 
                        prior = "invalid_prior"))
})

# Test 5: Utility functions basic functionality
test_that("hbcc basic functionality", {
  # Test with invalid model input
  expect_error(hbcc(model = "not_a_model"))
  expect_error(hbcc(model = NULL))
})

test_that("hbmc basic functionality", {
  # Test with empty model list
  expect_error(hbmc(model = list()))
  
  # Test with non-model input
  expect_error(hbmc(model = list("not_a_model")))
})

test_that("hbsae basic functionality", {
  # Test with invalid model input
  expect_error(hbsae("invalid"))
})

test_that("hbpc basic functionality", {
  # Create dummy data for testing
  sample_data <- create_test_data()
  
  # Test with non-brms/hbmfit model
  expect_error(hbpc(model = lm(y ~ x1, data = sample_data), 
                    data = sample_data, response_var = "y"))
})

test_that("update_hbm basic functionality", {
  # Test with wrong model type
  fit_wrong <- "wrong"
  expect_error(update_hbm(fit_wrong, iter = 1000))
})

test_that("run_sae_app basic functionality", {
  # Check that the function is available in the package
  expect_true("run_sae_app" %in% ls("package:hbsaems"))
})

# Test 6: Missing data handling validation
test_that("Missing data parameter validation", {
  data <- create_test_data()
  data_miss <- data
  data_miss$y[1] <- NA
  data_miss$x1[2] <- NA
  
  # Test error when handle_missing not specified with missing data
  expect_error(hbm(brms::bf(y ~ x1 + x2), data = data_miss))
  
  # For binomial data
  data_binom <- create_binomial_data()
  data_binom_miss <- data_binom
  data_binom_miss$y[1] <- NA
  data_binom_miss$x1[2] <- NA
  
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), 
                                data = data_binom_miss, 
                                handle_missing = "deleted"))
})

# Test 7: Spatial effects parameter validation
test_that("Spatial effects parameter validation", {
  data <- create_test_data()
  
  # Test invalid spatial effect type
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), 
                                sre = "sre", sre_type = "invalid", 
                                data = create_binomial_data()))
  
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("x1", "x2"), 
                                 sre = "sre", sre_type = "invalid", 
                                 data = create_beta_data()))
  
  expect_error(hbm_lnln(response = "y_obs", 
                        predictors = c("x1", "x2"), 
                        sre = "sre", sre_type = "invalid", 
                        data = create_test_data()))
})

# Test 8: Basic adjacency matrix validation
test_that("Adjacency matrix basic validation", {
  data <- create_binomial_data()
  
  # Non-symmetric matrix
  adjacency_matrix_wrong <- matrix(c(0, 1, 1, 1, 0, 0), nrow = 2, byrow = TRUE)
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), 
                                group = "group", sre = "group", 
                                sre_type = "car", car_type = "icar",
                                M = adjacency_matrix_wrong, data = data))
})

# Test 9: Basic data type validation
test_that("Basic data type validation", {
  # Test with non-data.frame input
  expect_error(hbm(brms::bf(y ~ x1), data = "not_a_dataframe"))
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1"), data = "not_a_dataframe"))
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("x1"), data = "not_a_dataframe"))
  expect_error(hbm_lnln(response = "y", 
                        predictors = c("x1"), data = "not_a_dataframe"))
})

# Test 10: Edge cases for trials validation (binomial)
test_that("Trials validation edge cases", {
  data <- create_binomial_data()
  
  # Test with negative trials
  data_wrong3 <- data
  data_wrong3$n[1] <- -1
  data_wrong3$y[1] <- 0
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), data = data_wrong3))
  
  # Test with NA in trials
  data_wrong_na <- data
  data_wrong_na$n[1] <- NA
  expect_error(hbm_binlogitnorm(response = "y", trials = "n", 
                                predictors = c("x1", "x2"), data = data_wrong_na))
})