# tests/testthat/dev-tests/test-hbm.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

# Load data
data <- data_fhnorm
if (!"group" %in% names(data)) data$group <- data$regency  # 100 levels for ordinary RE
if (!"sre"   %in% names(data)) data$sre   <- data$province  # 5 levels -> matches adjacency_matrix_car
adjacency_matrix <- adjacency_matrix_car   # 5x5, rownames province_01..05
spatial_weight <- spatial_weight_sar

# Expected result
test_that("Function returns a model object", {
  .dev_skip()
  
  model_hbm <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2 + x3), 
                                    data = data))
  expect_s3_class(model_hbm, "hbmfit")
  expect_named(model_hbm, c("model", "handle_missing", "missing_method", "data"), ignore.order = TRUE)
})

test_that("Function throws an error when prior is invalid", {
  .dev_skip()
  
  # 1. Error when prior is not a brms::prior object
  expect_error(
    hbm(brms::bf(y ~ x1 + x2 + x3), 
        data = data,
        prior = "invalid_prior"),
    "Argument 'prior' must be a 'brmsprior' object."
  )
  
  expect_error(
    hbm(brms::bf(y ~ x1 + x2 + x3),  
        data = data,
        prior = 123),
    "Argument 'prior' must be a 'brmsprior' object."
  )
  
  # 2. Error when prior references non-existent parameters
  invalid_prior <- brms::prior("normal(0, 1)", class = "b", coef = "x4")  # 'x4' tidak ada
  expect_error(
    hbm(brms::bf(y ~ x1 + x2 + x3),
        data = data,
        prior = invalid_prior)
  )
  
  # 3. Valid case (no error expected)
  valid_prior <- c(
    brms::set_prior("normal(0, 1)", class = "b"),            # Prior untuk koefisien regresi (fixed effects)
    brms::set_prior("normal(0, 1)", class = "Intercept")    # Prior untuk intercept
  )
  expect_s3_class(
    suppressWarnings(hbm(brms::bf(y ~ x1 + x2 + x3), data = data, prior = valid_prior)),
    "hbmfit"
  )
})

test_that("Function throws an error when re formula is not ~(1|group)", {
  .dev_skip()
  
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x3), 
                   data = data, 
                   re=group))
  
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x3),
                   data = data, 
                   re="group"))
  
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x3), 
                   data = data, 
                   re="~(1|group)"))
  
  # Test valid case
  expect_s3_class(suppressWarnings(hbm(formula(y ~ x1 + x2 +x3), 
                                       re = ~(1|group),
                                       data = data)),
                  "hbmfit")
})

test_that("Function runs correctly with valid handle_missing strategies", {
  .dev_skip()
  
  # Deleted: only response is missing
  data_miss1 <- data
  data_miss1$y[1] <- NA
  
  data_miss2 <- data
  data_miss2$y[1] <- NA
  data_miss2$x1[2] <- NA
  
  expect_error(hbm(brms::bf(y ~ x1 + x2), data = data_miss2, handle_missing = "deleted"))
  
  fit_deleted <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2), 
                                      data = data_miss1, 
                                      handle_missing = "deleted"))
  expect_s3_class(fit_deleted, "hbmfit")
  
  # Model: response and predictor is missing, and formula includes mi()
  data_miss2 <- data
  data_miss2$x1[2] <- NA
  
  fit_model <- suppressWarnings(hbm(brms::bf(y|mi() ~ mi(x1) + x2) + brms::bf(x1|mi() ~ x2) , 
                                    data = data_miss2,  re= ~(1|group), 
                                    handle_missing = "model"))
  expect_s3_class(fit_model, "hbmfit")
  
  # Multiple: any missing, and method handles multiple imputation internally
  data_miss3 <- data
  data_miss3$y[1] <- NA
  data_miss3$x2[3] <- NA
  
  fit_multiple <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                       data = data_miss3,
                                       handle_missing = "multiple",
                                       m = 2))  # use fewer imputations to keep test fast
  expect_s3_class(fit_multiple, "hbmfit")
})

test_that("Function returns a model object for logit normal missing data and handle with 'deleted' option", {
  .dev_skip()
  
  data_miss <- data_binlogitnorm
  data_miss$y[1] <- NA
  
  model_logit <- suppressWarnings(hbm(brms::bf(y | trials(n) ~ x1 + x2),
                                      hb_sampling = "binomial",
                                      hb_link = "logit",
                                      data = data_miss,
                                      handle_missing = "multiple"))
  expect_s3_class(model_logit, "hbmfit")
})

test_that("Function supports spatial random effects", {
  .dev_skip()
  
  suppressWarnings({
    model_logit <- hbm(brms::bf(y ~ x1 + x2),
                       spatial_var = "sre",
                       spatial_model = "car",
                       M = adjacency_matrix,
                       data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
  
  suppressWarnings({
    model_sar <- hbm(brms::bf(y ~ x1 + x2),
                       spatial_var = "regency",
                       spatial_model = "sar",
                       M = spatial_weight,
                       data = data)
    expect_s3_class(model_sar, "hbmfit")
  })
  
  suppressWarnings({
    model_sar2 <- hbm(brms::bf(y ~ x1 + x2),
                     spatial_var = "regency",
                     spatial_model = "sar",
                     sar_type = "lag",
                     M = spatial_weight,
                     data = data)
    expect_s3_class(model_sar2, "hbmfit")
  })
})

test_that("Function supports spatial random effects without specified parameter", {
  .dev_skip()
  
  suppressWarnings({
    model_logit <- hbm(brms::bf(y ~ x1 + x2),
                       spatial_var = "sre",
                       spatial_model = "car",
                       M = adjacency_matrix,
                       data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function supports spatial random effects with missing value in sre", {
  .dev_skip()
  
  data_missing_sre <- data
  data_missing_sre$sre[1] <- NA
  
  model <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                spatial_var = "sre",
                                spatial_model = "car",
                                M = adjacency_matrix,
                                data = data_missing_sre))
  expect_s3_class(model, "hbmfit")
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  .dev_skip()
  
  # Edge case: matrix has MORE locations (5 provinces) than the grouping
  # variable uses (4 levels).  The 4 levels must still be valid row names of
  # M, so use a subset of the province labels rather than bare integers.
  data_adj_dim <- data
  data_adj_dim$sre <- factor(rep(sprintf("province_0%d", 1:4),
                                 length.out = nrow(data_adj_dim)),
                             levels = rownames(adjacency_matrix))

  model <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                spatial_var = "sre",
                                spatial_model = "car",
                                car_type = "icar",
                                M = adjacency_matrix,
                                data = data_adj_dim))
  expect_s3_class(model, "hbmfit")
})


# === Migrated back from main tests (ghost-variable dependence) ===
test_that("Function throws an error when handle missing does not fit with the condition", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  # Data Dummy For Misisng Data
  # Missing at response
  data_miss1 <- data
  data_miss1$y[1] <- NA
  
  #Missing at response and predictor
  data_miss2 <- data
  data_miss2$y[1] <- NA
  data_miss2$x1[2] <- NA
  
  # When data contain missing and handle missing not set
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x3), 
                   data = data_miss1))
  
  # When response variable is missing and handle missing is model but the formula is wrong
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x3), 
                   data = data_miss1))
  
  # When predictor variable is missing and handle missing is model but the formula is wrong  
  expect_error(hbm(brms::bf(y |mi()~ x1 + x2 + x3), 
                   data = data_miss2, 
                   handle_missing = "model"))
  
  # When using "model" for missing handling but y is discrete (e.g., binomial)
  data_bin <- data
  data_bin$y <- rbinom(nrow(data), size = 1, prob = 0.5)
  data_bin$y[1] <- NA
  
  expect_error(hbm(brms::bf(y | mi() ~ x1 + x2),
                   data = data_bin,
                   hb_sampling = "binomial",
                   hb_link = "logit",
                   handle_missing = "model"))
  
  # When predictor variable missing handle missing is deleted but there are missing in predictor
  expect_error(hbm(brms::bf(y |~ x1 + x2), 
                   data = data_miss2, 
                   handle_missing = "deleted"))
})


# === Re-migrated from main (require real fits) ===
test_that("Function throws an error when formula is not suitble", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(hbm(123, 
                   data = data))
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2 + x4),
                   data = data)))
  
  expect_error(suppressWarnings(hbm(brms::bf(z ~ x1 + x2 + x3), 
                   data = data)))
})

test_that("Function throws error for invalid spatial random effect", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   spatial_var = "invalid",
                   spatial_model = "car",
                   M = adjacency_matrix,
                   data = data))
})

test_that("Function throws error for invalid car type", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   spatial_var = "sre",
                   spatial_model = "car",
                   car_type = "invalid",
                   M = adjacency_matrix,
                   data = data),
               "car_type.*should be one of|car_type")
})

test_that("Function throws error for invalid spatial random effect type", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   spatial_var = "sre",
                   spatial_model = "invalid",
                   M = adjacency_matrix,
                   data = data))
})

test_that("Function throws error for spatial random effect type = sar", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   hb_sampling = "Beta",
                   spatial_var = "sre",
                   spatial_model = "sar",
                   M = adjacency_matrix,
                   data = data),
               "SAR|gaussian and student|only families")
})

test_that("Function throws error when adjacency matrix is incorrect", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  adjacency_matrix_wrong <- matrix(c(
    0, 1, 1,
    1, 0, 0
  ), nrow = 2, byrow = TRUE)
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   spatial_var = "sre",
                   spatial_model = "car",
                   car_type = "icar",
                   M = adjacency_matrix_wrong,
                   data = data))
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,3] <- 1
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    spatial_var = "sre",
                                    spatial_model = "car",
                                    M = adjacency_matrix_wrong2,
                                    data = data)))
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1,3] <- 2
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    spatial_var = "sre",
                                    spatial_model = "car",
                                    M = adjacency_matrix_wrong3,
                                    data = data)))
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-3, -3]
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    spatial_var = "sre",
                                    spatial_model = "car",
                                    M = adjacency_matrix_wrong4,
                                    data = data)
  ))
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- as.character(6:10)
  
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   spatial_var = "sre",
                   spatial_model = "car",
                   M = adjacency_matrix_wrong5,
                   data = data
  ),
  "do not match the levels")
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    spatial_model = "car",
                                    M = adjacency_matrix_wrong4,
                                    data = data)
  ))
})

test_that("Function to check error in SAR model", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(suppressWarnings(
    hbm(
      formula = bf(y ~ x1 + x2 + x3),  
      spatial_var = "regency",
      spatial_model = "sar",
      sar_type = "invalid",
      M = spatial_weight_sar,    
      data = data)   
  ))
  
  expect_error(suppressWarnings(
    hbm(
      formula = bf(y ~ x1 + x2 + x3),  
      spatial_var = "regency",
      spatial_model = "sar",
      sar_type = "lag",
      M = adjacency_matrix,    
      data = data)  
    ))

})
