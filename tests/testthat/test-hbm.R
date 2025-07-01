# Load data
data <- data_fhnorm
adjacency_matrix <- adjacency_matrix_car
spatial_weight <- spatial_weight_sar

# Expected result
test_that("Function returns a model object", {
  skip_on_cran()
  
  model_hbm <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2 + x3), 
                                    data = data))
  expect_s3_class(model_hbm, "hbmfit")
  expect_named(model_hbm, c("model", "handle_missing", "data"), ignore.order = TRUE)
})

# Test the formula
test_that("Function throws an error when formula is not suitble", {
  skip_on_cran()
  
  expect_error(hbm(123, 
                   data = data))
  
  expect_error(hbm(brms::bf(y ~ x1 + x2 + x4),  
                   data = data),
               "Missing variables in data:.*x4")
  
  expect_error(hbm(brms::bf(z ~ x1 + x2 + x3), 
                   data = data),
               "Missing variables in data:.*z")
})

# Test the prior argument
test_that("Function throws an error when prior is invalid", {
  skip_on_cran()
  
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
  invalid_prior <- brms::prior(normal(0, 1), class = "b", coef = "x4")  # 'x4' tidak ada
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

# Test the re formula 
test_that("Function throws an error when re formula is not ~(1|group)", {
  skip_on_cran()
  
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

# Test the handle missing
test_that("Function throws an error when handle missing does not fit with the condition", {
  skip_on_cran()
  
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

suppressWarnings(library(mice))
test_that("Function runs correctly with valid handle_missing strategies", {
  skip_on_cran()
  
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

# Expected result
test_that("Function returns a model object for logit normal missing data and handle with 'deleted' option", {
  skip_on_cran()
  
  data_miss <- data_binlogitnorm
  data_miss$y[1] <- NA
  
  model_logit <- suppressWarnings(hbm(brms::bf(y | trials(n) ~ x1 + x2),
                                      hb_sampling = "binomial",
                                      hb_link = "logit",
                                      data = data_miss,
                                      handle_missing = "multiple"))
  expect_s3_class(model_logit, "hbmfit")
})

# Validate spatial effect
test_that("Function supports spatial random effects", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm(brms::bf(y ~ x1 + x2),
                       sre = "sre",
                       sre_type = "car",
                       M = adjacency_matrix,
                       data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
  
  suppressWarnings({
    model_sar <- hbm(brms::bf(y ~ x1 + x2),
                       sre_type = "sar",
                       M = spatial_weight,
                       data = data)
    expect_s3_class(model_sar, "hbmfit")
  })
  
  suppressWarnings({
    model_sar2 <- hbm(brms::bf(y ~ x1 + x2),
                     sre_type = "sar",
                     sar_type = "lag",
                     M = spatial_weight,
                     data = data)
    expect_s3_class(model_sar2, "hbmfit")
  })
})

test_that("Function throws error for invalid spatial random effect", {
  skip_on_cran()
  
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   sre = "invalid",
                   sre_type = "car",
                   M = adjacency_matrix,
                   data = data),
               "Variable 'invalid' not found in the data.")
})


test_that("Function supports spatial random effects without specified parameter", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm(brms::bf(y ~ x1 + x2),
                       sre = "sre",
                       M = adjacency_matrix,
                       data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function throws error for invalid car type", {
  skip_on_cran()
  
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   sre = "sre",
                   sre_type = "car",
                   car_type = "invalid",
                   M = adjacency_matrix,
                   data = data),
               "'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'")
})


test_that("Function supports spatial random effects with missing value in sre", {
  skip_on_cran()
  
  data_missing_sre <- data
  data_missing_sre$sre[1] <- NA
  
  model <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                sre = "sre",
                                sre_type = "car",
                                M = adjacency_matrix,
                                data = data_missing_sre))
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect type", {
  skip_on_cran()
  
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   sre = "sre",
                   sre_type = "invalid",
                   M = adjacency_matrix,
                   data = data),
               "Invalid spatial effect type. Use 'car' or 'sar'.")
})

test_that("Function throws error for spatial random effect type = sar", {
  skip_on_cran()
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   hb_sampling = "Beta",
                   sre = "sre",
                   sre_type = "sar",
                   M = adjacency_matrix,
                   data = data),
               "Currently, only families gaussian and student support SAR structures.")
})


# adj matrix
test_that("Function throws error when adjacency matrix is incorrect", {
  skip_on_cran()
  
  adjacency_matrix_wrong <- matrix(c(
    0, 1, 1,
    1, 0, 0
  ), nrow = 2, byrow = TRUE)
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   sre = "sre",
                   sre_type = "car",
                   car_type = "icar",
                   M = adjacency_matrix_wrong,
                   data = data),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,3] <- 1
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    sre = "sre",
                                    sre_type = "car",
                                    M = adjacency_matrix_wrong2,
                                    data = data)))
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1,3] <- 2
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    sre = "sre",
                                    sre_type = "car",
                                    M = adjacency_matrix_wrong3,
                                    data = data)))
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-3, -3]
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    sre = "sre",
                                    sre_type = "car",
                                    M = adjacency_matrix_wrong4,
                                    data = data)
  ))
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- as.character(6:10)
  
  expect_error(hbm(brms::bf(y ~ x1 + x2),
                   sre = "sre",
                   sre_type = "car",
                   M = adjacency_matrix_wrong5,
                   data = data
  ),
  "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  expect_error(suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                    sre_type = "car",
                                    M = adjacency_matrix_wrong4,
                                    data = data)
  ))
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  skip_on_cran()
  
  data_adj_dim <- data
  data_adj_dim$sre <- rep(1:4)
  
  model <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2),
                                sre = "sre",
                                sre_type = "car",
                                car_type = "icar",
                                M = adjacency_matrix,
                                data = data_adj_dim))
  expect_s3_class(model, "hbmfit")
})

test_that("Function to check error in SAR model", {
  skip_on_cran()
  expect_error(suppressWarnings(
    hbm(
      formula = bf(y ~ x1 + x2 + x3),  
      sre_type = "sar",
      sar_type = "invalid",
      M = spatial_weight_sar,    
      data = data)   
  ),
  "'sar_type' should be one of 'lag', 'error'"
  )
  
  expect_error(suppressWarnings(
    hbm(
      formula = bf(y ~ x1 + x2 + x3),  
      sre_type = "sar",
      sar_type = "lag",
      M = adjacency_matrix,    
      data = data)  
    ))

})


