# Data Dummy For Test
set.seed(123)
sample_data <- data.frame(
  y = rpois(100, lambda = 10),
  x1 = rnorm(100),
  x2 = rbinom(100, 1, 0.5),
  area = rep(1:10, each = 10),
  M = sample(c(NA, 1:3), 100, replace = TRUE)
)

# Adjacency Matrix with row names
adj_matrix <- matrix(0, 10, 10)
adj_matrix[row(adj_matrix) == col(adj_matrix) + 1] <- 1
adj_matrix[row(adj_matrix) == col(adj_matrix) - 1] <- 1

# Set row and column names to match 'area'
rownames(adj_matrix) <- colnames(adj_matrix) <- as.character(1:10)

# Adjacency Matrix for SAR
adj_matrix_full <- matrix(0, 100, 100)
adj_matrix_full[row(adj_matrix_full) == col(adj_matrix_full) + 1] <- 1
adj_matrix_full[row(adj_matrix_full) == col(adj_matrix_full) - 1] <- 1

# Expected result
test_that("Function returns a model object", {
  model_hbm <- suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                    data = sample_data))
  expect_s3_class(model_hbm, "hbmfit")
  expect_named(model_hbm, c("model", "handle_missing", "data"), ignore.order = TRUE)
})

# Test the formula
test_that("Function throws an error when formula is not suitble", {
  expect_error(hbm(123, 
                   data = sample_data))
  
  expect_error(hbm(bf(y ~ x1 + x3), 
                   data = sample_data),
               "Missing variables in data:.*x3")
  
  expect_error(hbm(bf(z ~ x1 + x2), 
                   data = sample_data),
               "Missing variables in data:.*z")
})

# Test the prior argument
test_that("Function throws an error when prior is invalid", {
  # 1. Error when prior is not a brms::prior object
  expect_error(
    hbm(bf(y ~ x1 + x2), 
        data = sample_data,
        prior = "invalid_prior"),
    "Argument 'prior' must be a 'brmsprior' object."
  )
  
  expect_error(
    hbm(bf(y ~ x1 + x2), 
        data = sample_data,
        prior = 123),
    "Argument 'prior' must be a 'brmsprior' object."
  )
  
  # 2. Error when prior references non-existent parameters
  invalid_prior <- brms::prior(normal(0, 1), class = "b", coef = "x3")  # 'x3' tidak ada
  expect_error(
    hbm(bf(y ~ x1 + x2), 
        data = sample_data,
        prior = invalid_prior)
  )
  
  # 3. Valid case (no error expected)
  valid_prior <- c(
    set_prior("normal(0, 1)", class = "b"),            # Prior untuk koefisien regresi (fixed effects)
    set_prior("normal(0, 1)", class = "Intercept")    # Prior untuk intercept
  )
  expect_s3_class(
    suppressWarnings(hbm(bf(y ~ x1 + x2), data = sample_data, prior = valid_prior)),
    "hbmfit"
  )
})

# Test the re formula 
test_that("Function throws an error when re formula is not ~(1|area)", {
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   re=area))
  
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   re="area"))
  
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   re="~(1|area)"))
  
  # Test valid case
  expect_s3_class(suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                       re = ~(1|area),
                                       data = sample_data)),
                  "hbmfit")
})

# Test the sre formula 
test_that("Function throws an error when sre formula is not suitable", {
  # sre
  # Test to check the presence of 
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre="area1", sre_type = "CAR", 
                   M = adj_matrix))
  
  # Test for sre not specified
  expect_s3_class(suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                       data = sample_data, 
                                       sre_type = "car", 
                                       M = adj_matrix_full)),
                  "hbmfit")
  
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data3, 
                   sre_type = "car", 
                   M = adj_matrix))
  
  # Test for missing value in sre and not specified car_type
  sample_data_miss <- data.frame(
    y = rpois(100, lambda = 10),
    x1 = rnorm(100),
    x2 = rbinom(100, 1, 0.5),
    area = rep(1:10, each = 10),
    M = sample(c(NA, 1:3), 100, replace = TRUE)
  )
  sample_data_miss$area[1] <- NA
  expect_s3_class(suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                       data = sample_data_miss, 
                                       sre = "area",
                                       sre_type = "car", 
                                       M = adj_matrix)),
                  "hbmfit")
  
  # sre_type
  # Test for invalid sre_type 
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre="area", sre_type = "CAR", 
                   M = adj_matrix))
  
  # Test when hb_sampling is not gaussian or student
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   hb_sampling = "Beta",
                   sre_type = "sar", 
                   sar_type = "lag",
                   M = adj_matrix_full),
               "Currently, only families gaussian and student support SAR structures.")
  
  # sar without sar_type specified
  expect_s3_class(suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                       data = sample_data, 
                                       sre_type = "sar", 
                                       M = adj_matrix_full)),
                  "hbmfit")
  
  # sar with sar_type
  expect_s3_class(suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                       data = sample_data, 
                                       sre_type = "sar", 
                                       sar_type = "lag",
                                       M = adj_matrix_full)),
                  "hbmfit")
  
  # Test for car type
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre = "area",
                   sre_type = "car", car_type = " icar ",
                   M = adj_matrix))
  
  # Test for sar type
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre_type = "sar", sar_type = "invalid",
                   M = adj_matrix))
  
  # Test when M does not exist
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre="area", 
                   sre_type = "car"))
  
  # Test when adjacency matrix is incorrect
  # Not symmetric
  adj_matrix_wrong <- adj_matrix
  adj_matrix_wrong[1, 2] <- 0  # Break symmetry
  adj_matrix_wrong[1, 3] <- 1
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre = "area", 
                   sre_type = "car", 
                   M = adj_matrix_wrong),
               "'M' for CAR terms must be symmetric.")
  
  adj_matrix_wrong2 <- adj_matrix
  adj_matrix_wrong2 <- adj_matrix_wrong2[-1, ]
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre = "area", 
                   sre_type = "car", 
                   M = adj_matrix_wrong2),
               "'M' for CAR terms must be symmetric.")
  
  adj_matrix_wrong3 <- adj_matrix
  adj_matrix_wrong3[1, 2] <- 0  # Break symmetry
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data, 
                   sre = "area", 
                   sre_type = "car", 
                   M = adj_matrix_wrong3))
  
  # The dimension of M is larger than the number of unique locations in sre
  sample_data2 <- data.frame(
    y = rpois(100, lambda = 10),
    x1 = rnorm(100),
    x2 = rbinom(100, 1, 0.5),
    area = rep(1:8, length.out = 100),
    M = sample(c(NA, 1:3), 100, replace = TRUE)
  )
  expect_s3_class(suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                       data = sample_data2, 
                                       sre = "area", 
                                       sre_type = "car", 
                                       car_type = "icar",
                                       M = adj_matrix)),
                  "hbmfit")
  
  # The dimension of M is smaller than the number of unique locations in sre
  sample_data3 <- data.frame(
    y = rpois(100, lambda = 10),
    x1 = rnorm(100),
    x2 = rbinom(100, 1, 0.5),
    area = rep(1:15, length.out = 100),
    M = sample(c(NA, 1:3), 100, replace = TRUE)
  )
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data3, 
                   sre = "area", 
                   sre_type = "car", 
                   M = adj_matrix))
  
  # Row names of 'M' for CAR terms do not match the names of the grouping levels
  adj_matrix_wrong4 <- adj_matrix
  rownames(adj_matrix_wrong4) <- colnames(adj_matrix_wrong4) <- as.character(2:11)
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = sample_data3, 
                   sre = "area", 
                   sre_type = "car", 
                   M = adj_matrix_wrong4))
  
})

# Test the handle missing
test_that("Function throws an error when handle missing does not fit with the condition", {
  # Data Dummy For Misisng Data
  # Missing at response
  data_miss1 <- sample_data
  data_miss1$y[1] <- NA
  
  #Missing at response and predictor
  data_miss2 <- sample_data
  data_miss2$y[1] <- NA
  data_miss2$x1[2] <- NA
  
  # When data contain missing and handle missing not set
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = data_miss1))
  
  # When response variable is missing and handle missing is model but the formula is wrong
  expect_error(hbm(bf(y ~ x1 + x2), 
                   data = data_miss1))
  
  # When predictor variable is missing and handle missing is model but the formula is wrong  
  expect_error(hbm(bf(y |mi()~ x1 + x2), 
                   data = data_miss2, 
                   handle_missing = "model"))
  
  # When using "model" for missing handling but y is discrete (e.g., binomial)
  data_bin <- sample_data
  data_bin$y <- rbinom(nrow(sample_data), size = 1, prob = 0.5)
  data_bin$y[1] <- NA
  
  expect_error(hbm(bf(y | mi() ~ x1 + x2),
                   data = data_bin,
                   hb_sampling = "binomial",
                   hb_link = "logit",
                   handle_missing = "model"))
  
  # When predictor variable missing handle missing is deleted but there are missing in predictor
  expect_error(hbm(bf(y |~ x1 + x2), 
                   data = data_miss2, 
                   handle_missing = "deleted"))
})

test_that("Function runs correctly with valid handle_missing strategies", {
  # Deleted: only response is missing
  data_miss1 <- sample_data
  data_miss1$y[1] <- NA
  
  fit_deleted <- suppressWarnings(hbm(bf(y ~ x1 + x2), 
                                      data = data_miss1, 
                                      handle_missing = "deleted"))
  expect_s3_class(fit_deleted, "hbmfit")
  
  # Model: response and predictor is missing, and formula includes mi()
  data_miss2 <- sample_data
  data_miss2$x1[2] <- NA
  
  fit_model <- suppressWarnings(hbm(bf(y|mi() ~ mi(x1) + x2) + bf(x1|mi() ~ x2) , 
                                    data = data_miss2, 
                                    handle_missing = "model"))
  expect_s3_class(fit_model, "hbmfit")
  
  # Multiple: any missing, and method handles multiple imputation internally
  data_miss3 <- sample_data
  data_miss3$y[1] <- NA
  data_miss3$x2[3] <- NA
  
  fit_multiple <- suppressWarnings(hbm(bf(y ~ x1 + x2),
                                       data = data_miss3,
                                       handle_missing = "multiple",
                                       m = 2))  # use fewer imputations to keep test fast
  expect_s3_class(fit_multiple, "hbmfit")
})