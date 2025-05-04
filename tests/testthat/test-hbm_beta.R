# Data Dummy For Beta Model
n <- 50
data <- data.frame(
  y = runif(n, 0.01, 0.99),
  x1 = rnorm(n),
  x2 = rnorm(n),
  n = sample(10:50, n, replace = TRUE),
  deff = runif(n, 1, 2),
  group = factor(1:50)
)

# Dummy spatial weight matrix
adjacency_matrix <- matrix(0, 50, 50)
for (i in 1:49) {
  adjacency_matrix[i, i + 1] <- 1
  adjacency_matrix[i + 1, i] <- 1
}
dimnames(adjacency_matrix) <- list(levels(data$group), levels(data$group))

# Expected result
test_that("Function returns a model object", {
  model_beta1 <- suppressWarnings(hbm_beta(response = "y", 
                                                  predictors = c("x1", "x2"), 
                                                  data = data))
  expect_s3_class(model_beta1, "hbmfit")
  expect_named(model_beta1, c("model", "handle_missing", "data"), ignore.order = TRUE)
})

test_that("Function returns a model object (with n & deff)", {
  model_beta2 <- suppressWarnings(hbm_beta(response = "y", 
                                                  predictors = c("x1", "x2"), 
                                                  n = "n",
                                                  deff = "deff",
                                                  data = data))
  expect_s3_class(model_beta2, "hbmfit")
  expect_named(model_beta2, c("model", "handle_missing", "data"), ignore.order = TRUE)
})

# Validate prior
test_that("Function accepts valid priors", {
  model_beta3 <- suppressWarnings(hbm_beta(response = "y", 
                                           predictors = c("x1", "x2"), 
                                           prior = c(set_prior("normal(0, 1)", class = "b")),
                                           data = data))
  expect_s3_class(model_beta3, "hbmfit")
})

# Invalid prior
test_that("Function throws error for invalid prior", {
  expect_error(hbm_beta(response = "y", 
                    predictors = c("x1", "x2"), 
                    data = data,
                    prior = "invalid"),
                    "Argument 'prior' must be a 'brmsprior' object."
  )
})

# Validate n and deff
test_that("Function throws error when n exists but deff does not", {
  expect_error(hbm_beta(response = "y", 
                         predictors = c("x1", "x2"), 
                         n = "n",
                         data = data,
                         "Both variables 'n' and 'deff' must be specified or undefined simultaneously."))
})

test_that("Function throws error when deff exists but n does not", {
  expect_error(hbm_beta(response = "y", 
                         predictors = c("x1", "x2"), 
                         deff = "deff",
                         data = data,
                         "Both variables 'n' and 'deff' must be specified or undefined simultaneously."))
})

# Variable not exixt
test_that("Function stops when response variable not found", {
  expect_error(hbm_beta(response = "invalid", 
                        predictors = c("x1", "x2"), 
                        n = "n",
                        deff = "deff",
                        data = data,
                        "Variable 'invalid' not found in the 'data'."))
})

test_that("Function stops when predictor variable not found", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("invalid1", "invalid2"), 
                        n = "n",
                        deff = "deff",
                        data = data,
                        "Variable 'invalid1, invalid2' not found in the 'data'."))
})

test_that("Function stops when n not found", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        n = "invalid",
                        deff = "deff",
                        data = data,
                        "Variable 'n' not found in the 'data'."))
})

test_that("Function stops when deff variable not found", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        n = "n",
                        deff = "invalid",
                        data = data,
                        "Variable 'invalid' not found in the 'data'."))
})

# Check for response values
test_that("Function stops when response value not between 0 and 1", {
  data_wrong1 <- data
  data_wrong1$y[1] <- 3
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        data = data_wrong1,
                        "Response variable must be between 0 and 1."))
})

# Check if n and deff are non-positive
test_that("Function stops when n and deff not positive", {
  data_wrong2 <- data
  data_wrong2$n[1] <- -3
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        n = "n",
                        deff = "deff",
                        data = data_wrong2,
                        "Both 'n' and 'deff' must be strictly positive values."))
})

# Validate random effects
test_that("Function supports random effects", {
  model_beta4 <- suppressWarnings(hbm_beta(response = "y", 
                                           predictors = c("x1", "x2"), 
                                           re = "group",
                                           data = data))
  expect_s3_class(model_beta4, "hbmfit")
})

test_that("Function throws error for invalid random effect variable", {
  expect_error(hbm_beta(response = "y", 
                         predictors = c("x1", "x2"), 
                         re = "invalid",
                         data = data),
                        "Variable 'invalid' not found in the data." )
})

# Validate spatial effect
test_that("Function supports spatial random effects", {
  model_beta5 <- suppressWarnings(hbm_beta(response = "y", 
                                           predictors = c("x1", "x2"), 
                                           sre = "group",
                                           sre_type = "car",
                                           car_type = "icar",
                                           M = adjacency_matrix,
                                           data = data))
  expect_s3_class(model_beta5, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        re = "group",
                        sre = "invalid",
                        sre_type = "car",
                        car_type = "icar",
                        M = adjacency_matrix,
                        data = data),
               "Variable 'invalid' not found in the data.")
})

test_that("Function supports spatial random effects withouh specified parameter", {
  model_beta6 <- suppressWarnings(hbm_beta(response = "y", 
                                           predictors = c("x1", "x2"), 
                                           sre_type = "car",
                                           M = adjacency_matrix,
                                           data = data))
  expect_s3_class(model_beta6, "hbmfit")
})

test_that("Function throws error for invalid car type", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        sre = "group",
                        sre_type = "car",
                        car_type = "invalid",
                        M = adjacency_matrix,
                        data = data),
               "'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'")
})

test_that("Function supports spatial random effects with missing value in sre", {
  data_missing5 <- data
  data_missing5$sre[1] <- NA
  
  model_beta7 <- suppressWarnings(hbm_beta(response = "y", 
                                           predictors = c("x1", "x2"), 
                                           sre = "group",
                                           sre_type = "car",
                                           M = adjacency_matrix,
                                           data = data))
  expect_s3_class(model_beta7, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect type", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        re = "group",
                        sre = "group",
                        sre_type = "invalid",
                        M = adjacency_matrix,
                        data = data),
               "Invalid spatial effect type. Use 'car' or 'sar'.")
})

test_that("Function throws error for spatial random effect type = sar", {
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        re = "group",
                        sre = "group",
                        sre_type = "sar",
                        M = adjacency_matrix,
                        data = data),
               "Currently, only families gaussian and student support SAR structures.")
})

# adj matrix
test_that("Function throws error when adjacency matrix is incorrect", {
  adjacency_matrix_wrong <- matrix(c(0, 1, 1,
                                     1, 0, 0), nrow = 2, byrow = TRUE)
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        sre = "group",
                        re = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong,
                        data = data),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,4] <- 1
  
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        sre = "group",
                        re = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong2,
                        data = data),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1,2] <- 0
  
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        sre = "group",
                        re = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong3,
                        data = data))
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-50, -50]
  
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        sre = "group",
                        re = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong4,
                        data = data),
               "Row names of 'M' for CAR terms do not match the names of the grouping levels.")
 
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- 2:51
  
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        sre = "group",
                        re = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong5,
                        data = data),
               "Row names of 'M' for CAR terms do not match the names of the grouping levels.")
  
  expect_error(suppressWarnings(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        re = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong4,
                        data = data)),
               "Dimensions of 'M' for CAR terms must be equal to the number of observations.")
  
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  data2 <- data.frame(
    y = runif(n, 0.01, 0.99),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = rep(1:10, length.out = 50)
  )
  
  model_beta8 <- suppressWarnings(hbm_beta(response = "y", 
                                           predictors = c("x1", "x2"), 
                                           sre = "group",
                                           sre_type = "car",
                                           M = adjacency_matrix,
                                           data = data2))
  expect_s3_class(model_beta8, "hbmfit")
})

#Validate handle missing
test_that("Function support handle missing data with 'model' method", {
  data_missing1 <- data
  data_missing1$y[1] <- NA
  data_missing1$x1[3] <- NA
  
  model_beta9 <- suppressWarnings(hbm_beta(response = "y",
                                           predictors = c("x1", "x2"),
                                           data = data_missing1,
                                           handle_missing = "model"))
  expect_s3_class(model_beta9, "hbmfit")
})

test_that("Function support handle missing data with 'deleted' method", {
  data_missing2 <- data
  data_missing2$y[1] <- NA
  
  model_beta10 <- suppressWarnings(hbm_beta(response = "y",
                                           predictors = c("x1", "x2"),
                                           data = data_missing2,
                                           handle_missing = "deleted"))
  expect_s3_class(model_beta10, "hbmfit")
})

test_that("Function support handle missing data with 'multiple' method", {
  data_missing3 <- data
  data_missing3$y[1] <- NA
  data_missing3$x1[2] <- NA
  
  model_beta11 <- suppressWarnings(hbm_beta(response = "y",
                                            predictors = c("x1", "x2"),
                                            data = data_missing3,
                                            handle_missing = "multiple"))
  expect_s3_class(model_beta11, "hbmfit")
})

test_that("Function stop when n missing", {
  data_missing3 <- data
  data_missing3$n[1] <- NA
  
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        n = "n",
                        deff = "deff",
                        data = data_missing3), 
               "Missing values detected in either 'n' or 'deff'.")
})

test_that("Function stop when deff missing", {
  data_missing4 <- data
  data_missing4$deff[1] <- NA
  
  expect_error(hbm_beta(response = "y", 
                        predictors = c("x1", "x2"), 
                        n = "n",
                        deff = "deff",
                        data = data_missing4), 
               "Missing values detected in either 'n' or 'deff'.")
})