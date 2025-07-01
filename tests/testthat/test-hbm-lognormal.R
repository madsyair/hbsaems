# Data Dummy For Lognormal
n <- 30
data_test <- data.frame(
  response = rlnorm(n, meanlog = 2, sdlog = 0.5),
  predictor1 = abs(rnorm(n)) + 0.1,
  predictor2 = abs(rnorm(n)) + 0.1,
  group = factor(1:n)
)

# Dummy spatial weight matrix
adjacency_matrix <- matrix(0, n, n)
for (i in 1:(n - 1)) {
  adjacency_matrix[i, i + 1] <- 1
  adjacency_matrix[i + 1, i] <- 1
}

# Set row and column names to match the group levels
rownames(adjacency_matrix) <- levels(data_test$group)
colnames(adjacency_matrix) <- levels(data_test$group)

# Expected result
test_that("Function returns a model object", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test
    )
    expect_s3_class(model, "hbmfit")
    expect_named(model, c("model", "handle_missing", "data"), ignore.order = TRUE)
  })
})

# Variable not exist
test_that("Function stops when response variable not found", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "invalid_var",
      predictors = c("predictor1", "predictor2"),
      data = data_test
    ),
    "Response variable not found"
  )
})

test_that("Function stops when predictor variable not found", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("invalid", "predictor2"),
      data = data_test
    ),
    "One or more predictor variables not found"
  )
})

# Validate response variable
test_that("Function throws error when response has zero or negative values", {
  skip_on_cran()
  data_test_invalid1 <- data_test
  data_test_invalid1$response[1] <- -1 # Inject invalid response
  data_test_invalid2 <- data_test
  data_test_invalid2$response[1] <- 0 # Inject invalid response

  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test_invalid1
    ),
    "contains zero or negative values"
  )
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test_invalid2
    ),
    "contains zero or negative values"
  )
})

# Validate prior
test_that("Function accepts valid priors", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test,
      prior = c(
        set_prior("normal(0, 1)", class = "b"),
        set_prior("student_t(3, 0, 2)", class = "Intercept")
      )
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid prior", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test,
      prior = "invalid_prior"
    ),
    "Argument 'prior' must be a 'brmsprior' object."
  )
})

# Validate random effects
test_that("Function supports random effects", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      data = data_test
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid random effect variable", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "invalid_group",
      data = data_test
    ),
    "Variable 'invalid_group' not found in the data."
  )
})

# Validate handle missing
test_that("Function support handle missing data with 'model' method", {
  skip_on_cran()
  data_test_missing1 <- data_test
  data_test_missing1$response[1] <- NA
  data_test_missing1$predictor1[2] <- NA

  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test_missing1,
      handle_missing = "model"
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handle missing data without method argument", {
  skip_on_cran()
  data_test_missing1 <- data_test
  data_test_missing1$response[1] <- NA
  data_test_missing1$predictor1[2] <- NA
  
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test_missing1
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handle missing data with 'deleted' method", {
  skip_on_cran()
  data_test_missing2 <- data_test
  data_test_missing2$response[1] <- NA

  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test_missing2,
      handle_missing = "deleted"
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handle missing data with 'multiple' method", {
  skip_on_cran()
  data_test_missing1 <- data_test
  data_test_missing1$response[1] <- NA
  data_test_missing1$predictor1[2] <- NA

  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      data = data_test_missing1,
      handle_missing = "multiple"
    )
  })
  expect_s3_class(model, "hbmfit")
})



test_that("Function throws an error when handle missing does not fit with the condition", {
  skip_on_cran()
  data_test_missing1 <- data_test
  data_test_missing1$response[1] <- NA
  data_test_missing1$predictor1[2] <- NA

  # When response variable missing, handle missing is deleted. but there are missing in predictor
  expect_error(hbm_lognormal(
    response = "response",
    predictors = c("predictor1", "predictor2"),
    data = data_test_missing1,
    handle_missing = "deleted"
  ))
})

# Validate spatial effect
test_that("Function supports spatial random effects", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "car",
      M = adjacency_matrix,
      data = data_test
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid spatial random effect", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "invalid",
      sre_type = "car",
      car_type = "icar",
      M = adjacency_matrix,
      data = data_test
    ),
    "Variable 'invalid' not found in the data."
  )
})

test_that("Function supports spatial random effects without specified parameter", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      sre_type = "car",
      M = adjacency_matrix,
      data = data_test
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid car type", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      sre = "group",
      sre_type = "car",
      car_type = "invalid",
      M = adjacency_matrix,
      data = data_test
    ),
    "'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'"
  )
})


test_that("Function supports spatial random effects with missing value in sre", {
  skip_on_cran()
  data_test_missing1 <- data_test
  data_test_missing1$group[1] <- NA
  
  model <- suppressWarnings(hbm_lognormal(
    response = "response",
    predictors = c("predictor1", "predictor2"),
    sre = "group",
    sre_type = "car",
    M = adjacency_matrix,
    data = data_test
  ))
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect type", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "invalid",
      M = adjacency_matrix,
      data = data_test
    ),
    "Invalid spatial effect type. Use 'car' or 'sar'."
  )
})

test_that("Function throws error for spatial random effect type = sar", {
  skip_on_cran()
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "sar",
      M = adjacency_matrix,
      data = data_test
    ),
    "Currently, only families gaussian and student support SAR structures."
  )
})

# adj matrix
test_that("Function throws error when adjacency matrix is incorrect", {
  skip_on_cran()
  adjacency_matrix_wrong <- matrix(c(
    0, 1, 1,
    1, 0, 0
  ), nrow = 2, byrow = TRUE)
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "car",
      car_type = "icar",
      M = adjacency_matrix_wrong,
      data = data_test
    ),
    "'M' for CAR terms must be symmetric."
  )
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1, 4] <- 1
  
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "car",
      M = adjacency_matrix_wrong2,
      data = data_test
    ),
    "'M' for CAR terms must be symmetric."
  )
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1, 2] <- 0
  
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "car",
      M = adjacency_matrix_wrong3,
      data = data_test
    )
  )
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-30, -30]
  
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "car",
      M = adjacency_matrix_wrong4,
      data = data_test
    ),
    "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- 2:31
  
  expect_error(
    hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      re = "group",
      sre = "group",
      sre_type = "car",
      M = adjacency_matrix_wrong5,
      data = data_test
    ),
    "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  expect_error(
    suppressWarnings(
      hbm_lognormal(
        response = "response",
        predictors = c("predictor1", "predictor2"),
        re = "group",
        sre_type = "car",
        M = adjacency_matrix_wrong4,
        data = data_test
      )
    ),
    "Dimensions of 'M' for CAR terms must be equal to the number of observations.",
  )
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  skip_on_cran()
  data_test2 <- data.frame(
    response = rlnorm(n, meanlog = 2, sdlog = 0.5),
    predictor1 = abs(rnorm(n)) + 0.1,
    predictor2 = abs(rnorm(n)) + 0.1,
    group = rep(1:10, length.out = 30)
  )
  
  suppressWarnings({
    model <- hbm_lognormal(
      response = "response",
      predictors = c("predictor1", "predictor2"),
      sre = "group",
      sre_type = "car",
      M = adjacency_matrix,
      data = data_test2
    )
    expect_s3_class(model, "hbmfit")
  })
})
