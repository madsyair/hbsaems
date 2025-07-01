# Data Dummy For Lognormal
data <- data_lnln
M <- adjacency_matrix_car

# Expected result
test_that("Function returns a model object", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data
    )
    expect_s3_class(model, "hbmfit")
    expect_named(model, c("model", "handle_missing", "data"), ignore.order = TRUE)
  })
})

# Variable not exist
test_that("Function stops when response variable not found", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "invalid_var",
      predictors = c("x1", "x2", "x3"),
      data = data
    ),
    "Response variable not found"
  )
})

test_that("Function stops when predictor variable not found", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("invalid", "x2", "x3"),
      data = data
    ),
    "One or more predictor variables not found"
  )
})

# Validate response variable
test_that("Function throws error when response has zero or negative values", {
  skip_on_cran()
  data_invalid1 <- data
  data_invalid1$y_obs[1] <- -1 # Inject invalid response
  data_invalid2 <- data
  data_invalid2$y_obs[1] <- 0 # Inject invalid response

  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_invalid1
    ),
    "contains zero or negative values"
  )
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_invalid2
    ),
    "contains zero or negative values"
  )
})

# Validate prior
test_that("Function accepts valid priors", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data,
      prior = c(
        prior(normal(0.1,0.1), class = "b"),
        prior(normal(1,1), class = "Intercept")
      )
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid prior", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data,
      prior = "invalid_prior"
    ),
    "Argument 'prior' must be a 'brmsprior' object."
  )
})

# Validate random effects
test_that("Function supports random effects", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      data = data
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid random effect variable", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "invalid_group",
      data = data
    ),
    "Variable 'invalid_group' not found in the data."
  )
})

# Validate handle missing
test_that("Function support handle missing data with 'model' method", {
  skip_on_cran()
  data_missing1 <- data
  data_missing1$y_obs[1] <- NA
  data_missing1$x1[2] <- NA

  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_missing1,
      handle_missing = "model"
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handle missing data without method argument", {
  skip_on_cran()
  data_missing1 <- data
  data_missing1$y_obs[1] <- NA
  data_missing1$x1[2] <- NA
  
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_missing1
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handle missing data with 'deleted' method", {
  skip_on_cran()
  data_missing2 <- data
  data_missing2$y_obs[1] <- NA

  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_missing2,
      handle_missing = "deleted"
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handle missing data with 'multiple' method", {
  skip_on_cran()
  data_missing1 <- data
  data_missing1$y_obs[1] <- NA
  data_missing1$x1[2] <- NA

  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_missing1,
      handle_missing = "multiple"
    )
  })
  expect_s3_class(model, "hbmfit")
})



test_that("Function throws an error when handle missing does not fit with the condition", {
  skip_on_cran()
  data_missing1 <- data
  data_missing1$y_obs[1] <- NA
  data_missing1$x1[2] <- NA

  # When response variable missing, handle missing is deleted. but there are missing in predictor
  expect_error(hbm_lnln(
    response = "y_obs",
    predictors = c("x1", "x2", "x3"),
    data = data_missing1,
    handle_missing = "deleted"
  ))
})

# Validate spatial effect
test_that("Function supports spatial random effects", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "car",
      M = M,
      data = data
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid spatial random effect", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "invalid",
      sre_type = "car",
      car_type = "icar",
      M = M,
      data = data
    ),
    "Variable 'invalid' not found in the data."
  )
})

test_that("Function supports spatial random effects without specified parameter", {
  skip_on_cran()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      sre = "sre",
      sre_type = "car",
      M = M,
      data = data
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid car type", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      sre = "sre",
      sre_type = "car",
      car_type = "invalid",
      M = M,
      data = data
    ),
    "'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'"
  )
})


test_that("Function supports spatial random effects with missing value in sre", {
  skip_on_cran()
  data_missing1 <- data
  data_missing1$area[1] <- NA
  
  model <- suppressWarnings(hbm_lnln(
    response = "y_obs",
    predictors = c("x1", "x2", "x3"),
    sre = "sre",
    sre_type = "car",
    M = M,
    data = data
  ))
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect type", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "invalid",
      M = M,
      data = data
    ),
    "Invalid spatial effect type. Use 'car' or 'sar'."
  )
})

test_that("Function throws error for spatial random effect type = sar", {
  skip_on_cran()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "sar",
      M = M,
      data = data
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
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "car",
      car_type = "icar",
      M = adjacency_matrix_wrong,
      data = data
    ),
    "'M' for CAR terms must be symmetric."
  )
  
  adjacency_matrix_wrong2 <- M
  adjacency_matrix_wrong2[1, 3] <- 2
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "car",
      M = adjacency_matrix_wrong2,
      data = data
    ),
    "'M' for CAR terms must be symmetric."
  )
  
  adjacency_matrix_wrong3 <- M
  adjacency_matrix_wrong3[1, 3] <- 2
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "car",
      M = adjacency_matrix_wrong3,
      data = data
    )
  )
  
  adjacency_matrix_wrong4 <- M
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-3, -3]
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "car",
      M = adjacency_matrix_wrong4,
      data = data
    ),
    "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  adjacency_matrix_wrong5 <- M
  rownames(adjacency_matrix_wrong5) <- as.character(6:10)
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      sre = "sre",
      sre_type = "car",
      M = adjacency_matrix_wrong5,
      data = data
    ),
    "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  expect_error(
    suppressWarnings(
      hbm_lnln(
        response = "y_obs",
        predictors = c("x1", "x2", "x3"),
        group = "group",
        sre_type = "car",
        M = adjacency_matrix_wrong4,
        data = data
      )
    ),
    "Dimensions of 'M' for CAR terms must be equal to the number of observations.",
  )
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  skip_on_cran()
  data2 <- data
  data2$sre <- rep(1:4) 
  
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      sre = "sre",
      sre_type = "car",
      car_type = "icar",
      M = M,
      data = data2
    )
    expect_s3_class(model, "hbmfit")
  })
})
