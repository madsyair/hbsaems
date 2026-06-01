# tests/testthat/dev-tests/test-hbm-lnln.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

# Data Dummy For Lognormal
data <- data_lnln
# v1.0.0 compatibility for these dev-tests: the bundled data uses `regency`
# as the area id and has no `group`/`sre` columns.  Provide them as aliases
# so the legacy `group=`/`spatial_var =` arguments (still accepted, deprecated) and
# manual `data$sre[...] <- NA` edits below resolve to a real column.
if (!"group" %in% names(data)) data$group <- data$regency
if (!"sre"   %in% names(data)) data$sre   <- data$regency

M <- adjacency_matrix_car_regency  # rownames match regency levels

# Expected result
test_that("Function returns a model object", {
  .dev_skip()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data
    )
    expect_s3_class(model, "hbmfit")
    expect_named(model, c("model", "handle_missing", "missing_method", "data"), ignore.order = TRUE)
  })
})

test_that("Function accepts valid priors", {
  .dev_skip()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data,
      prior = c(
        prior("normal(0.1,0.1)", class = "b"),
        prior("normal(1,1)", class = "Intercept")
      )
    )
  })
  expect_s3_class(model, "hbmfit")
})

test_that("Function supports random effects", {
  .dev_skip()
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

test_that("Function support handle missing data with 'model' method", {
  .dev_skip()
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

test_that("Function errors when missing data present and no method given", {
  .dev_skip()
  data_missing1 <- data
  data_missing1$y_obs[1] <- NA

  # By design (v1.0.0), if the data contain missing values and the user did
  # not write mi()/me() in the formula, hbm() requires handle_missing to be
  # set explicitly rather than guessing.  A string-based wrapper call with no
  # handle_missing must therefore raise that informative error.
  expect_error(
    suppressWarnings(hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_missing1
    )),
    "handle_missing"
  )
})

test_that("Function support handle missing data with 'deleted' method", {
  .dev_skip()
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
  .dev_skip()
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

test_that("Function supports spatial random effects", {
  .dev_skip()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "car",
      M = M,
      data = data
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function supports spatial random effects without specified parameter", {
  .dev_skip()
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      spatial_var = "sre",
      spatial_model = "car",
      M = M,
      data = data
    )
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function supports spatial random effects with missing value in sre", {
  .dev_skip()
  data_missing1 <- data
  data_missing1$area[1] <- NA
  
  model <- suppressWarnings(hbm_lnln(
    response = "y_obs",
    predictors = c("x1", "x2", "x3"),
    spatial_var = "sre",
    spatial_model = "car",
    M = M,
    data = data
  ))
  expect_s3_class(model, "hbmfit")
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  .dev_skip()
  data2 <- data
  data2$sre <- rep(1:4) 
  
  suppressWarnings({
    model <- hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      spatial_var = "sre",
      spatial_model = "car",
      car_type = "icar",
      M = M,
      data = data2
    )
    expect_s3_class(model, "hbmfit")
  })
})


# === Migrated back from main tests (ghost-variable dependence) ===
test_that("Function throws an error when handle missing does not fit with the condition", {
  .dev_skip()
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


# === Re-migrated from main (require real fits) ===
test_that("Function stops when response variable not found", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "invalid_var",
      predictors = c("x1", "x2", "x3"),
      data = data
    ),
    "Response variable .* not found in 'data'"
  )
})

test_that("Function stops when predictor variable not found", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("invalid", "x2", "x3"),
      data = data
    ),
    "Auxiliary variable\\(s\\) not found"
  )
})

test_that("Function throws error when response has zero or negative values", {
  .dev_skip()
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
    "must be strictly positive \\(y > 0\\)"
  )
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      data = data_invalid2
    ),
    "must be strictly positive \\(y > 0\\)"
  )
})

test_that("Function throws error for invalid random effect variable", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "invalid_group",
      data = data
    ))
})

test_that("Function throws error for invalid spatial random effect", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "invalid",
      spatial_model = "car",
      car_type = "icar",
      M = M,
      data = data
    ))
})

test_that("Function throws error for invalid car type", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      spatial_var = "sre",
      spatial_model = "car",
      car_type = "invalid",
      M = M,
      data = data
    ),
    "car_type.*should be one of|car_type"
  )
})

test_that("Function throws error for invalid spatial random effect type", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "invalid",
      M = M,
      data = data
    ))
})

test_that("Function throws error for spatial random effect type = sar", {
  .dev_skip()
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "sar",
      M = M,
      data = data
    ),
    "SAR|gaussian and student|only families"
  )
})

test_that("Function throws error when adjacency matrix is incorrect", {
  .dev_skip()
  adjacency_matrix_wrong <- matrix(c(
    0, 1, 1,
    1, 0, 0
  ), nrow = 2, byrow = TRUE)
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "car",
      car_type = "icar",
      M = adjacency_matrix_wrong,
      data = data
    ))
  
  adjacency_matrix_wrong2 <- M
  adjacency_matrix_wrong2[1, 3] <- 2
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "car",
      M = adjacency_matrix_wrong2,
      data = data
    ))
  
  adjacency_matrix_wrong3 <- M
  adjacency_matrix_wrong3[1, 3] <- 2
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "car",
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
      spatial_var = "sre",
      spatial_model = "car",
      M = adjacency_matrix_wrong4,
      data = data
    ))
  
  adjacency_matrix_wrong5 <- M
  rownames(adjacency_matrix_wrong5) <- as.character(6:10)
  
  expect_error(
    hbm_lnln(
      response = "y_obs",
      predictors = c("x1", "x2", "x3"),
      group = "group",
      spatial_var = "sre",
      spatial_model = "car",
      M = adjacency_matrix_wrong5,
      data = data
    ),
  "do not match the levels")
  
  expect_error(
    suppressWarnings(
      hbm_lnln(
        response = "y_obs",
        predictors = c("x1", "x2", "x3"),
        group = "group",
        spatial_model = "car",
        M = adjacency_matrix_wrong4,
        data = data
      )
    ),
    "Dimensions of 'M' for CAR terms must be equal to the number of observations.",
  )
})
