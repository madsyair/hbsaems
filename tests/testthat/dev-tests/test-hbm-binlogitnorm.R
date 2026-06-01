# tests/testthat/dev-tests/test-hbm-binlogitnorm.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

# Load data
data <- data_binlogitnorm
# v1.0.0 compatibility for these dev-tests: the bundled data uses `regency`
# as the area id and has no `group`/`sre` columns.  Provide them as aliases
# so the legacy `group=`/`spatial_var =` arguments (still accepted, deprecated) and
# manual `data$sre[...] <- NA` edits below resolve to a real column.
if (!"group" %in% names(data)) data$group <- data$regency
if (!"sre"   %in% names(data)) data$sre   <- data$regency

adjacency_matrix <- adjacency_matrix_car_regency  # rownames match regency levels

# Expected result
test_that("Function returns a model object", {
  .dev_skip()
  
  model_logit <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                                  trials = "n", 
                                                  predictors = c("x1", "x2", "x3"), 
                                                  data = data))
  expect_s3_class(model_logit, "hbmfit")
  expect_named(model_logit, c("model", "handle_missing", "missing_method", "data"), ignore.order = TRUE)
})

test_that("Function accepts valid priors", {
  .dev_skip()
  
  suppressWarnings({
    model_logit <- hbm_binlogitnorm(response = "y", 
                                    trials = "n", 
                                    predictors = c("x1", "x2", "x3"), 
                                    data = data,
                                    prior = c(
                                      set_prior("normal(0, 1)", class = "b")
                                    ))
  })
  expect_s3_class(model_logit, "hbmfit")
})

test_that("Function supports random effects", {
  .dev_skip()
  
  suppressWarnings({
    model <- hbm_binlogitnorm(response = "y", 
                              trials = "n", 
                              predictors = c("x1", "x2", "x3"), 
                              group = "group",
                              data = data)
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function supports spatial random effects", {
  .dev_skip()
  
  suppressWarnings({
    model_logit <- hbm_binlogitnorm(response = "y", 
                                    trials = "n", 
                                    predictors = c("x1", "x2", "x3"),
                                    group = "group",
                                    spatial_var = "sre",
                                    spatial_model = "car",
                                    M = adjacency_matrix,
                                    data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function supports spatial random effects without specified parameter", {
  .dev_skip()
  
  suppressWarnings({
    model_logit <- hbm_binlogitnorm(response = "y", 
                                    trials = "n", 
                                    predictors = c("x1", "x2", "x3"), 
                                    group = "group",
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
  
  model <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                             trials = "n", 
                                             predictors = c("x1", "x2", "x3"), 
                                             spatial_var = "sre",
                                             spatial_model = "car",
                                             M = adjacency_matrix,
                                             data = data_missing_sre))
  expect_s3_class(model, "hbmfit")
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  .dev_skip()
  
  # Edge case: matrix has MORE locations than the grouping uses.  The grouping
  # levels must still be valid row names of M, so draw a subset of the
  # regency labels (M = adjacency_matrix_car_regency has 5 regency_0N rows).
  data_adj_dim <- data
  data_adj_dim$sre <- factor(rep(sprintf("regency_0%d", 1:4),
                                 length.out = nrow(data_adj_dim)),
                             levels = rownames(adjacency_matrix))
  
  model <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                                   trials = "n", 
                                                   predictors = c("x1", "x2", "x3"), 
                                                   spatial_var = "sre",
                                                   spatial_model = "car",
                                                   car_type = "icar",
                                                   M = adjacency_matrix,
                                                   data = data_adj_dim))
  expect_s3_class(model, "hbmfit")
})

test_that("Function support handles missing data with 'deleted' method", {
  .dev_skip()
  
  # Missing at response
  data_miss_binom1 <- data
  data_miss_binom1$y[1] <- NA
  
  suppressWarnings({
    model <- hbm_binlogitnorm(response = "y", 
                              trials = "n", 
                              predictors = c("x1", "x2", "x3"),
                              group = "group",
                              data = data_miss_binom1,
                              handle_missing = "deleted")
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function support handles missing data with 'multiple' method", {
  .dev_skip()
  
  #Missing at response and predictor
  data_miss_binom2 <- data
  data_miss_binom2$y[1] <- NA
  data_miss_binom2$x1[3] <- NA
  
  suppressWarnings({
    model <- hbm_binlogitnorm(response = "y", 
                              trials = "n", 
                              predictors = c("x1", "x2", "x3"),
                              group = "group",
                              data = data,
                              handle_missing = "multiple")
    expect_s3_class(model, "hbmfit")
  })
})


# === Migrated back from main tests (ghost-variable dependence) ===
test_that("Function throws an error when handle missing does not fit with the condition", {
  .dev_skip()
  #Missing at response and predictor
  data_miss_binom2 <- data
  data_miss_binom2$y[1] <- NA
  data_miss_binom2$x1[3] <- NA
  
  # When predictor variable missing handle missing is deleted but there are missing in predictor
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                data = data_miss_binom2, 
                                handle_missing = "deleted"))
})


# === Re-migrated from main (require real fits) ===
test_that("Function throws error when response is missing", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "z_dir", 
                               trials = "n", 
                               predictors = c("x1", "x2", "x3"),
                               data = data),
               "Response variable .* not found in 'data'")
})

test_that("Function throws error when predictors are missing", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x4"),
                               data = data),
               "Auxiliary variable\\(s\\) not found")
})

test_that("Function throws error when trials are missing", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "m_i", 
                                predictors = c("x1", "x2", "x3"),
                               data = data),
               "Addition variable .* not found in 'data'")
})

test_that("Function throws error when response contains a negative integer.", {
  .dev_skip()
  data_wrong1 <- data
  data_wrong1$y[1] <- -1
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                data = data_wrong1),
               "non-negative integers")
})

test_that("Function throws error when response is greater than the number of trials.", {
  .dev_skip()
  data_wrong2 <- data
  data_wrong2$y[1] <- 101
  data_wrong2$n[1] <- 100
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                data = data_wrong2),
               "must not exceed trials")
})

test_that("Function throws error when number of trials is not a positive integer.", {
  .dev_skip()
  data_wrong3 <- data
  data_wrong3$n[1] <- -1
  data_wrong3$y[1] <- 0  # Ensure response <= trials
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                data = data_wrong3),
               "must be strictly positive")
})

test_that("Function throws error when trials contain NA", {
  .dev_skip()
  data_wrong_na <- data
  data_wrong_na$n[1] <- NA
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                data = data_wrong_na),
               "Trials variable .n. contains missing values")
})

test_that("Function throws error for invalid random effect variable", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "invalid",
                                data = data))
})

test_that("Function throws error for invalid spatial random effect", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                spatial_var = "invalid",
                                spatial_model = "car",
                                M = adjacency_matrix,
                                data = data))
})

test_that("Function throws error for invalid car type", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                group = "group",
                                spatial_var = "sre",
                                spatial_model = "car",
                                car_type = "invalid",
                                M = adjacency_matrix,
                                data = data),
               "car_type.*should be one of|car_type")
})

test_that("Function throws error for invalid spatial random effect type", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                spatial_var = "sre",
                                spatial_model = "invalid",
                                M = adjacency_matrix,
                                data = data))
})

test_that("Function throws error for spatial random effect type = sar", {
  .dev_skip()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                spatial_var = "sre",
                                spatial_model = "sar",
                                M = adjacency_matrix,
                                data = data),
               "SAR|gaussian and student|only families")
})

test_that("Function throws error when adjacency matrix is incorrect", {
  .dev_skip()
  adjacency_matrix_wrong <- matrix(c(
    0, 1, 1,
    1, 0, 0
  ), nrow = 2, byrow = TRUE)
  # A 2x3 matrix is invalid for CAR.  The validator rejects it for being
  # non-square BEFORE it ever reaches the symmetry check, so assert that an
  # error is raised rather than pinning the exact message (which legitimately
  # describes the squareness failure, not symmetry).
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                spatial_var = "sre",
                                spatial_model = "car",
                                car_type = "icar",
                                M = adjacency_matrix_wrong,
                                data = data))
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,3] <- 1
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                group = "group",
                                spatial_var = "sre",
                                spatial_model = "car",
                                M = adjacency_matrix_wrong2,
                                data = data)))
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1,3] <- 2
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                spatial_var = "sre",
                                spatial_model = "car",
                                M = adjacency_matrix_wrong3,
                                data = data)))
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-3, -3]
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                spatial_var = "sre",
                                spatial_model = "car",
                                M = adjacency_matrix_wrong4,
                                data = data)
  ))
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- as.character(6:10)
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                spatial_var = "sre",
                                spatial_model = "car",
                                M = adjacency_matrix_wrong5,
                                data = data
  )),
  "do not match the levels")
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                                 trials = "n", 
                                                 predictors = c("x1", "x2", "x3"),
                                                 group = "group",
                                                 spatial_model = "car",
                                                 M = adjacency_matrix_wrong4,
                                                 data = data)
  ))
})
