# tests/testthat/dev-tests/test-hbm-betalogitnorm.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

# Load data
data <- data_betalogitnorm
adjacency_matrix <- adjacency_matrix_car

# Expected result
test_that("Function returns a model object", {
  .dev_skip()
  # Model without n and deff information
  model <- suppressWarnings(hbm_betalogitnorm(response = "y",
                                              predictors = c("x1", "x2", "x3"),
                                              data = data))
  expect_s3_class(model, "hbmfit")
  expect_named(model, c("model", "handle_missing", "data"), ignore.order = TRUE)
  
  # Model with n and deff information
  model2 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                           predictors = c("x1", "x2", "x3"), 
                                           n = "n",
                                           deff = "deff",
                                           data = data))
  expect_s3_class(model2, "hbmfit")
  expect_named(model2, c("model", "handle_missing", "data"), ignore.order = TRUE)
  
  # Validate random effects
  model <- suppressWarnings(hbm_betalogitnorm(response = "y",
                                              predictors = c("x1", "x2", "x3"),
                                              group = "group",
                                              data = data))
  expect_s3_class(model, "hbmfit")
})

test_that("Function to check prior implementation", {
  .dev_skip()
  model <- suppressWarnings(hbm_betalogitnorm(response = "y",
                                              predictors = c("x1", "x2", "x3"),
                                              prior = c(
                                                set_prior("normal(0, 10)", class = "Intercept"),
                                                set_prior("normal(0, 5)", class = "b"),
                                                set_prior("gamma(1,1)", class = "phi")
                                              ),
                                              data = data))
  expect_s3_class(model, "hbmfit")
  
  model1 <- suppressWarnings(hbm_betalogitnorm(response = "y",
                                              predictors = c("x1", "x2", "x3"),
                                              prior = c(
                                                prior("normal(0, 10)", class = "Intercept"),
                                                prior("normal(0, 5)", class = "b")
                                              ),
                                              data = data))
  expect_s3_class(model1, "hbmfit")
  
  model2 <- suppressWarnings(hbm_betalogitnorm(response = "y",
                                              predictors = c("x1", "x2", "x3"),
                                              prior = c(
                                                prior("normal(0, 10)", class = "Intercept"),
                                                prior("gamma(1,1)", class = "phi")
                                              ),
                                              data = data))
  expect_s3_class(model2, "hbmfit")
  
  model3 <- suppressWarnings(hbm_betalogitnorm(response = "y",
                                              predictors = c("x1", "x2", "x3"),
                                              prior = c(
                                                prior("normal(0, 5)", class = "b"),
                                                prior("gamma(1,1)", class = "phi")
                                              ),
                                              data = data))
  expect_s3_class(model3, "hbmfit")
  
  expect_error(hbm_betalogitnorm(response = "y",
                                 predictors = c("x1", "x2", "x3"),
                                 prior = "invalid",
                                 data = data),
               "Argument 'prior' must be a 'brmsprior' object."
  )

  model4 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               n = "n",
                                               deff = "deff",
                                               prior = c(
                                                 prior("normal(0, 10)", class = "Intercept"),
                                                 prior("normal(0, 5)", class = "b")
                                               ),
                                               data = data))
  expect_s3_class(model4, "hbmfit")
  
  model5 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               n = "n",
                                               deff = "deff",
                                               prior = c(
                                                 prior("normal(0, 10)", class = "Intercept")
                                               ),
                                               data = data))
  expect_s3_class(model5, "hbmfit")
  
  model6 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               n = "n",
                                               deff = "deff",
                                               prior = c(
                                                 prior("normal(0, 5)", class = "b")
                                               ),
                                               data = data))
  expect_s3_class(model6, "hbmfit")
  
  expect_error(hbm_betalogitnorm(response = "y",
                                 predictors = c("x1", "x2", "x3"),
                                 n = "n",
                                 deff = "deff",
                                 prior = c(
                                   prior("normal(0, 5)", class = "invalid")
                                 ),
                                 data = data))
  expect_error(hbm_betalogitnorm(response = "y",
                                 predictors = c("x1", "x2", "x3"),
                                 n = "n",
                                 deff = "deff",
                                 prior = c(
                                   prior("normal(0, 10)", class = "Intercept"),
                                   prior("normal(0, 5)", class = "b"),
                                   prior("gamma(1,1)", class = "phi")
                                 ),
                                 data = data),
               "Remove priors for 'phi' if phi is fixed using n and deff."
  )
})

test_that("Function throws an error when handle missing does not fit with the condition", {
  .dev_skip()
  
  # Missing at response and predictor
  data_miss1 <- data
  data_miss1$y[1] <- NA
  data_miss1$x1[3] <- NA
  
  #Function support handles missing data with 'multiple' method
  model1 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"),
                                               data = data_miss1,
                                               handle_missing = "multiple"))
  expect_s3_class(model1, "hbmfit")
  
  # Missing at response
  #Function support handles missing data with 'deleted' method
  data_miss2 <- data
  data_miss2$y[1] <- NA
  model2 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                                 predictors = c("x1", "x2", "x3"),
                                                 data = data_miss2,
                                                 handle_missing = "deleted"))
  expect_s3_class(model2, "hbmfit")
  
  # When response variable missing, handle missing is deleted, but there are missing in predictor
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("x1", "x2", "x3"), 
                                 data = data_miss1, 
                                 handle_missing = "deleted"))
  
  # Function support handle missing data with 'model' method
  #model3 <- suppressWarnings(hbm_betalogitnorm(response = "y",
   #                                            predictors = c("x1", "x2", "x3"),
    #                                           data = data_miss1,
     #                                          handle_missing = "model"))
  #expect_s3_class(model3, "hbmfit")
    
})

test_that("Function supports spatial random effects", {
  .dev_skip()
  model1 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               sre = "sre",
                                               sre_type = "car",
                                               car_type = "icar",
                                               M = adjacency_matrix,
                                               data = data))
  expect_s3_class(model1, "hbmfit")
  
  model2 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               n = "n",
                                               deff = "deff",
                                               sre = "sre",
                                               sre_type = "car",
                                               car_type = "icar",
                                               M = adjacency_matrix,
                                               data = data))
  expect_s3_class(model2, "hbmfit")
  
  # Function supports spatial random effects withouh specified parameter
  adjacency_matrix_new <- matrix(0, 100, 100)
  for (i in 1:49) {
    adjacency_matrix_new[i, i + 1] <- 1
    adjacency_matrix_new[i + 1, i] <- 1
  }
  dimnames(adjacency_matrix_new) <- list(levels(data$group), levels(data$group))
  model3 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               sre_type = "car",
                                               car_type = "icar",
                                               M = adjacency_matrix_new,
                                               data = data))
  expect_s3_class(model3, "hbmfit")
  
  model4 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                               predictors = c("x1", "x2", "x3"), 
                                               n = "n",
                                               deff = "deff",
                                               sre_type = "car",
                                               car_type = "icar",
                                               M = adjacency_matrix_new,
                                               data = data))
  expect_s3_class(model4, "hbmfit")
  
  # Function supports spatial random effects with missing value in sre
  data_miss_spatial1 <- data
  data_miss_spatial1$sre[1] <- NA
  
  model5 <- suppressWarnings(hbm_betalogitnorm(response = "y", 
                                           predictors = c("x1", "x2", "x3"), 
                                           sre = "sre",
                                           sre_type = "car",
                                           M = adjacency_matrix,
                                           data = data_miss_spatial1))
  expect_s3_class(model5, "hbmfit")
  
})


# === Re-migrated from main (require real fits) ===
test_that("Function to check for errors in spatial effect models", {
  .dev_skip()
  expect_error(hbm_betalogitnorm(response = "y", 
                                 predictors = c("x1", "x2", "x3"), 
                                 sre = "sre",
                                 sre_type = "invalid",
                                 car_type = "escar",
                                 M = adjacency_matrix,
                                 data = data),
               "Invalid spatial effect type. Use 'car' or 'sar'.")
  
  expect_error(suppressWarnings(
      hbm_betalogitnorm(
        response = "y", 
        predictors = c("x1", "x2", "x3"), 
        sre_type = "car",
        car_type = "escar",
        M = adjacency_matrix,
        data = data
      )
    ),
    "Dimensions of 'M' for CAR terms must be equal to the number of observations."
  )
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        predictors = c("x1", "x2", "x3"), 
                        sre = "sre",
                        sre_type = "car",
                        car_type = "invalid",
                        M = adjacency_matrix,
                        data = data),
               "'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'")
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        predictors = c("x1", "x2", "x3"), 
                        sre = "sre",
                        sre_type = "sar",
                        M = adjacency_matrix,
                        data = data),
               "Currently, only families gaussian and student support SAR structures.")

})

test_that("Function throws error when adjacency matrix is incorrect", {
  .dev_skip()
  adjacency_matrix_wrong <- matrix(c(0, 1, 1,
                                     1, 0, 0), nrow = 2, byrow = TRUE)
  expect_error(hbm_betalogitnorm(response = "y", 
                        predictors = c("x1", "x2", "x3"), 
                        sre = "sre",
                        group = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong,
                        data = data),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,2] <- 1
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        predictors = c("x1", "x2", "x3"), 
                        sre = "sre",
                        group = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong2,
                        data = data),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-3, -3]
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        predictors = c("x1", "x2", "x3"), 
                        sre = "sre",
                        group = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong4,
                        data = data),
               "Row names of 'M' for CAR terms do not match the names of the grouping levels.")
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- 2:6
  
  expect_error(hbm_betalogitnorm(response = "y", 
                        predictors = c("x1", "x2", "x3"), 
                        sre = "sre",
                        group = "group",
                        sre_type = "car",
                        M = adjacency_matrix_wrong5,
                        data = data),
               "Row names of 'M' for CAR terms do not match the names of the grouping levels.")
})
