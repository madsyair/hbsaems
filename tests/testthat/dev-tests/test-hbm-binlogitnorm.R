# Load data
data <- data_binlogitnorm
adjacency_matrix <- adjacency_matrix_car

# Expected result
test_that("Function returns a model object", {
  skip_on_cran()
  
  model_logit <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                                  trials = "n", 
                                                  predictors = c("x1", "x2", "x3"), 
                                                  data = data))
  expect_s3_class(model_logit, "hbmfit")
  expect_named(model_logit, c("model", "handle_missing", "data"), ignore.order = TRUE)
})

# Variable not exixt
test_that("Function throws error when response is missing", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "z_dir", 
                               trials = "n", 
                               predictors = c("x1", "x2", "x3"),
                               data = data),
               "Response variable not found in 'data'.")
})

test_that("Function throws error when predictors are missing", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x4"),
                               data = data),
               "One or more predictor variables not found in 'data'.")
})

test_that("Function throws error when trials are missing", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "m_i", 
                                predictors = c("x1", "x2", "x3"),
                               data = data),
               "Trials not found in 'data'.")
})

# Validate variable
test_that("Function throws error when response contains a negative integer.", {
  skip_on_cran()
  
  data_wrong1 <- data
  data_wrong1$y[1] <- -1
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                data = data_wrong1),
               "Response must be a non-negative integer.")
})

test_that("Function throws error when response is greater than the number of trials.", {
  skip_on_cran()
  
  data_wrong2 <- data
  data_wrong2$y[1] <- 101
  data_wrong2$n[1] <- 100
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                data = data_wrong2),
               "Response cannot be greater than the number of trials.")
})

test_that("Function throws error when number of trials is not a positive integer.", {
  skip_on_cran()
  
  data_wrong3 <- data
  data_wrong3$n[1] <- -1
  data_wrong3$y[1] <- 0  # Ensure response <= trials
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                data = data_wrong3),
               "Number of trials must be a positive integer.")
})

test_that("Function throws error when trials contain NA", {
  skip_on_cran()
  
  data_wrong_na <- data
  data_wrong_na$n[1] <- NA
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                data = data_wrong_na),
               "Trials contains NA values. The model cannot proceed.")
})


# Validate prior
test_that("Function accepts valid priors", {
  skip_on_cran()
  
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

test_that("Function throws error for invalid prior", {
  skip_on_cran()
  
  expect_error(
    hbm_binlogitnorm(response = "y", 
                     trials = "n", 
                     predictors = c("x1", "x2", "x3"),
                     data = data,
                     prior = "invalid"),
    "Argument 'prior' must be a 'brmsprior' object."
  )
})

#Validate random effects
test_that("Function supports random effects", {
  skip_on_cran()
  
  suppressWarnings({
    model <- hbm_binlogitnorm(response = "y", 
                              trials = "n", 
                              predictors = c("x1", "x2", "x3"), 
                              group = "group",
                              data = data)
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid random effect variable", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "invalid",
                                data = data),
               "Variable 'invalid' not found in the data." )
})

# Validate spatial effect
test_that("Function supports spatial random effects", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm_binlogitnorm(response = "y", 
                                    trials = "n", 
                                    predictors = c("x1", "x2", "x3"),
                                    group = "group",
                                    sre = "sre",
                                    sre_type = "car",
                                    M = adjacency_matrix,
                                    data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function throws error for invalid spatial random effect", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                sre = "invalid",
                                sre_type = "car",
                                M = adjacency_matrix,
                                data = data),
               "Variable 'invalid' not found in the data.")
})


test_that("Function supports spatial random effects without specified parameter", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm_binlogitnorm(response = "y", 
                                    trials = "n", 
                                    predictors = c("x1", "x2", "x3"), 
                                    group = "group",
                                    sre = "sre",
                                    M = adjacency_matrix,
                                    data = data)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function throws error for invalid car type", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                group = "group",
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
  
  model <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                             trials = "n", 
                                             predictors = c("x1", "x2", "x3"), 
                                             sre = "sre",
                                             sre_type = "car",
                                             M = adjacency_matrix,
                                             data = data_missing_sre))
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect type", {
  skip_on_cran()
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                sre = "sre",
                                sre_type = "invalid",
                                M = adjacency_matrix,
                                data = data),
               "Invalid spatial effect type. Use 'car' or 'sar'.")
})

test_that("Function throws error for spatial random effect type = sar", {
  skip_on_cran()
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
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
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                sre = "sre",
                                sre_type = "car",
                                car_type = "icar",
                                M = adjacency_matrix_wrong,
                                data = data),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,3] <- 1
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"), 
                                group = "group",
                                sre = "sre",
                                sre_type = "car",
                                M = adjacency_matrix_wrong2,
                                data = data)))
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1,3] <- 2
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                sre = "sre",
                                sre_type = "car",
                                M = adjacency_matrix_wrong3,
                                data = data)))
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-3, -3]
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                sre = "sre",
                                sre_type = "car",
                                M = adjacency_matrix_wrong4,
                                data = data)
  ))
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- as.character(6:10)
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                trials = "n", 
                                predictors = c("x1", "x2", "x3"),
                                group = "group",
                                sre = "sre",
                                sre_type = "car",
                                M = adjacency_matrix_wrong5,
                                data = data
  ),
  "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  expect_error(suppressWarnings(hbm_binlogitnorm(response = "y", 
                                                 trials = "n", 
                                                 predictors = c("x1", "x2", "x3"),
                                                 group = "group",
                                                 sre_type = "car",
                                                 M = adjacency_matrix_wrong4,
                                                 data = data)
  ))
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  skip_on_cran()
  
  data_adj_dim <- data
  data_adj_dim$sre <- rep(1:4)
  
  model <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                                   trials = "n", 
                                                   predictors = c("x1", "x2", "x3"), 
                                                   sre = "sre",
                                                   sre_type = "car",
                                                   car_type = "icar",
                                                   M = adjacency_matrix,
                                                   data = data_adj_dim))
  expect_s3_class(model, "hbmfit")
})


#Validate Handle Missing
test_that("Function throws an error when handle missing does not fit with the condition", {
  skip_on_cran()
  
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


test_that("Function support handles missing data with 'deleted' method", {
  skip_on_cran()
  
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
  skip_on_cran()
  
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

test_that("Function do not support handles missing data with 'model' method in binlogitnorm model", {
  skip_on_cran()
  
  #Missing at response and predictor
  data_miss_binom2 <- data
  data_miss_binom2$y[1] <- NA
  data_miss_binom2$x1[3] <- NA
  
  expect_error(hbm_binlogitnorm(response = "y", 
                                  trials = "n", 
                                  predictors = c("x1", "x2", "x3"),
                                  group = "group",
                                  data = data_miss_binom2,
                                  handle_missing = "model"))
})