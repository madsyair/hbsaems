# Data Dummy For Logit Normal Model
n <- 30  
sample_data2 <- data.frame(
  y = rbinom(n, size = 10, prob = 0.5),  
  n = rep(10, n),                   
  x1 = rnorm(n),                          
  x2 = rnorm(n),
  group = factor(1:n)                      
)

# Dummy spatial weight matrix
adjacency_matrix <- matrix(0, n, n)
for (i in 1:(n - 1)) {
  adjacency_matrix[i, i + 1] <- 1
  adjacency_matrix[i + 1, i] <- 1
}

# Set row and column names to match the group levels
rownames(adjacency_matrix) <- levels(sample_data2$group)
colnames(adjacency_matrix) <- levels(sample_data2$group)

# Expected result
test_that("Function returns a model object", {
  skip_on_cran()
  
  model_logit <- suppressWarnings(hbm_logitnormal(response = "y", 
                                                  trials = "n", 
                                                  predictors = c("x1", "x2"), 
                                                  data = sample_data2))
  expect_s3_class(model_logit, "hbmfit")
  expect_named(model_logit, c("model", "handle_missing", "data"), ignore.order = TRUE)
})

# Variable not exixt
test_that("Function throws error when response is missing", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "z", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               data = sample_data2),
               "Response variable not found in 'data'.")
})

test_that("Function throws error when predictors are missing", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x3"), 
                               data = sample_data2),
               "One or more predictor variables not found in 'data'.")
})

test_that("Function throws error when trials are missing", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "m", 
                               predictors = c("x1", "x2"), 
                               data = sample_data2),
               "Trials not found in 'data'.")
})

# Validate variable
test_that("Function throws error when response contains a negative integer.", {
  skip_on_cran()
  
  data_wrong1 <- sample_data2
  data_wrong1$y[1] <- -1
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               data = data_wrong1),
               "Response must be a non-negative integer.")
})

test_that("Function throws error when response is greater than the number of trials.", {
  skip_on_cran()
  
  data_wrong2 <- sample_data2
  data_wrong2$y[1] <- 101
  data_wrong2$n[1] <- 100
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               data = data_wrong2),
               "Response cannot be greater than the number of trials.")
})

test_that("Function throws error when number of trials is not a positive integer.", {
  skip_on_cran()
  
  data_wrong3 <- sample_data2
  data_wrong3$n[1] <- -1
  data_wrong3$y[1] <- 0  # Ensure response <= trials
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               data = data_wrong3),
               "Number of trials must be a positive integer.")
})

test_that("Function throws error when trials contain NA", {
  skip_on_cran()
  
  data_wrong_na <- sample_data2
  data_wrong_na$n[1] <- NA
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               data = data_wrong_na),
               "Trials contains NA values. The model cannot proceed.")
})


# Validate prior
test_that("Function accepts valid priors", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm_logitnormal(response = "y", 
                                   trials = "n", 
                                   predictors = c("x1", "x2"), 
                                   data = sample_data2,
                                   prior = c(
                                     set_prior("normal(0, 1)", class = "b")
                                   ))
  })
  expect_s3_class(model_logit, "hbmfit")
})

test_that("Function throws error for invalid prior", {
  skip_on_cran()
  
  expect_error(
    hbm_logitnormal(response = "y", 
                    trials = "n", 
                    predictors = c("x1", "x2"), 
                    data = sample_data2,
                    prior = "invalid"),
    "Argument 'prior' must be a 'brmsprior' object."
  )
})

# Validate spatial effect
test_that("Function throws error when adjacency matrix is incorrect", {
  skip_on_cran()
  
  adj_matrix_wrong <- matrix(c(0, 1, 1,
                               1, 0, 0), nrow = 2, byrow = TRUE)
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               M = adj_matrix_wrong,
                               data = sample_data2),
               "'M' for CAR terms must be symmetric.")
})

test_that("Function throws error for invalid spatial random effect type", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "invalid",
                               M = adjacency_matrix,
                               data = sample_data2),
               "Invalid spatial effect type. Use 'car' or 'sar'.")
})

test_that("Function throws error for invalid spatial random effect type sar", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "sar",
                               M = adjacency_matrix,
                               data = sample_data2),
               "Currently, only families gaussian and student support SAR structures.")
})

test_that("Function throws error for invalid car type", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               car_type = "invalid",
                               M = adjacency_matrix,
                               data = sample_data2),
               "'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'")
})

test_that("Function supports spatial random effects", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm_logitnormal(response = "y", 
                                   trials = "n", 
                                   predictors = c("x1", "x2"), 
                                   re = "group",
                                   sre = "group",
                                   sre_type = "car",
                                   M = adjacency_matrix,
                                   data = sample_data2)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function supports spatial random effects without specified parameter", {
  skip_on_cran()
  
  suppressWarnings({
    model_logit <- hbm_logitnormal(response = "y", 
                                   trials = "n", 
                                   predictors = c("x1", "x2"), 
                                   re = "group",
                                   sre = "group",
                                   sre_type = "car",
                                   car_type = "icar",
                                   M = adjacency_matrix,
                                   data = sample_data2)
    expect_s3_class(model_logit, "hbmfit")
  })
})

test_that("Function supports spatial random effects with missing value in sre", {
  skip_on_cran()
  
  data_missing_sre <- sample_data2
  data_missing_sre$group[1] <- NA
  
  model <- suppressWarnings(hbm_logitnormal(response = "y", 
                                            trials = "n", 
                                            predictors = c("x1", "x2"), 
                                            sre = "group",
                                            sre_type = "car",
                                            M = adjacency_matrix,
                                            data = data_missing_sre))
  expect_s3_class(model, "hbmfit")
})

test_that("Function throws error for invalid spatial random effect type", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               sre = "group",
                               sre_type = "invalid",
                               M = adjacency_matrix,
                               data = sample_data2),
               "Invalid spatial effect type. Use 'car' or 'sar'.")
})

# adj matrix
test_that("Function throws error when adjacency matrix is incorrect", {
  skip_on_cran()
  
  adjacency_matrix_wrong <- matrix(c(0, 1, 1,
                                     1, 0, 0), nrow = 2, byrow = TRUE)
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               M = adjacency_matrix_wrong,
                               data = sample_data2),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong2 <- adjacency_matrix
  adjacency_matrix_wrong2[1,4] <- 1
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               M = adjacency_matrix_wrong2,
                               data = sample_data2),
               "'M' for CAR terms must be symmetric.")
  
  adjacency_matrix_wrong3 <- adjacency_matrix
  adjacency_matrix_wrong3[1,2] <- 0
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               M = adjacency_matrix_wrong3,
                               data = sample_data))
  
  adjacency_matrix_wrong4 <- adjacency_matrix
  adjacency_matrix_wrong4 <- adjacency_matrix_wrong4[-30, -30]
  
  expect_error(hbm_logitnormal(response = "y",
                               trials = "n",
                               predictors = c("x1", "x2"),
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               M = adjacency_matrix_wrong4,
                               data = sample_data2
  ),
  "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  adjacency_matrix_wrong5 <- adjacency_matrix
  rownames(adjacency_matrix_wrong5) <- 2:31
  
  expect_error(hbm_logitnormal(response = "y",
                               trials = "n",
                               predictors = c("x1", "x2"),
                               re = "group",
                               sre = "group",
                               sre_type = "car",
                               M = adjacency_matrix_wrong5,
                               data = sample_data2
  ),
  "Row names of 'M' for CAR terms do not match the names of the grouping levels."
  )
  
  expect_error(suppressWarnings(hbm_logitnormal(response = "y",
                                                trials = "n",
                                                predictors = c("x1", "x2"),
                                                re = "group",
                                                sre_type = "car",
                                                M = adjacency_matrix_wrong4,
                                                data = sample_data2
  )
  ),
  "Dimensions of 'M' for CAR terms must be equal to the number of observations.",
  )
})

test_that("Function supports with the number of dimensions greater than the number of locations sre", {
  skip_on_cran()
  
  data_adj_dim <- sample_data2
  data_adj_dim$group <- rep(1:10, length.out = n)
  
  model_beta8 <- suppressWarnings(hbm_logitnormal(response = "y", 
                                                  trials = "n", 
                                                  predictors = c("x1", "x2"), 
                                                  sre = "group",
                                                  sre_type = "car",
                                                  M = adjacency_matrix,
                                                  data = data_adj_dim))
  expect_s3_class(model_beta8, "hbmfit")
})

#Validate random effects
test_that("Function supports random effects", {
  skip_on_cran()
  
  suppressWarnings({
    model <- hbm_logitnormal(response = "y", 
                             trials = "n", 
                             predictors = c("x1", "x2"), 
                             re = "group",
                             data = sample_data2)
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function throws error for invalid random effect variable", {
  skip_on_cran()
  
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               re = "invalid",
                               data = sample_data2),
               "Variable 'invalid' not found in the data." )
})

#Validate Handle Missing
test_that("Function throws an error when handle missing does not fit with the condition", {
  skip_on_cran()
  
  #Missing at response and predictor
  data_miss_binom2 <- sample_data2
  data_miss_binom2$y[1] <- NA
  data_miss_binom2$x1[3] <- NA
  
  # When predictor variable missing handle missing is deleted but there are missing in predictor
  expect_error(hbm_logitnormal(response = "y", 
                               trials = "n", 
                               predictors = c("x1", "x2"), 
                               data = data_miss_binom2, 
                               handle_missing = "deleted"))
})

test_that("Function support handles missing data with 'multiple' method", {
  skip_on_cran()
  
  #Missing at response and predictor
  data_miss_binom2 <- sample_data2
  data_miss_binom2$y[1] <- NA
  data_miss_binom2$x1[3] <- NA
  
  suppressWarnings({
    model <- hbm_logitnormal(response = "y", 
                             trials = "n", 
                             predictors = c("x1", "x2"), 
                             re = "group",
                             data = sample_data2,
                             handle_missing = "multiple")
    expect_s3_class(model, "hbmfit")
  })
})

test_that("Function support handles missing data with 'deleted' method", {
  skip_on_cran()
  
  # Missing at response
  data_miss_binom1 <- sample_data2
  data_miss_binom1$y[1] <- NA
  
  suppressWarnings({
    model <- hbm_logitnormal(response = "y", 
                             trials = "n", 
                             predictors = c("x1", "x2"), 
                             re = "group",
                             data = sample_data2,
                             handle_missing = "deleted")
    expect_s3_class(model, "hbmfit")
  })
})
