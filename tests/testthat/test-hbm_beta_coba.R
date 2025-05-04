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