# Data Dummy For Test
set.seed(123)
sample_data <- data.frame(
  y = rpois(100, lambda = 10),
  x1 = rnorm(100),
  x2 = rbinom(100, 1, 0.5),
  area = rep(1:10, each = 10),
  M = sample(c(NA, 1:3), 100, replace = TRUE)
)

pdf(NULL)
on.exit(dev.off(), add = TRUE)

data_dummy <- sample_data
fit <- suppressWarnings(hbm(bf(y ~ x1 + x2), data = data_dummy))

# hbcc
test_that("hbcc runs and returns invisibly for valid hbmfit", {
  result <- suppressWarnings(hbcc(model = fit))
  expect_type(result, "list")
})

test_that("hbcc throws error with invalid model input", {
  expect_error(hbcc(model = "not_a_model"))
  expect_error(hbcc(model = NULL))
})

test_that("hbcc throws error when plot_types is invalid", {
  # plot_types not dikenal
  expect_error(suppressWarnings(hbcc(model = fit, plot_types = "invalid_plot")))
})

test_that("hbcc throws error when diag_tests is invalid", {
  # diag_tests not valid
  expect_error(suppressWarnings(hbcc(model = fit, diag_tests = "wrong_diag")))
})

# hbmc
fit2 <- suppressWarnings(hbm(bf(y ~ x1), data = data_dummy))

test_that("hbmc runs with a valid hbmfit model and returns expected components", {
  result <- suppressWarnings(hbmc(fit))
  expect_s3_class(result, "hbmc")
  expect_type(result, "list")
})

test_that("hbmc throws an error for non-model input", {
  expect_error(hbmc("not_a_model"), "Input model must be a brmsfit or hbmfit object")
})

test_that("hbmc throws an error for invalid plot_types", {
  expect_error(
    hbmc(fit, plot_types = c("wrong_plot")),
    "Invalid diag_tests: wrong_plot"
  )
})

test_that("hbmc runs with two models and returns comparison metrics", {
  result <- suppressWarnings(hbmc(fit, model2 = fit2))
  
  expect_s3_class(result, "hbmc")
  expect_true(!is.null(result$waic2))
  expect_true(!is.null(result$loo2))
  expect_true(!is.null(result$bf))
  expect_s3_class(result$bf, "bf_bridge")
})

# hbsae
test_that("hbsae runs correctly with hbmfit object", {
  result <- hbsae(fit)
  
  expect_s3_class(result, "hbsae")
  expect_type(result, "list")
  expect_true("result_table" %in% names(result))
  expect_true("rse_model" %in% names(result))
  expect_true("pred" %in% names(result))
})

test_that("hbsae throws error for invalid model input", {
  expect_error(hbsae("invalid"), "Input model must be a brmsfit or hbmfit object.")
})

test_that("hbsae returns response scale correctly", {
  result <- hbsae(fit, scale = "response")
  
  expect_s3_class(result, "hbsae")
  expect_true(all(c("Prediction", "RSE_percent") %in% colnames(result$result_table)))
})

test_that("hbsae returns linear scale correctly", {
  result <- suppressWarnings(hbsae(fit, scale = "linear"))
  
  expect_s3_class(result, "hbsae")
  expect_true(all(c("Prediction", "RSE_percent") %in% colnames(result$result_table)))
})

test_that("hbsae returns linear_inverse scale correctly", {
  result <- suppressWarnings(hbsae(fit, scale = "linear_inverse"))
  
  expect_s3_class(result, "hbsae")
  expect_true(all(c("Prediction", "RSE_percent") %in% colnames(result$result_table)))
})

test_that("hbsae works with newdata", {
  result <- hbsae(fit, newdata = sample_data)
  result2 <- suppressWarnings(hbsae(fit, newdata = sample_data, scale = "linear"))
  result3 <- suppressWarnings(hbsae(fit, newdata = sample_data, scale = "linear_inverse"))
  
  expect_s3_class(result, "hbsae")
  expect_s3_class(result2, "hbsae")
  expect_s3_class(result3, "hbsae")
})

test_that("hbsae returns proper error for invalid scale input", {
  expect_error(hbsae(fit, scale = "unknown"), 
               'Prediction` must be either "response", "linear", or "linear_inverse.')
})

test_that("hbsae can handle handle_missing = 'deleted'", {
  fit_deleted <- fit
  fit_deleted$handle_missing <- "deleted"
  fit_deleted$data <- fit$data
  
  result <- hbsae(fit_deleted)
  result2 <- suppressWarnings(hbsae(fit_deleted, scale = "linear"))
  result3 <- suppressWarnings(hbsae(fit_deleted, scale = "linear_inverse"))
  expect_s3_class(result, "hbsae")
  expect_s3_class(result2, "hbsae")
  expect_s3_class(result3, "hbsae")
})
