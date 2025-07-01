# Data Dummy For Test
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
fit <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2), data = data_dummy))
fit2 <- suppressWarnings(hbm(brms::bf(y ~ x1), data = data_dummy))

test_that("hbmc() throws error when model list is empty", {
  expect_error(
    hbmc(model = list()), 
    "If 'model' is a list, it must contain at least one brmsfit/hbmfit object."
  )
})

test_that("hbmc runs with a valid hbmfit model and returns expected components", {
  skip_on_cran()
  
  result <- suppressWarnings(hbmc(model = list(a=fit, b=fit2), comparison_metrics = c("aaa", "waic", "bf"), plot_types = c("pp_check", "params", "bbb"), moment_match = FALSE, run_prior_sensitivity= TRUE, sensitivity_var=c("b_Intercept", "b_x1")))
  result2 <- suppressWarnings(hbmc(model = list(fit, fit2), moment_match = TRUE, reloo_args = list(reloo = TRUE),run_prior_sensitivity= TRUE, sensitivity_var=c("b_Intercept", "b_x1")))
  expect_s3_class(result, "hbmc_results")
  expect_s3_class(result2, "hbmc_results")
})

test_that("hbmc throws an error for non-model input", {
  skip_on_cran()
  
  expect_error(hbmc(model = list("not_a_model")))
})

test_that("hbm triggers reloo when high Pareto k detected", {
  skip_on_cran()
  
  set.seed(123)
  N <- 100
  x <- rnorm(N)
  y <- 2 * x + rnorm(N)
  x[(N-4):N] <- x[(N-4):N] + 150   
  y[(N-4):N] <- y[(N-4):N] - 1000  
  area <- rep(1:10, each = 10)
  area[(N-4):N] <- 999
  df <- data.frame(y = y, x = x, area = area)
  
  fit <- suppressWarnings(hbm(
    formula = y ~ x + (1 | area),
    data = df,
    hb_sampling = "gaussian"
  ))
  
  result <- suppressWarnings(hbmc(model = list(fit), moment_match = TRUE, run_prior_sensitivity= TRUE, sensitivity_var=c("b_Intercept", "b_x1")))
  expect_s3_class(result, "hbmc_results")
})

test_that("Warning is issued when run_prior_sensitivity is TRUE but sensitivity_vars is NULL", {
  
  fit <- fit$model
  result <- suppressWarnings(hbmc(model = fit, comparison_metrics = NULL, moment_match = FALSE, run_prior_sensitivity= TRUE))
  expect_s3_class(result, "hbmc_results")
})

test_that("hbmc runs with skipping prior sensitivity analysis ", {
  skip_on_cran()
  
  fit$model$prior <- NULL
  result <- suppressWarnings(hbmc(model = list(a=fit), moment_match = FALSE, run_prior_sensitivity= TRUE, sensitivity_var=c("b_Intercept", "b_x1")))
  expect_s3_class(result, "hbmc_results")
})

