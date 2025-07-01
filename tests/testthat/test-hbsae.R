pdf(NULL)
on.exit(dev.off(), add = TRUE)

data_dummy <- data_fhnorm
fit <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2), data = data_dummy))

# hbsae
test_that("hbsae runs correctly with hbmfit object", {
  skip_on_cran()
  
  result <- hbsae(fit)
  
  expect_s3_class(result, "hbsae_results")
  expect_type(result, "list")
  expect_true("result_table" %in% names(result))
  expect_true("rse_model" %in% names(result))
  expect_true("pred" %in% names(result))
})

test_that("hbsae throws error for invalid model input", {
  skip_on_cran()
  
  expect_error(hbsae("invalid"), "Input model must be a brmsfit or hbmfit object.")
})

test_that("hbsae works with newdata", {
  skip_on_cran()
  
  result <- suppressWarnings(hbsae(fit, newdata = data_dummy))
  
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae can handle handle_missing = 'deleted'", {
  skip_on_cran()
  
  fit_deleted <- fit
  fit_deleted$handle_missing <- "deleted"
  fit_deleted$data <- fit$data
  
  result <- hbsae(fit_deleted)
  expect_s3_class(result, "hbsae_results")
})

# for binomial
n <- 30  
data_binom <- data.frame(
  y = rbinom(n, size = 10, prob = 0.5),  
  n = rep(10, n),                   
  x1 = rnorm(n),                          
  x2 = rnorm(n),
  group = factor(1:n)                      
)

fit_binom <- suppressWarnings(hbm_binlogitnorm(response = "y", 
                                              trials = "n", 
                                              predictors = c("x1", "x2"), 
                                              data = data_binom))

test_that("hbsae handles binomial logit-normal model", {
  skip_on_cran()
  
  result <- hbsae(fit_binom)
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae handles binomial logit-normal model with new data", {
  skip_on_cran()
  
  result_new <- hbsae(fit_binom, newdata = data_binom)
  expect_s3_class(result_new, "hbsae_results")
})

test_that("hbsae handles binomial logit-normal model with missing y and deleted handling", {
  skip_on_cran()
  
  # Buat sebagian nilai y menjadi NA
  data_binom$y[c(5, 10, 15)] <- NA
  
  # Fit model dengan handle_missing = "deleted"
  fit_binom <- suppressWarnings(hbm_binlogitnorm(
    response = "y",
    trials = "n",
    predictors = c("x1", "x2"),
    data = data_binom,
    handle_missing = "deleted"
  ))
  
  result <- hbsae(fit_binom)
  expect_s3_class(result, "hbsae_results")
})