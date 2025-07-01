set.seed(123)
df <- data_fhnorm

# Fit original model
fit <- suppressWarnings(hbm(
                          formula = y ~ x1 + (1 | group),
                          data = df,
                          iter = 500, warmup = 250, chains = 2, cores = 1,
                          hb_sampling = "gaussian")
)

# Fit original model
fit2 <- suppressWarnings(hbm(
  formula = bf(y ~ x1) + bf(x1 ~ 1),
  data = df,
  iter = 500, warmup = 250, chains = 2, cores = 1,
  hb_sampling = "gaussian")
)

test_that("update_hbm works with brmsfit and hbmfit objects", {
  skip_on_cran()
  
  # Test update with brmsfit
  fit_brms <- fit$model
  updated_brms <- suppressWarnings(update_hbm(fit_brms, iter = 1000))
  expect_s3_class(updated_brms, "brmsfit")
  
  # Test update with hbmfit
  updated_hbm <- suppressWarnings(update_hbm(fit, iter = 1000))
  expect_s3_class(updated_hbm, "hbmfit")
  
  # Test update with wrong model
  fit_wrong <- "wrong"
  expect_error(update_hbm(fit_wrong, iter = 1000))
  
  # Test update with multivariate model
  updated_hbm <- suppressWarnings(update_hbm(fit2, newdata = df))
  expect_s3_class(updated_hbm, "hbmfit")
})

test_that("update_hbm handles newdata correctly", {
  skip_on_cran()
  
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    area = factor(rep(1:10, each = 5))
  )
  
  # Valid newdata (same structure)
  newdf <- df
  updated <- suppressWarnings(update_hbm(fit, newdata = newdf))
  expect_s3_class(updated, "hbmfit")
  
  # Missing variable
  df_missing_var <- df
  df_missing_var$x1 <- NULL
  expect_error(update_hbm(fit, newdata = df_missing_var))
  
  # NA values
  df_with_na <- df
  df_with_na$y[1] <- NA
  expect_error(update_hbm(fit, newdata = df_with_na))
})

test_that("update_hbm detects and fills missing grouping variable", {
  skip_on_cran()
  
  set.seed(789)
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50)
  )
  
  updated <- suppressWarnings(update_hbm(fit, newdata = df, iter = 600, warmup = 300, chains = 5, cores = 2, control = list(adapt_delta = 0.95, max_treedepth = 15)))
  expect_s3_class(updated, "hbmfit")
})
