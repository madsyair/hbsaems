# tests/testthat/dev-tests/test-update_hbm.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

# Avoid eager Stan compilation at file-source time -- if a user runs the
# whole dev-tests directory without NOT_CRAN=true, we should NOT spend
# minutes fitting models that will be skipped.  Each test_that() block
# below calls .dev_skip() first and then fits inside the block.

.update_hbm_fit_setup <- function() {
  set.seed(123)
  df <- data_fhnorm

  # Fit original model.  NOTE: in v1.0.0 the dataset column was renamed
  # from `group` to `regency` (see data-raw/data_fhnorm.R).
  fit <- suppressWarnings(hbm(
    formula = y ~ x1 + (1 | regency),
    data = df,
    iter = 500, warmup = 250, chains = 2, cores = 1,
    hb_sampling = "gaussian"))

  # Fit second model (a different formula on the same data).
  fit2 <- suppressWarnings(hbm(
    formula = brms::bf(y ~ x1) + brms::bf(x1 ~ 1),
    data = df,
    iter = 500, warmup = 250, chains = 2, cores = 1,
    hb_sampling = "gaussian"))

  list(fit = fit, fit2 = fit2, df = df)
}

test_that("update_hbm works with brmsfit and hbmfit objects", {
  .dev_skip()
  setup <- .update_hbm_fit_setup()

  # Test update with brmsfit
  fit_brms <- setup$fit$model
  updated_brms <- suppressWarnings(update_hbm(fit_brms, iter = 1000))
  expect_s3_class(updated_brms, "brmsfit")

  # Test update with hbmfit
  updated_hbm <- suppressWarnings(update_hbm(setup$fit, iter = 1000))
  expect_s3_class(updated_hbm, "hbmfit")

  # Test update with wrong model
  fit_wrong <- "wrong"
  expect_error(update_hbm(fit_wrong, iter = 1000))

  # Test update with multivariate model
  updated_hbm <- suppressWarnings(update_hbm(setup$fit2, newdata = setup$df))
  expect_s3_class(updated_hbm, "hbmfit")
})

test_that("update_hbm handles newdata correctly", {
  .dev_skip()
  setup <- .update_hbm_fit_setup()

  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    area = factor(rep(1:10, each = 5))
  )

  # Valid newdata (same structure)
  newdf <- df
  updated <- suppressWarnings(update_hbm(setup$fit, newdata = newdf))
  expect_s3_class(updated, "hbmfit")

  # Missing variable
  df_missing_var <- df
  df_missing_var$x1 <- NULL
  expect_error(update_hbm(setup$fit, newdata = df_missing_var))

  # NA values
  df_with_na <- df
  df_with_na$y[1] <- NA
  expect_error(update_hbm(setup$fit, newdata = df_with_na))
})

test_that("update_hbm detects and fills missing grouping variable", {
  .dev_skip()
  setup <- .update_hbm_fit_setup()

  set.seed(789)
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50)
  )

  updated <- suppressWarnings(update_hbm(
    setup$fit, newdata = df,
    iter = 600, warmup = 300, chains = 5, cores = 2,
    control = list(adapt_delta = 0.95, max_treedepth = 15)))
  expect_s3_class(updated, "hbmfit")
})
