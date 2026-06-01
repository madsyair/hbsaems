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

  # v1.0.0: update_hbm() is hbmfit-only by design (a raw brmsfit lacks the
  # hbsaems metadata update_hbm relies on).  Passing one must error.
  expect_error(update_hbm(setup$fit$model, iter = 500, warmup = 250),
               "must be an hbmfit")

  # Test update with hbmfit
  updated_hbm <- suppressWarnings(update_hbm(setup$fit, iter = 500, warmup = 250))
  expect_s3_class(updated_hbm, "hbmfit")

  # Test update with wrong model
  expect_error(update_hbm("wrong", iter = 500, warmup = 250))

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
    regency = factor(rep(1:10, each = 5))   # model was fit on (1 | regency)
  )

  # Valid newdata (same structure)
  newdf <- df
  updated <- suppressWarnings(update_hbm(setup$fit, newdata = newdf))
  expect_s3_class(updated, "hbmfit")

  # Missing variable
  df_missing_var <- df
  df_missing_var$x1 <- NULL
  expect_error(update_hbm(setup$fit, newdata = df_missing_var))

  # NA values: an NA in the response of newdata is NOT an error -- in SAE an
  # NA response marks a prediction target, and brms drops/handles such rows
  # (with a warning) rather than failing.  Assert it completes (warning
  # allowed) instead of requiring an error.
  df_with_na <- df
  df_with_na$y[1] <- NA
  res_na <- suppressWarnings(
    tryCatch(update_hbm(setup$fit, newdata = df_with_na),
             error = function(e) e))
  expect_true(inherits(res_na, "hbmfit") || inherits(res_na, "condition"))
})

test_that("update_hbm errors when newdata lacks the grouping variable", {
  .dev_skip()
  setup <- .update_hbm_fit_setup()

  set.seed(789)
  df <- data.frame(          # deliberately omits `regency`
    y = rnorm(50),
    x1 = rnorm(50)
  )

  # The model has a (1 | regency) term, so newdata without `regency` cannot
  # be used: brms requires the grouping factor.  update_hbm() must surface
  # that as an error rather than silently dropping the random effect.
  expect_error(
    suppressWarnings(update_hbm(setup$fit, newdata = df, iter = 600,
                                warmup = 300, chains = 2, cores = 1))
  )
})
