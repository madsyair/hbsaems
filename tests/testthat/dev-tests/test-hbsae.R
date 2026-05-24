# tests/testthat/dev-tests/test-hbsae.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

pdf(NULL)
on.exit(dev.off(), add = TRUE)

data_dummy <- data_fhnorm
fit <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2), data = data_dummy))

# hbsae
test_that("hbsae handles binomial logit-normal model with missing y and deleted handling", {
  .dev_skip()
  
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


# === Migrated back from main tests (ghost-variable dependence) ===
test_that("hbsae runs correctly with hbmfit object", {
  .dev_skip()
  result <- hbsae(fit)
  
  expect_s3_class(result, "hbsae_results")
  expect_type(result, "list")
  expect_true("result_table" %in% names(result))
  expect_true("rse_model" %in% names(result))
  expect_true("pred" %in% names(result))
})

test_that("hbsae works with newdata", {
  .dev_skip()
  result <- suppressWarnings(hbsae(fit, newdata = data_dummy))
  
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae can handle handle_missing = 'deleted'", {
  .dev_skip()
  fit_deleted <- fit
  fit_deleted$handle_missing <- "deleted"
  fit_deleted$data <- fit$data
  
  result <- hbsae(fit_deleted)
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae handles binomial logit-normal model", {
  .dev_skip()
  result <- hbsae(fit_binom)
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae handles binomial logit-normal model with new data", {
  .dev_skip()
  result_new <- hbsae(fit_binom, newdata = data_binom)
  expect_s3_class(result_new, "hbsae_results")
})


# === Re-migrated from main (require real fits) ===
test_that("hbsae throws error for invalid model input", {
  .dev_skip()
  expect_error(hbsae("invalid"), "Input model must be a brmsfit or hbmfit object.")
})
