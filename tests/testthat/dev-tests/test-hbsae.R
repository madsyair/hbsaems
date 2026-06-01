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

# Binomial fixtures shared across several test_that() blocks below.  Defining
# them at file scope (not inside the first block) so later blocks that
# reference `data_binom` / `fit_binom` resolve them.
data_binom <- data_binlogitnorm
fit_binom  <- suppressWarnings(hbm_binlogitnorm(
  response = "y", trials = "n", predictors = c("x1", "x2"),
  data = data_binom))

# hbsae
test_that("hbsae handles binomial logit-normal model with missing y and deleted handling", {
  .dev_skip()
  
  # Buat sebagian nilai y menjadi NA (salinan lokal; jangan rusak fixture global)
  data_binom_na <- data_binom
  data_binom_na$y[c(5, 10, 15)] <- NA
  
  # Fit model dengan handle_missing = "deleted"
  fit_binom <- suppressWarnings(hbm_binlogitnorm(
    response = "y",
    trials = "n",
    predictors = c("x1", "x2"),
    data = data_binom_na,
    handle_missing = "deleted"
  ))
  
  result <- suppressWarnings(hbsae(fit_binom))
  expect_s3_class(result, "hbsae_results")
})


# === Migrated back from main tests (ghost-variable dependence) ===
test_that("hbsae runs correctly with hbmfit object", {
  .dev_skip()
  result <- suppressWarnings(hbsae(fit))
  
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
  
  result <- suppressWarnings(hbsae(fit_deleted))
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae handles binomial logit-normal model", {
  .dev_skip()
  result <- suppressWarnings(hbsae(fit_binom))
  expect_s3_class(result, "hbsae_results")
})

test_that("hbsae handles binomial logit-normal model with new data", {
  .dev_skip()
  result_new <- suppressWarnings(hbsae(fit_binom, newdata = data_binom))
  expect_s3_class(result_new, "hbsae_results")
})


# === Re-migrated from main (require real fits) ===
test_that("hbsae throws error for invalid model input", {
  .dev_skip()
  expect_error(suppressWarnings(hbsae("invalid")),
               "Input model must be a brmsfit or hbmfit object.")
})

# =============================================================================
# predict_type = "proportion" for binomial models (v1.1.0)
# Verifies the count-vs-proportion theory end-to-end against a real Stan fit.
# =============================================================================

test_that("sae_predict(proportion) returns area proportions in (0,1) for binomial", {
  .dev_skip()

  est <- suppressWarnings(sae_predict(fit_binom, predict_type = "proportion"))
  expect_s3_class(est, "hbsae_results")
  p <- est$result_table$Prediction
  # Proportions must lie strictly inside (0, 1).
  expect_true(all(p > 0 & p < 1),
              info = paste("range:", paste(round(range(p), 3), collapse = "-")))
})

test_that("sae_predict(epred) on binomial auto-converts to proportion with warning", {
  .dev_skip()

  warns <- character(0)
  est_epred <- withCallingHandlers(
    sae_predict(fit_binom, predict_type = "epred"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")
    }
  )
  # A warning must explain the count -> proportion conversion.
  expect_true(any(grepl("COUNT|PROPORTION|proportion", warns)),
              info = paste("warnings:", paste(warns, collapse = " | ")))
  # And the values must be proportions, matching predict_type = "proportion".
  est_prop <- suppressWarnings(
    sae_predict(fit_binom, predict_type = "proportion"))
  expect_equal(est_epred$result_table$Prediction,
               est_prop$result_table$Prediction, tolerance = 1e-6)
  expect_true(all(est_epred$result_table$Prediction < 1))
})

test_that("sae_predict(response) on binomial returns counts that track trials", {
  .dev_skip()

  est_resp <- suppressWarnings(
    sae_predict(fit_binom, predict_type = "response"))
  # Counts should exceed 1 (they are n_i * p_i with n_i >> 1), unlike the
  # proportion path which is bounded by 1.
  expect_true(mean(est_resp$result_table$Prediction) > 1)
})

test_that("binomial proportion equals epred / trials (theory check)", {
  .dev_skip()

  bm   <- fit_binom$model
  ep   <- colMeans(brms::posterior_epred(bm))
  nvec <- hbsaems:::.binomial_trials(bm, fit_binom$data)
  expect_false(is.null(nvec))
  prop_manual <- ep / nvec
  est_prop <- suppressWarnings(
    sae_predict(fit_binom, predict_type = "proportion"))
  expect_equal(unname(est_prop$result_table$Prediction),
               unname(prop_manual), tolerance = 1e-6)
})
