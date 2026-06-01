# tests/testthat/test-prior-check.R
# =============================================================================
# CRAN-safe tests for the 1.1.0 prior_check() auto-detection feature and the
# hbm_binlogitnorm() NA-trials validation hardening.  These exercise only
# input resolution and error paths -- no Stan compile.  The success paths
# (real prior-predictive draws) live in tests/testthat/dev-tests/test-hbpc.R.
# =============================================================================

test_that("prior_check() errors when data cannot be auto-detected", {
  fake <- structure(
    list(model = structure(list(), class = "brmsfit"),
         data = NULL, missing_method = NULL),
    class = "hbmfit"
  )
  expect_error(
    prior_check(fake),
    "could not be automatically determined"
  )
})

test_that("prior_check() auto-detects the response from the model formula", {
  skip_if_not_installed("brms")
  fake <- structure(list(formula = brms::bf(y ~ x1 + x2)),
                    class = "brmsfit")
  # 'y' is resolved from the formula but absent from data -> auto-path error.
  expect_error(
    prior_check(fake, data = data.frame(x1 = 1, x2 = 2)),
    "Automatically determined response variable 'y' not found"
  )
})

test_that("prior_check() errors when the formula has no response", {
  skip_if_not_installed("brms")
  fake <- structure(list(formula = brms::bf(~ x1 + x2)),
                    class = "brmsfit")
  expect_error(
    prior_check(fake, data = data.frame(x1 = 1, x2 = 2)),
    "could not be automatically determined from the model formula"
  )
})

test_that("prior_check() keeps the explicit-argument contract", {
  skip_if_not_installed("brms")
  fake <- structure(list(formula = brms::bf(y ~ x1)),
                    class = "brmsfit")
  # Explicit response_var that is genuinely absent uses the original message.
  expect_error(
    prior_check(fake, data = data.frame(x1 = 1), response_var = "z"),
    "Response variable 'z' not found in 'data'."
  )
})

test_that(".detect_response_var resolves univariate and response-less forms", {
  skip_if_not_installed("brms")
  expect_identical(
    hbsaems:::.detect_response_var(
      structure(list(formula = brms::bf(y ~ x1)), class = "brmsfit")),
    "y"
  )
  expect_null(
    hbsaems:::.detect_response_var(
      structure(list(formula = brms::bf(~ x1)), class = "brmsfit"))
  )
})

# ---- hbm_binlogitnorm(): NA in trials is a hard error (1.1.0) ---------------

test_that("hbm_binlogitnorm() rejects missing values in trials", {
  skip_if_not_installed("brms")
  d <- data.frame(
    y  = c(2L, 3L, 4L),
    n  = c(10L, NA, 12L),
    x1 = stats::rnorm(3),
    area = factor(1:3)
  )
  expect_error(
    hbm_binlogitnorm(response = "y", trials = "n",
                     auxiliary = "x1", area_var = "area", data = d),
    "Trials variable 'n' contains missing values"
  )
})
