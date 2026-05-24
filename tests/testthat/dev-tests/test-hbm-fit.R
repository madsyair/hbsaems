# tests/testthat/test-hbm-fit.R
# =============================================================================
# Live Stan tests -- skipped on CRAN.
# Exercises hbm() -> new_hbmfit() -> S3 methods round-trip.
# =============================================================================

test_that("hbm() returns an hbmfit and S3 methods work", {
  .dev_skip()
  skip_if_no_stan()

  set.seed(123)
  d <- data.frame(
    y     = stats::rnorm(20),
    x1    = stats::rnorm(20),
    x2    = stats::rnorm(20),
    group = factor(rep(1:5, each = 4))
  )

  fit <- suppressWarnings(suppressMessages(
    hbm(formula = brms::bf(y ~ x1 + x2),
        data    = d,
        re      = ~(1 | group),
        chains  = 2, iter = 1000, warmup = 500,
        cores   = 1, seed = 42, refresh = 0)
  ))

  # Class checks
  expect_s3_class(fit, "hbmfit")
  expect_true(is.hbmfit(fit))

  # Standard S3 methods
  expect_silent(coef(fit))
  expect_silent(fixef(fit))
  expect_equal(nobs(fit), 20L)
  expect_s3_class(stats::family(fit), "brmsfamily")

  # Predict / fitted
  expect_silent(p <- predict(fit))
  expect_true(is.matrix(p) || is.data.frame(p))

  # Print / summary do not error
  expect_output(print(fit))
  expect_output(summary(fit))
})


test_that("convergence_check returns hbcc_results with rhat_ess", {
  .dev_skip()
  skip_if_no_stan()

  set.seed(123)
  d   <- data.frame(y = stats::rnorm(20), x1 = stats::rnorm(20))
  fit <- suppressWarnings(suppressMessages(
    hbm(formula = brms::bf(y ~ x1), data = d,
        chains = 2, iter = 1000, warmup = 500, cores = 1,
        seed = 1, refresh = 0)
  ))

  diag <- convergence_check(fit, plot_types = character(0L))
  expect_s3_class(diag, "hbcc_results")
  expect_true(is.matrix(diag$rhat_ess))
  expect_true(all(c("Rhat", "Bulk_ESS", "Tail_ESS")
                   %in% colnames(diag$rhat_ess)))

  # is_converged() returns a single logical
  iv <- is_converged(fit)
  expect_true(is.logical(iv) && length(iv) == 1L)
})


test_that("sae_predict returns hbsae_results with valid columns", {
  .dev_skip()
  skip_if_no_stan()

  set.seed(123)
  d   <- data.frame(y = stats::rnorm(20), x1 = stats::rnorm(20))
  fit <- suppressWarnings(suppressMessages(
    hbm(formula = brms::bf(y ~ x1), data = d,
        chains = 2, iter = 1000, warmup = 500, cores = 1,
        seed = 1, refresh = 0)
  ))

  est <- sae_predict(fit)
  expect_s3_class(est, "hbsae_results")
  expect_named(est$result_table, c("Prediction", "SD", "RSE_percent"))
  expect_equal(length(est$pred), 20L)
})
