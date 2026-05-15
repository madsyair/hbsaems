# tests/testthat/test-s3-generic-dispatch.R
# =============================================================================
# Regression tests for v1.0.0 fix: posterior_interval() and prior_draws()
# now correctly re-export the rstantools / brms generics rather than
# defining their own (which caused a name collision producing the error
# "For the default method 'object' should be a matrix" when brms was
# attached together with hbsaems).
# =============================================================================

test_that("posterior_interval is re-exported from rstantools", {
  testthat::skip_if_not_installed("rstantools")
  expect_identical(
    posterior_interval,
    rstantools::posterior_interval
  )
})

test_that("posterior_interval generic has rstantools signature", {
  args_pi <- names(formals(posterior_interval))
  expect_true("object" %in% args_pi)
  expect_false("model" %in% args_pi)
})

test_that("posterior_interval.hbmfit method is registered", {
  m <- getS3method("posterior_interval", "hbmfit",
                    optional = TRUE)
  expect_false(is.null(m))
  expect_true("object" %in% names(formals(m)))
})

test_that("prior_draws is re-exported from brms", {
  testthat::skip_if_not_installed("brms")
  expect_identical(
    prior_draws,
    brms::prior_draws
  )
})

test_that("prior_draws.hbmfit method is registered", {
  m <- getS3method("prior_draws", "hbmfit", optional = TRUE)
  expect_false(is.null(m))
  expect_true("x" %in% names(formals(m)))
})

test_that("posterior_interval.hbmfit dispatches correctly", {
  # Build a minimal hbmfit-like object + mock posterior_draws
  mock_hbmfit <- structure(
    list(model = list(), formula = y ~ x, family = "gaussian"),
    class = "hbmfit"
  )

  # Replace posterior_draws.hbmfit temporarily
  ns <- asNamespace("hbsaems")
  orig <- get("posterior_draws.hbmfit", envir = ns)
  on.exit(assignInNamespace("posterior_draws.hbmfit", orig, ns = "hbsaems"))

  mock_draws <- function(model, params = NULL, ...) {
    matrix(rnorm(200), ncol = 2, dimnames = list(NULL, c("a", "b")))
  }
  assignInNamespace("posterior_draws.hbmfit", mock_draws, ns = "hbsaems")

  res <- posterior_interval(mock_hbmfit, prob = 0.9)
  expect_true(is.matrix(res))
  expect_equal(rownames(res), c("5%", "95%"))
  expect_equal(colnames(res), c("a", "b"))
})

test_that("posterior_interval.hbmfit validates prob argument", {
  mock_hbmfit <- structure(list(), class = "hbmfit")
  expect_error(
    posterior_interval(mock_hbmfit, prob = 1.5),
    "prob.*must.*\\(0, 1\\)"
  )
  expect_error(
    posterior_interval(mock_hbmfit, prob = c(0.5, 0.9)),
    "single number"
  )
})
