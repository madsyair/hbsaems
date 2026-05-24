# tests/testthat/test-update-hbm.R
# =============================================================================
# Regression tests for update_hbm()'s auto-fallback behaviour when the
# user supplies a new formula introducing variables not in the original
# model frame.
# =============================================================================

test_that("update_hbm rejects non-hbmfit input", {
  expect_error(
    update_hbm(list(), iter = 100),
    "must be an hbmfit"
  )
})

test_that("update_hbm auto-fallback catches 'New variables found'", {
  # Build a fake hbmfit with a brmsfit-like object
  fake_data  <- data.frame(y = 1:10, x1 = 1:10, x2 = rnorm(10),
                            group = rep(1:5, 2))
  fake_hbmfit <- structure(
    list(model          = structure(list(), class = "brmsfit"),
         data           = fake_data,
         missing_method = "none"),
    class = "hbmfit"
  )

  # Stub update.brmsfit in the brms namespace to simulate the brms error
  # on first call without newdata, and succeed on second call with newdata.
  orig_update <- getFromNamespace("update.brmsfit", "brms")
  on.exit(
    assignInNamespace("update.brmsfit", orig_update, ns = "brms"),
    add = TRUE
  )

  call_count <- 0L
  fake_update <- function(object, ...) {
    call_count <<- call_count + 1L
    args <- list(...)
    if (call_count == 1L && is.null(args$newdata))
      stop("New variables found: 'x2'. ",
           "Please supply your data again via argument 'newdata'.",
           call. = FALSE)
    structure(list(), class = "brmsfit")
  }
  assignInNamespace("update.brmsfit", fake_update, ns = "brms")

  # Run and verify auto-fallback triggered the second call
  res <- update_hbm(fake_hbmfit, formula. = . ~ . + x2)
  expect_s3_class(res, "hbmfit")
  expect_equal(call_count, 2L)   # one failed + one successful retry
})

test_that("update_hbm forwards explicit newdata without retry", {
  fake_data <- data.frame(y = 1:10, x1 = 1:10, x2 = rnorm(10),
                           group = rep(1:5, 2))
  fake_hbmfit <- structure(
    list(model          = structure(list(), class = "brmsfit"),
         data           = fake_data,
         missing_method = "none"),
    class = "hbmfit"
  )

  orig_update <- getFromNamespace("update.brmsfit", "brms")
  on.exit(
    assignInNamespace("update.brmsfit", orig_update, ns = "brms"),
    add = TRUE
  )

  call_count <- 0L
  newdata_passed <- FALSE
  fake_update <- function(object, ...) {
    call_count <<- call_count + 1L
    args <- list(...)
    newdata_passed <<- !is.null(args$newdata)
    structure(list(), class = "brmsfit")
  }
  assignInNamespace("update.brmsfit", fake_update, ns = "brms")

  res <- update_hbm(fake_hbmfit, formula. = . ~ . + x2,
                     newdata = fake_data)
  expect_s3_class(res, "hbmfit")
  expect_equal(call_count, 1L)    # no retry
  expect_true(newdata_passed)
})

test_that("update_hbm preserves non-New-variables errors", {
  fake_hbmfit <- structure(
    list(model          = structure(list(), class = "brmsfit"),
         data           = data.frame(y = 1:5, x1 = 1:5),
         missing_method = "none"),
    class = "hbmfit"
  )

  orig_update <- getFromNamespace("update.brmsfit", "brms")
  on.exit(
    assignInNamespace("update.brmsfit", orig_update, ns = "brms"),
    add = TRUE
  )

  fake_update <- function(object, ...) {
    stop("Some other brms error message.", call. = FALSE)
  }
  assignInNamespace("update.brmsfit", fake_update, ns = "brms")

  expect_error(
    update_hbm(fake_hbmfit, iter = 100),
    "Some other brms error"
  )
})
