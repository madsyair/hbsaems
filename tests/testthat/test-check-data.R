# tests/testthat/test-check-data.R
# =============================================================================
# Unit tests for check_data().  All CRAN-safe (no Stan).
# =============================================================================

# -- helper: small synthetic dataset -------------------------------------------
.fake_data <- function(n = 20L, seed = 1L) {
  set.seed(seed)
  data.frame(
    y     = rnorm(n, 10, 2),
    x1    = rnorm(n),
    x2    = rnorm(n),
    group = rep(seq_len(n / 4L), each = 4L),
    sre   = rep(seq_len(n / 4L), each = 4L)
  )
}


# -- input validation ---------------------------------------------------------

test_that("check_data rejects non-data.frame", {
  expect_error(check_data(matrix(1, 3, 3), "y", "x1"), "data.frame")
})

test_that("check_data rejects non-character response", {
  d <- .fake_data()
  expect_error(check_data(d, 1L, "x1"), "character")
})

test_that("check_data rejects empty predictors", {
  d <- .fake_data()
  expect_error(check_data(d, "y", character(0)), "non-empty")
})


# -- variable presence -------------------------------------------------------

test_that("check_data reports missing required variables", {
  d <- .fake_data()
  res <- check_data(d, response = "yyy", auxiliary = c("x1", "x2"))
  expect_s3_class(res, "hbsaems_data_check")
  expect_true(length(res$issues) >= 1L)
  expect_true(any(grepl("yyy", res$issues)))
})

test_that("check_data reports missing optional variables", {
  d <- .fake_data()
  res <- check_data(d, "y", c("x1", "x2"), area_var = "no_such_group")
  expect_true(any(grepl("no_such_group", res$issues)))
})


# -- missing patterns --------------------------------------------------------

test_that("check_data detects 'none' pattern", {
  d <- .fake_data()
  res <- check_data(d, "y", c("x1", "x2"))
  expect_equal(res$missing_pattern, "none")
  expect_true(is.na(res$recommended_method))
  expect_null(res$non_sample_warning)
})

test_that("check_data detects 'y_only' pattern + non-sample warning", {
  d <- .fake_data()
  d$y[1:3] <- NA
  res <- check_data(d, "y", c("x1", "x2"))
  expect_equal(res$missing_pattern, "y_only")
  expect_equal(res$recommended_method, "deleted")
  expect_false(is.null(res$non_sample_warning))
  expect_true(grepl("non-sample|NON-SAMPLE", res$non_sample_warning,
                     ignore.case = TRUE))
})

test_that("check_data detects 'x_only' pattern -> 'multiple'", {
  d <- .fake_data()
  d$x1[5:7] <- NA
  res <- check_data(d, "y", c("x1", "x2"))
  expect_equal(res$missing_pattern, "x_only")
  expect_equal(res$recommended_method, "multiple")
})

test_that("check_data detects 'both' for continuous Y -> 'model'", {
  d <- .fake_data()
  d$y[1:2]  <- NA
  d$x1[5:6] <- NA
  res <- check_data(d, "y", c("x1", "x2"))
  expect_equal(res$missing_pattern, "both")
  expect_equal(res$recommended_method, "model")
})

test_that("check_data detects 'both' for binomial -> 'multiple'", {
  d <- .fake_data()
  d$y      <- as.integer(rpois(nrow(d), 5))
  d$n      <- as.integer(rep(10L, nrow(d)))
  d$y[1:2] <- NA_integer_
  d$x1[5]  <- NA
  res <- check_data(d, "y", c("x1", "x2"), trials = "n")
  expect_equal(res$missing_pattern, "both")
  expect_equal(res$recommended_method, "multiple")
  expect_true(grepl("binomial|multiple",
                     res$recommendation_text, ignore.case = TRUE))
})


# -- dimension check ---------------------------------------------------------

test_that("check_data validates square matrix", {
  d <- .fake_data()
  M_bad <- matrix(0, 5, 4)
  res <- check_data(d, "y", c("x1", "x2"), M = M_bad)
  expect_true(any(grepl("square", res$issues)))
})

test_that("check_data detects dimension mismatch with sre", {
  d <- .fake_data()
  M <- matrix(0, 7, 7)  # 7x7 but only 5 unique sre levels
  res <- check_data(d, "y", c("x1", "x2"), spatial_var = "sre", M = M)
  expect_true(any(grepl("Dimension mismatch", res$issues)))
})

test_that("check_data accepts matching dimensions", {
  d <- .fake_data()
  n_unique <- length(unique(d$sre))
  M <- matrix(0, n_unique, n_unique)
  res <- check_data(d, "y", c("x1", "x2"), spatial_var = "sre", M = M)
  expect_length(res$issues, 0L)
  expect_true(res$dimension_check$dim_match)
})


# -- print / summary ---------------------------------------------------------

test_that("print method runs without error", {
  d <- .fake_data()
  d$y[1] <- NA
  res <- check_data(d, "y", c("x1", "x2"))
  expect_output(print(res), "HBSAE Data Check")
  expect_invisible(print(res))
})

test_that("summary method runs without error", {
  d <- .fake_data()
  res <- check_data(d, "y", c("x1", "x2"))
  expect_output(summary(res), "HBSAE Data Check Summary")
})
