# tests/testthat/test-model-compare-all.R
# =============================================================================
# model_compare_all() and hbm_table -- input validation tests.
# =============================================================================

test_that("model_compare_all requires hbmfit objects", {
  expect_error(model_compare_all("x"), "hbmfit")
  expect_error(model_compare_all(),    "At least one")
})

test_that("model_compare_all rejects mixed types", {
  m  <- structure(list(), class = "hbmfit")
  expect_error(model_compare_all(m, "not_a_model"), "hbmfit")
})

test_that("model_compare_all validates criterion", {
  m1 <- structure(list(), class = "hbmfit")
  expect_error(
    model_compare_all(m1, criterion = "harmonic"),
    "should be one of"
  )
})


test_that("print.hbm_table shows Best model", {
  tbl <- structure(
    data.frame(
      Model    = c("m1", "m2"),
      ELPD_LOO = c(-100, -110),
      LOO_rank = c(1L, 2L),
      stringsAsFactors = FALSE
    ),
    class = c("hbm_table", "data.frame")
  )
  expect_output(print(tbl), "Best model: m1")
})


# model_average input validation

test_that("model_average requires at least two hbmfits", {
  m <- structure(list(), class = "hbmfit")
  expect_error(model_average(m), "at least two")
})

test_that("model_average rejects negative weights", {
  m1 <- structure(list(), class = "hbmfit")
  m2 <- structure(list(), class = "hbmfit")
  expect_error(model_average(m1, m2, weights = c(-0.5, 1.5)),
               "non-negative")
})

test_that("model_average rejects weights of wrong length", {
  m1 <- structure(list(), class = "hbmfit")
  m2 <- structure(list(), class = "hbmfit")
  expect_error(model_average(m1, m2, weights = c(0.3, 0.4, 0.3)),
               "must equal")
})
