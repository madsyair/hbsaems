# tests/testthat/test-class-predicates.R
# =============================================================================
# Class-membership predicates.  All CRAN-safe (no Stan).
# =============================================================================

test_that("is.hbsae_results identifies the right objects", {
  x <- mock_hbsae_results()
  expect_true(is.hbsae_results(x))
  expect_false(is.hbsae_results("string"))
  expect_false(is.hbsae_results(NULL))
  expect_false(is.hbsae_results(data.frame(x = 1)))
})

test_that("is.hbmfit returns FALSE for non-hbmfit objects", {
  expect_false(is.hbmfit(NULL))
  expect_false(is.hbmfit(list(model = 1)))
  expect_false(is.hbmfit(mock_hbsae_results()))
})

test_that("is.hbcc/hbmc/hbpc_results work on bare structures", {
  expect_true(is.hbcc_results(structure(list(), class = "hbcc_results")))
  expect_true(is.hbmc_results(structure(list(), class = "hbmc_results")))
  expect_true(is.hbpc_results(structure(list(), class = "hbpc_results")))
  expect_false(is.hbcc_results(list()))
})
