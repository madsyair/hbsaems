# tests/testthat/test-data-integrity.R
# =============================================================================
# Data-integrity sanity tests for the bundled .rda datasets.
#
# These catch regressions where:
#   * a data column gets dropped during a data-raw regeneration
#   * an adjacency matrix loses its row/column names
#   * a documented pairing (e.g. data_binlogitnorm <-> adjacency_matrix_car_regency)
#     stops being satisfied because someone bumped the levels of one but not
#     the other
#
# v1.0.0
# =============================================================================

test_that("data_fhnorm: regency-level, all complete, finite", {
  data("data_fhnorm", package = "hbsaems")
  expect_equal(nrow(data_fhnorm), 100L)
  required_cols <- c("y", "D", "x1", "x2", "x3",
                      "theta_true", "u", "regency", "province")
  expect_true(all(required_cols %in% names(data_fhnorm)))
  expect_false(any(is.na(data_fhnorm)))
  expect_true(all(is.finite(data_fhnorm$y)))
  expect_true(all(is.finite(data_fhnorm$D)))
  expect_true(all(data_fhnorm$D > 0))
  expect_equal(length(unique(data_fhnorm$regency)), 100L)
  expect_equal(length(unique(data_fhnorm$province)), 5L)
})

test_that("data_betalogitnorm: regency-level, all complete, y in (0,1)", {
  data("data_betalogitnorm", package = "hbsaems")
  expect_equal(nrow(data_betalogitnorm), 100L)
  required_cols <- c("y", "theta", "x1", "x2", "x3",
                      "n", "deff", "regency", "province")
  expect_true(all(required_cols %in% names(data_betalogitnorm)))
  expect_false(any(is.na(data_betalogitnorm)))
  expect_true(all(data_betalogitnorm$y > 0 & data_betalogitnorm$y < 1))
  expect_true(all(data_betalogitnorm$n > 0))
  expect_true(all(data_betalogitnorm$deff >= 1))
  expect_equal(length(unique(data_betalogitnorm$regency)), 100L)
  expect_equal(length(unique(data_betalogitnorm$province)), 5L)
})

test_that("data_binlogitnorm: district-level, regency = coarse spatial cluster", {
  data("data_binlogitnorm", package = "hbsaems")
  expect_equal(nrow(data_binlogitnorm), 100L)
  expect_true("district" %in% names(data_binlogitnorm))
  expect_true("regency"  %in% names(data_binlogitnorm))
  # Note: in this dataset "regency" is the COARSE level (5 counties),
  # whereas in data_fhnorm / data_betalogitnorm the analogous role is
  # played by "province".  Documented in R/data.R top comment.
  expect_equal(length(unique(data_binlogitnorm$district)), 100L)
  expect_equal(length(unique(data_binlogitnorm$regency)), 5L)
  expect_true(all(data_binlogitnorm$y <= data_binlogitnorm$n))
})

test_that("data_lnln: district-level, log-scale response", {
  data("data_lnln", package = "hbsaems")
  expect_equal(nrow(data_lnln), 100L)
  expect_true("district" %in% names(data_lnln))
  expect_true("regency"  %in% names(data_lnln))
  expect_true("psi_i"    %in% names(data_lnln))
  expect_true(all(data_lnln$psi_i > 0))
  expect_equal(length(unique(data_lnln$district)), 100L)
  expect_equal(length(unique(data_lnln$regency)), 5L)
})

test_that("adjacency_matrix_car: 5x5 province-level, binary, symmetric", {
  data("adjacency_matrix_car", package = "hbsaems")
  expect_true(is.matrix(adjacency_matrix_car))
  expect_equal(dim(adjacency_matrix_car), c(5L, 5L))
  expect_true(all(adjacency_matrix_car %in% c(0, 1)))
  expect_true(isSymmetric(adjacency_matrix_car))
  expect_true(all(diag(adjacency_matrix_car) == 0))
  expect_true(all(grepl("^province_", rownames(adjacency_matrix_car))))
})

test_that("adjacency_matrix_car_regency: 5x5 binary, pairs with data_binlogitnorm/lnln", {
  data("adjacency_matrix_car_regency", package = "hbsaems")
  data("data_binlogitnorm", package = "hbsaems")

  expect_true(is.matrix(adjacency_matrix_car_regency))
  expect_equal(dim(adjacency_matrix_car_regency), c(5L, 5L))
  expect_true(all(adjacency_matrix_car_regency %in% c(0, 1)))
  expect_true(isSymmetric(adjacency_matrix_car_regency))

  # CRITICAL: documented pairing must hold -- the matrix row names must
  # match data_binlogitnorm.s regency levels.
  expect_setequal(rownames(adjacency_matrix_car_regency),
                   unique(data_binlogitnorm$regency))
})

test_that("spatial_weight_sar: 100x100, row-standardised", {
  data("spatial_weight_sar", package = "hbsaems")
  expect_true(is.matrix(spatial_weight_sar))
  expect_equal(dim(spatial_weight_sar), c(100L, 100L))
  expect_true(all(diag(spatial_weight_sar) == 0))
  # Row sums should be ~1 (row-standardised) for non-isolated rows
  rs <- rowSums(spatial_weight_sar)
  expect_true(all(rs >= 0 & rs <= 1.01))
})

test_that("province adjacency pairs with regency-level datasets", {
  data("adjacency_matrix_car", package = "hbsaems")
  data("data_fhnorm", package = "hbsaems")
  data("data_betalogitnorm", package = "hbsaems")

  expect_setequal(rownames(adjacency_matrix_car),
                   unique(data_fhnorm$province))
  expect_setequal(rownames(adjacency_matrix_car),
                   unique(data_betalogitnorm$province))
})
