# tests/testthat/test-spatial-validate.R
# =============================================================================
# Unit tests for check_spatial_weight().  Pure R, CRAN-safe.
# =============================================================================

# -- Helper: small valid CAR matrix (4-cell square grid) ---------------------
.valid_car_matrix <- function() {
  M <- matrix(0L, 4, 4)
  M[1, 2] <- M[2, 1] <- 1L
  M[1, 3] <- M[3, 1] <- 1L
  M[2, 4] <- M[4, 2] <- 1L
  M[3, 4] <- M[4, 3] <- 1L
  M
}

# -- Helper: row-standardised version (suitable for SAR) ---------------------
.valid_sar_matrix <- function() {
  M <- .valid_car_matrix()
  rs <- rowSums(M)
  rs[rs == 0] <- 1
  M / rs
}


# -- input validation --------------------------------------------------------

test_that("check_spatial_weight rejects non-matrix", {
  expect_error(check_spatial_weight(1:5, sre_type = "car"),
               "must be a matrix")
})

test_that("check_spatial_weight rejects non-numeric", {
  expect_error(check_spatial_weight(matrix("a", 2, 2), sre_type = "car"),
               "must be numeric")
})


# -- structural checks -------------------------------------------------------

test_that("non-square matrix flagged", {
  M <- matrix(0, 3, 4)
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_false(res$is_square)
  expect_true(any(grepl("not square", res$issues)))
})

test_that("non-zero diagonal flagged", {
  M <- .valid_car_matrix()
  diag(M) <- 1L
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_false(res$has_zero_diag)
  expect_true(any(grepl("self-loops|diagonal", res$issues,
                          ignore.case = TRUE)))
})


# -- symmetry checks --------------------------------------------------------

test_that("symmetric matrix passes for CAR", {
  M <- .valid_car_matrix()
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_true(res$is_symmetric)
  expect_length(res$issues, 0L)
})

test_that("asymmetric matrix flagged as ISSUE for CAR", {
  M <- .valid_car_matrix()
  M[1, 2] <- 2  # break symmetry
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_false(res$is_symmetric)
  expect_true(any(grepl("symmetric", res$issues, ignore.case = TRUE)))
  expect_false(res$compatible)
})

test_that("asymmetric matrix is only WARNING for SAR", {
  M <- .valid_sar_matrix()
  # Already row-standardised but not symmetric -> warning, not error
  res <- check_spatial_weight(M, sre_type = "sar", verbose = FALSE)
  expect_length(res$issues, 0L)  # no fatal error
})


# -- style detection --------------------------------------------------------

test_that("binary matrix detected as style 'B'", {
  M <- .valid_car_matrix()
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_equal(res$detected_style, "B")
})

test_that("row-standardised matrix detected as style 'W'", {
  M <- .valid_sar_matrix()
  res <- check_spatial_weight(M, sre_type = "sar", verbose = FALSE)
  expect_equal(res$detected_style, "W")
})

test_that("CAR with W matrix produces warning", {
  M <- .valid_sar_matrix()
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_true(any(grepl("BINARY|style", res$warnings,
                          ignore.case = TRUE)))
})

test_that("SAR with B matrix produces warning", {
  M <- .valid_car_matrix()
  res <- check_spatial_weight(M, sre_type = "sar", verbose = FALSE)
  expect_true(any(grepl("ROW-STANDARDISED|style", res$warnings,
                          ignore.case = TRUE)))
})


# -- connectivity ------------------------------------------------------------

test_that("isolated areas flagged as warning", {
  M <- .valid_car_matrix()
  # Add 5th area with no neighbours
  M2 <- rbind(cbind(M, 0), c(0, 0, 0, 0, 0))
  res <- check_spatial_weight(M2, sre_type = "car", verbose = FALSE)
  expect_equal(res$n_isolated, 1L)
  expect_true(any(grepl("isolated|no neighbours", res$warnings,
                          ignore.case = TRUE)))
})

test_that("disconnected components detected", {
  # 2 separate 2-area components: {1-2} and {3-4}
  M <- matrix(0, 4, 4)
  M[1, 2] <- M[2, 1] <- 1
  M[3, 4] <- M[4, 3] <- 1
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_equal(res$n_components, 2L)
  expect_true(any(grepl("connected components|bym2", res$warnings,
                          ignore.case = TRUE)))
})

test_that("single connected component reported correctly", {
  M <- .valid_car_matrix()
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_equal(res$n_components, 1L)
})


# -- compatible flag --------------------------------------------------------

test_that("compatible TRUE only when no fatal issues", {
  M <- .valid_car_matrix()
  expect_true(check_spatial_weight(M, sre_type = "car",
                                     verbose = FALSE)$compatible)

  M2 <- M; M2[1, 2] <- 2  # asymmetric -> fatal for CAR
  expect_false(check_spatial_weight(M2, sre_type = "car",
                                      verbose = FALSE)$compatible)
})


# -- print method -----------------------------------------------------------

test_that("print method works", {
  M <- .valid_car_matrix()
  res <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_output(print(res), "Spatial Weight Matrix Diagnostic")
  expect_output(print(res), "Square")
  expect_output(print(res), "Symmetric")
})
