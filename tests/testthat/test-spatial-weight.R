# tests/testthat/test-spatial-weight.R
# =============================================================================
# Unit tests for build_spatial_weight().  Skipped when sf or spdep are not
# installed, so this file is CRAN-safe.
# =============================================================================

skip_if_no_spatial <- function() {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("spdep")
}


# Helper: 4-cell grid as a tiny polygon sf object
.fake_grid_sf <- function() {
  skip_if_no_spatial()

  # 2x2 grid of unit squares
  poly <- function(xmin, ymin) {
    sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmin + 1, ymin),
      c(xmin + 1, ymin + 1), c(xmin, ymin + 1),
      c(xmin, ymin)
    )))
  }
  geom <- sf::st_sfc(poly(0,0), poly(1,0), poly(0,1), poly(1,1))
  sf::st_sf(id = c("A", "B", "C", "D"), geometry = geom)
}


# -- input validation --------------------------------------------------------

test_that("build_spatial_weight rejects bogus input", {
  skip_if_no_spatial()
  expect_error(build_spatial_weight(42), "must be a path|sf object")
})

test_that("build_spatial_weight requires sf when given a path", {
  skip_if_no_spatial()
  expect_error(
    build_spatial_weight("/no/such/file.shp"),
    "Shapefile not found"
  )
})

test_that("build_spatial_weight rejects invalid id_col", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  expect_error(
    build_spatial_weight(g, id_col = "no_col"),
    "not found"
  )
})


# -- for_model argument auto-selects type/style ------------------------------

test_that("for_model='car' sets type=queen, style=B", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  M <- build_spatial_weight(g, for_model = "car", validate = FALSE)
  expect_equal(attr(M, "hbsaems_type"),  "queen")
  expect_equal(attr(M, "hbsaems_style"), "B")
  expect_true(all(M %in% c(0, 1)))
})

test_that("for_model='sar' sets type=knn, style=W", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  # 4-cell grid: use k=1 to avoid spdep's "k > n/3" warning while
  # still exercising the knn path
  M <- suppressWarnings(
    build_spatial_weight(g, for_model = "sar", k = 1L,
                          validate = FALSE)
  )
  expect_equal(attr(M, "hbsaems_type"),  "knn")
  expect_equal(attr(M, "hbsaems_style"), "W")
  # Row-standardised: rows sum to 1 (or 0 for isolated)
  rs <- rowSums(M)
  expect_true(all(abs(rs - 1) < 1e-10))
})

test_that("explicit type/style overrides for_model", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  M <- build_spatial_weight(g, for_model = "car",
                              type = "rook", validate = FALSE)
  expect_equal(attr(M, "hbsaems_type"), "rook")
})


# -- validation flag --------------------------------------------------------

test_that("validate=TRUE attaches check object", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  M <- build_spatial_weight(g, for_model = "car", validate = TRUE)
  expect_s3_class(attr(M, "hbsaems_check"), "hbsaems_spatial_check")
  expect_true(attr(M, "hbsaems_check")$compatible)
})

test_that("validate=FALSE skips checking", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  M <- build_spatial_weight(g, for_model = "car", validate = FALSE)
  expect_null(attr(M, "hbsaems_check"))
})


# -- output structure --------------------------------------------------------

test_that("queen contiguity returns square symmetric binary matrix", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  M <- build_spatial_weight(g, type = "queen", style = "B",
                              id_col = "id")
  expect_true(is.matrix(M))
  expect_equal(dim(M), c(4, 4))
  expect_equal(rownames(M), c("A", "B", "C", "D"))
  expect_equal(diag(M), c(A=0, B=0, C=0, D=0))
  expect_true(isSymmetric(M))
  expect_true(all(M %in% c(0, 1)))
})

test_that("rook contiguity matches queen for axis-aligned grid", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  Mq <- build_spatial_weight(g, type = "queen", style = "B")
  Mr <- build_spatial_weight(g, type = "rook",  style = "B")
  # Queen includes corners (diagonals), rook does not.
  expect_true(sum(Mq) >= sum(Mr))
})

test_that("style 'W' gives row-stochastic matrix", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  W <- build_spatial_weight(g, type = "queen", style = "W")
  rs <- rowSums(W)
  # Each row sums to 1 (or 0 for isolated polygons; none here)
  expect_true(all(abs(rs - 1) < 1e-10))
})

test_that("knn works", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  # Suppress spdep's "k > n/3" warning for the toy 4-cell grid
  M <- suppressWarnings(
    build_spatial_weight(g, type = "knn", style = "B", k = 2L)
  )
  expect_equal(dim(M), c(4, 4))
})

test_that("distance-based neighbours work", {
  skip_if_no_spatial()
  g <- .fake_grid_sf()
  M <- build_spatial_weight(g, type = "distance", style = "B",
                              threshold = 2)
  expect_equal(dim(M), c(4, 4))
  # All polygons within distance 2 -> at least some neighbours
  expect_true(sum(M) > 0)
})

test_that("isolated polygon emits warning", {
  skip_if_no_spatial()

  # Far-away 5th polygon
  poly <- function(xmin, ymin) {
    sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmin + 1, ymin),
      c(xmin + 1, ymin + 1), c(xmin, ymin + 1),
      c(xmin, ymin)
    )))
  }
  geom <- sf::st_sfc(poly(0,0), poly(1,0), poly(0,1), poly(1,1),
                     poly(100,100))
  g <- sf::st_sf(id = letters[1:5], geometry = geom)

  # An isolated polygon triggers THREE warnings:
  #   1. spdep's "neighbour object has N sub-graphs" message
  #   2. hbsaems's "X area(s) have no neighbours"  <-- the one we test
  #   3. hbsaems's "Graph has N connected components"
  # Capture them all together so none leaks out of the test.
  warnings_captured <- character(0)
  withCallingHandlers(
    build_spatial_weight(g, type = "queen", style = "B"),
    warning = function(w) {
      warnings_captured <<- c(warnings_captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # The diagnostic we care about: "no neighbours" warning fires
  expect_true(any(grepl("no neighbours", warnings_captured)),
              info = paste("Captured warnings:",
                            paste(warnings_captured, collapse = " | ")))
})
