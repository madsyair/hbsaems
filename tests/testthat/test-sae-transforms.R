# tests/testthat/test-sae-transforms.R
# =============================================================================
# SAE post-processing utilities -- all pure-R, no Stan dependency.
# =============================================================================

# -- sae_transform -------------------------------------------------------------

test_that("sae_transform applies fun element-wise", {
  x  <- mock_hbsae_results(5L)
  sq <- sae_transform(x, sqrt)
  expect_equal(sq$pred, sqrt(x$pred), tolerance = 1e-12)
  expect_s3_class(sq, "hbsae_results")
  expect_equal(sq$result_table$Prediction, sqrt(x$pred), tolerance = 1e-12)
})

test_that("sae_transform passes additional args to fun", {
  x <- mock_hbsae_results(5L)
  r <- sae_transform(x, round, digits = 0L)
  expect_equal(r$pred, round(x$pred, 0L))
})

test_that("sae_transform errors for non-function fun", {
  x <- mock_hbsae_results(5L)
  expect_error(sae_transform(x, "log"),  "must be a function")
  expect_error(sae_transform(x, NULL),   "must be a function")
})


# -- sae_scale -----------------------------------------------------------------

test_that("sae_scale standardises to mean = 0, sd = 1", {
  x  <- mock_hbsae_results(20L, 7L)
  sc <- sae_scale(x)
  expect_equal(mean(sc$pred), 0, tolerance = 1e-10)
  expect_equal(stats::sd(sc$pred), 1, tolerance = 1e-10)
})

test_that("sae_scale keeps result_table consistent with pred", {
  x  <- mock_hbsae_results(10L)
  sc <- sae_scale(x)
  expect_equal(sc$pred, sc$result_table$Prediction, tolerance = 1e-12)
})

test_that("sae_scale warns when all predictions are identical", {
  x      <- mock_hbsae_results(5L)
  x$pred <- rep(7, 5L)
  x$result_table$Prediction <- rep(7, 5L)
  expect_warning(sae_scale(x), "identical")
})


# -- sae_filter ----------------------------------------------------------------

test_that("sae_filter keeps the right rows", {
  x    <- mock_hbsae_results(10L)
  cond <- x$pred > stats::median(x$pred)
  filt <- sae_filter(x, cond)
  expect_equal(nrow(filt$result_table), sum(cond))
  expect_true(all(filt$pred > stats::median(x$pred)))
})

test_that("sae_filter errors when no rows match", {
  x <- mock_hbsae_results(5L)
  expect_error(sae_filter(x, rep(FALSE, 5L)), "No areas match")
})

test_that("sae_filter errors for non-logical condition", {
  x <- mock_hbsae_results(5L)
  expect_error(sae_filter(x, c(1, 0, 1, 0, 1)), "logical vector")
})

test_that("sae_filter errors for length mismatch", {
  x <- mock_hbsae_results(5L)
  expect_error(sae_filter(x, c(TRUE, FALSE)), "must equal")
})


# -- sae_aggregate -------------------------------------------------------------

test_that("sae_aggregate mean is correct", {
  p1  <- mock_hbsae_results(5L, 1L)
  p2  <- mock_hbsae_results(5L, 2L)
  agg <- sae_aggregate(p1, p2, method = "mean")
  expect_equal(agg$pred, (p1$pred + p2$pred) / 2, tolerance = 1e-12)
  expect_s3_class(agg, "hbsae_results")
})

test_that("sae_aggregate median is correct", {
  p1  <- mock_hbsae_results(5L, 1L)
  p2  <- mock_hbsae_results(5L, 2L)
  p3  <- mock_hbsae_results(5L, 3L)
  agg <- sae_aggregate(p1, p2, p3, method = "median")
  mat <- cbind(p1$pred, p2$pred, p3$pred)
  expect_equal(agg$pred, apply(mat, 1L, stats::median), tolerance = 1e-12)
})

test_that("sae_aggregate weighted is correct", {
  p1  <- mock_hbsae_results(5L, 3L)
  p2  <- mock_hbsae_results(5L, 4L)
  agg <- sae_aggregate(p1, p2, method = "weighted",
                       weights = c(0.7, 0.3))
  expect_equal(agg$pred, p1$pred * 0.7 + p2$pred * 0.3, tolerance = 1e-12)
})

test_that("sae_aggregate normalises weights", {
  p1  <- mock_hbsae_results(5L, 3L)
  p2  <- mock_hbsae_results(5L, 4L)
  # Same as 0.7/0.3 after normalisation
  agg <- sae_aggregate(p1, p2, method = "weighted", weights = c(7, 3))
  expect_equal(agg$pred, p1$pred * 0.7 + p2$pred * 0.3, tolerance = 1e-12)
})

test_that("sae_aggregate errors for mismatched lengths", {
  p1 <- mock_hbsae_results(5L); p2 <- mock_hbsae_results(3L)
  expect_error(sae_aggregate(p1, p2), "same number of areas")
})

test_that("sae_aggregate errors when fewer than two objects supplied", {
  p1 <- mock_hbsae_results(5L)
  expect_error(sae_aggregate(p1), "at least two")
})

test_that("sae_aggregate weighted requires weights argument", {
  p1 <- mock_hbsae_results(5L); p2 <- mock_hbsae_results(5L)
  expect_error(sae_aggregate(p1, p2, method = "weighted"),
               "'weights' is required")
})

test_that("sae_aggregate rejects negative weights", {
  p1 <- mock_hbsae_results(5L); p2 <- mock_hbsae_results(5L)
  expect_error(sae_aggregate(p1, p2, method = "weighted",
                             weights = c(-0.5, 1.5)),
               "non-negative")
})


# -- print / summary / as.data.frame -------------------------------------------

test_that("print and summary produce output", {
  x <- mock_hbsae_results(8L)
  expect_output(print(x))
  expect_output(summary(x))
  expect_invisible(print(x))
})

test_that("as.data.frame.hbsae_results returns the result_table", {
  x  <- mock_hbsae_results(6L)
  df <- as.data.frame(x)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 6L)
  expect_true(all(c("Prediction", "RSE_percent") %in% names(df)))
})
