# tests/testthat/test-sae-benchmark.R
# =============================================================================
# Unit tests for sae_benchmark().  Pure-R; CRAN-safe.
#
# v1.0.0 semantics
# -----------------
# When `weights = NULL`, the chosen default depends on `target_type`:
#   * target_type = "total" (default) -> weights = rep(1, n)
#     so that sum(weights * pred) = sum(pred), the population total.
#   * target_type = "mean"            -> weights = rep(1 / n, n)
#     so that sum(weights * pred) = mean(pred).
#
# An informational message is emitted whenever the default is used;
# wrap calls with suppressMessages() in tests so the message does not
# pollute testthat output.
# =============================================================================

# -- input validation --------------------------------------------------------

test_that("sae_benchmark rejects non-hbsae_results", {
  expect_error(sae_benchmark("nope", target = 10),
               "hbsae_results")
})

test_that("sae_benchmark rejects non-numeric target", {
  p <- mock_hbsae_results(5L)
  expect_error(sae_benchmark(p, target = "ten"), "numeric")
})

test_that("sae_benchmark rejects wrong-length weights", {
  p <- mock_hbsae_results(5L)
  expect_error(sae_benchmark(p, target = 10, weights = c(1, 2, 3)),
               "length")
})

test_that("sae_benchmark rejects negative weights", {
  p <- mock_hbsae_results(5L)
  expect_error(sae_benchmark(p, target = 10,
                              weights = c(1, -1, 1, 1, 1)),
               "non-negative")
})

test_that("sae_benchmark rejects vector target for ratio/difference", {
  p <- mock_hbsae_results(5L)
  expect_error(sae_benchmark(p, target = c(10, 20),
                              method = "ratio"),
               "single")
})

test_that("sae_benchmark requires groups for raking", {
  p <- mock_hbsae_results(5L)
  expect_error(sae_benchmark(p, target = c(10, 20), method = "raking"),
               "groups")
})


# -- ratio benchmark --------------------------------------------------------

test_that("ratio benchmark matches target (default = total semantics)", {
  p <- mock_hbsae_results(5L, seed = 1L)
  T_total <- 100
  # Default target_type = "total"; weights default to rep(1, n).
  bm <- suppressMessages(
    sae_benchmark(p, target = T_total, method = "ratio")
  )
  # sum(pred) should equal target after benchmarking
  expect_equal(sum(bm$pred), T_total, tolerance = 1e-10)
  expect_s3_class(bm, "hbsae_results")
  expect_equal(bm$benchmark_info$method, "ratio")
})

test_that("ratio benchmark matches target (mean semantics, explicit)", {
  p <- mock_hbsae_results(5L, seed = 1L)
  T_mean <- 100
  # Explicit `target_type = "mean"` preserves the pre-v1.0.0 default
  bm <- suppressMessages(
    sae_benchmark(p, target = T_mean, method = "ratio",
                   target_type = "mean")
  )
  ws <- sum((1 / 5) * bm$pred)
  expect_equal(ws, T_mean, tolerance = 1e-10)
})

test_that("ratio benchmark preserves relative ordering", {
  p <- mock_hbsae_results(8L, seed = 42L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio")
  )
  expect_equal(order(bm$pred), order(p$pred))
})

test_that("ratio benchmark errors when sum is zero", {
  p <- mock_hbsae_results(4L)
  p$pred[] <- 0
  p$result_table$Prediction <- 0
  expect_error(
    suppressMessages(sae_benchmark(p, target = 1, method = "ratio")),
    "undefined"
  )
})


# -- difference benchmark ---------------------------------------------------

test_that("difference benchmark matches target", {
  p <- mock_hbsae_results(5L, seed = 1L)
  pop <- c(0.2, 0.3, 0.1, 0.25, 0.15)
  T_total <- 12
  bm <- sae_benchmark(p, target = T_total, weights = pop,
                       method = "difference")
  expect_equal(sum(pop * bm$pred), T_total, tolerance = 1e-10)
})

test_that("difference benchmark preserves spacing", {
  p <- mock_hbsae_results(5L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 30, method = "difference")
  )
  expect_equal(diff(bm$pred), diff(p$pred), tolerance = 1e-12)
})


# -- raking ----------------------------------------------------------------

test_that("raking with single group equals ratio benchmark", {
  p <- mock_hbsae_results(6L, seed = 7L)
  bm_r <- suppressMessages(
    sae_benchmark(p, target = 60, method = "ratio")
  )
  bm_k <- suppressMessages(
    sae_benchmark(p, target = 60, method = "raking",
                   groups = rep(1L, 6L))
  )
  expect_equal(bm_r$pred, bm_k$pred, tolerance = 1e-8)
})

test_that("raking preserves group totals", {
  p <- mock_hbsae_results(6L, seed = 9L)
  groups  <- c(1, 1, 1, 2, 2, 2)
  weights <- rep(1 / 6, 6)
  targets <- c(15, 25)
  bm <- sae_benchmark(p, target = targets, weights = weights,
                       method = "raking", groups = groups)
  expect_true(bm$benchmark_info$converged)
  for (g in 1:2) {
    idx <- which(groups == g)
    expect_equal(sum(weights[idx] * bm$pred[idx]), targets[g],
                 tolerance = 1e-6)
  }
})

test_that("raking errors when target length != number of groups", {
  p <- mock_hbsae_results(6L)
  expect_error(
    sae_benchmark(p, target = c(10, 20, 30),
                   method = "raking",
                   groups = c(1, 1, 2, 2, 1, 2)),
    "must equal number of unique groups"
  )
})


# -- benchmark_info structure ----------------------------------------------

test_that("benchmark_info contains expected fields", {
  p <- mock_hbsae_results(5L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio")
  )
  expect_true(all(c("method", "target", "weights",
                     "converged", "adjustment") %in%
                    names(bm$benchmark_info)))
})


# -- target_type semantics (v1.0.0) ----------------------------------------

test_that("target_type = 'total' produces weights = rep(1, n)", {
  p <- mock_hbsae_results(5L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 100, method = "ratio",
                   target_type = "total")
  )
  expect_equal(bm$benchmark_info$weights, rep(1, 5))
})

test_that("target_type = 'mean' produces weights = rep(1/n, n)", {
  p <- mock_hbsae_results(5L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 100, method = "ratio",
                   target_type = "mean")
  )
  expect_equal(bm$benchmark_info$weights, rep(1 / 5, 5))
})

test_that("explicit weights override target_type", {
  p <- mock_hbsae_results(5L)
  user_w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
  bm <- sae_benchmark(p, target = 100, method = "ratio",
                       weights = user_w,
                       target_type = "mean")    # should be ignored
  expect_equal(bm$benchmark_info$weights, user_w)
})

test_that("default weighting emits an informational message", {
  p <- mock_hbsae_results(5L)
  expect_message(
    sae_benchmark(p, target = 100, method = "ratio"),
    "default weights"
  )
})


# =============================================================================
# v1.0.0: Fully Bayesian benchmarking tests
# =============================================================================

# Helper: build hbsae_results with synthetic posterior draws
.fake_hbsae_with_draws <- function(n_areas = 4L, n_draws = 500L,
                                    means = NULL, sd = 1, seed = 1L) {
  set.seed(seed)
  if (is.null(means)) means <- seq(8, by = 1, length.out = n_areas)
  draws <- matrix(stats::rnorm(n_draws * n_areas,
                                mean = rep(means, each = n_draws),
                                sd = sd),
                  nrow = n_draws, ncol = n_areas)
  pred <- colMeans(draws)
  structure(
    list(
      result_table = data.frame(
        Prediction  = pred,
        SD          = apply(draws, 2L, stats::sd),
        RSE_percent = 100 * apply(draws, 2L, stats::sd) / abs(pred)
      ),
      rse_model        = mean(100 * apply(draws, 2L, stats::sd) / abs(pred)),
      pred             = pred,
      posterior_draws  = draws
    ),
    class = "hbsae_results"
  )
}


test_that("posterior=TRUE auto-extracts draws from hbsae_results", {
  p <- .fake_hbsae_with_draws(4L, 300L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio",
                   posterior = TRUE)
  )
  expect_s3_class(bm, "hbsae_results")
  expect_true(bm$benchmark_info$posterior_used)
  expect_equal(bm$benchmark_info$n_draws, 300L)
  expect_true(!is.null(bm$posterior_adjusted))
  expect_equal(dim(bm$posterior_adjusted), c(300L, 4L))
})

test_that("posterior=TRUE without draws raises informative error", {
  p <- structure(
    list(
      result_table = data.frame(Prediction = 1:4,
                                 SD = 1, RSE_percent = 25),
      pred = 1:4
    ),
    class = "hbsae_results"
  )
  expect_error(
    suppressMessages(
      sae_benchmark(p, target = 10, method = "ratio", posterior = TRUE)
    ),
    "Could not extract"
  )
})

test_that("posterior matrix supplied directly works", {
  p <- .fake_hbsae_with_draws(4L, 200L)
  user_draws <- matrix(rnorm(200 * 4, mean = c(10, 12, 9, 11), sd = 1),
                        nrow = 200, byrow = TRUE)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio",
                   posterior = user_draws)
  )
  expect_true(bm$benchmark_info$posterior_used)
  expect_equal(bm$benchmark_info$n_draws, 200L)
})

test_that("posterior with wrong number of columns errors", {
  p <- .fake_hbsae_with_draws(4L)
  bad_ncol <- matrix(rnorm(15), nrow = 5, ncol = 3)
  expect_error(
    suppressMessages(
      sae_benchmark(p, target = 50, method = "ratio",
                     posterior = bad_ncol)
    ),
    "columns"
  )
})

test_that("Bayesian ratio: SD scaled (not just by |r|) due to stochastic r", {
  p <- .fake_hbsae_with_draws(n_areas = 4L, n_draws = 5000L,
                               means = c(10, 12, 9, 11),
                               sd = 1, seed = 42L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio",
                   posterior = TRUE)
  )
  r <- bm$benchmark_info$adjustment
  expect_true(abs(r) > 0.5)

  # SD should be within reasonable factor of |r| * pre-SD
  expected_lo <- 0.1 * abs(r) * p$result_table$SD
  expected_hi <- 5.0 * abs(r) * p$result_table$SD
  expect_true(all(bm$result_table$SD > expected_lo))
  expect_true(all(bm$result_table$SD < expected_hi))
})

test_that("Bayesian ratio: variance REDUCED vs deterministic-r prediction", {
  p <- .fake_hbsae_with_draws(n_areas = 4L, n_draws = 5000L,
                               means = c(10, 12, 9, 11),
                               sd = 1, seed = 42L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio",
                   posterior = TRUE)
  )
  r <- bm$benchmark_info$adjustment
  expect_true(mean(bm$result_table$SD) <
                mean(abs(r) * p$result_table$SD))
})

test_that("Bayesian difference: SD changes only modestly", {
  p <- .fake_hbsae_with_draws(n_areas = 4L, n_draws = 5000L,
                               means = c(10, 12, 9, 11),
                               sd = 1, seed = 42L)
  bm <- sae_benchmark(p, target = 50, method = "difference",
                       posterior = TRUE,
                       weights = rep(0.25, 4))
  expect_equal(bm$result_table$SD, p$result_table$SD, tolerance = 0.3)
})

test_that("Bayesian mode adds quantile columns named by probs", {
  p <- .fake_hbsae_with_draws(4L)
  bm <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio",
                   posterior = TRUE,
                   probs = c(0.05, 0.5, 0.95))
  )
  expect_true("Q5"  %in% names(bm$result_table))
  expect_true("Q50" %in% names(bm$result_table))
  expect_true("Q95" %in% names(bm$result_table))
})

test_that("Bayesian raking preserves group totals on the POINT estimate", {
  p <- .fake_hbsae_with_draws(6L, 500L,
                               means = c(8, 10, 9, 11, 12, 10))
  groups <- c(1, 1, 1, 2, 2, 2)
  wts    <- rep(1 / 6, 6)
  targets <- c(15, 25)
  bm <- sae_benchmark(p, target = targets, method = "raking",
                       weights = wts, groups = groups,
                       posterior = TRUE)
  expect_true(bm$benchmark_info$converged)
  for (g in 1:2) {
    idx <- which(groups == g)
    expect_equal(sum(wts[idx] * bm$pred[idx]),
                 targets[g], tolerance = 1e-6)
  }
})

test_that("point-estimate mode (posterior=NULL) preserves old behaviour", {
  p <- .fake_hbsae_with_draws(4L)
  bm_pt <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio")
  )
  expect_false(bm_pt$benchmark_info$posterior_used)
  expect_equal(bm_pt$result_table$SD, p$result_table$SD)
})

test_that("posterior=FALSE explicitly equals default mode", {
  p <- .fake_hbsae_with_draws(4L)
  bm_a <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio")
  )
  bm_b <- suppressMessages(
    sae_benchmark(p, target = 50, method = "ratio",
                   posterior = FALSE)
  )
  expect_equal(bm_a$pred, bm_b$pred)
  expect_equal(bm_a$result_table$SD, bm_b$result_table$SD)
})
