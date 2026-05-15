# tests/testthat/helper-mocks.R
# =============================================================================
# Helper objects shared across test files.  Loaded automatically by testthat.
# =============================================================================

# Mock hbsae_results object for testing transforms / aggregates without Stan
mock_hbsae_results <- function(n = 10L, seed = 1L) {
  set.seed(seed)
  tbl <- data.frame(
    Prediction  = stats::rnorm(n, 10, 2),
    SD          = stats::runif(n, 0.5, 1.5),
    RSE_percent = stats::runif(n, 2, 8),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      result_table = tbl,
      rse_model    = mean(tbl$RSE_percent),
      pred         = tbl$Prediction
    ),
    class = "hbsae_results"
  )
}

# Skip helper for tests that need Stan
skip_if_no_stan <- function() {
  testthat::skip_if_not_installed("brms")
  testthat::skip_if_not_installed("rstan")
  testthat::skip_on_cran()
}
