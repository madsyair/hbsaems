# tests/testthat/helper-mocks.R
# =============================================================================
# Helper objects shared across test files.  Loaded automatically by testthat.
#
# Provides mock objects so most test_that() blocks can exercise the
# package logic WITHOUT going through a real Stan compile or MCMC run.
# Each helper documents its scope and limits.
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

# ----------------------------------------------------------------------------
# .brm_stub() / .hbm_stub()
#
# Lightweight stand-ins for brms::brm() and hbsaems::hbm() that skip the
# Stan compile and the MCMC.  They return an object structured well
# enough for downstream tests that only need to inspect the formula,
# data, and family resolution.
#
# Use via testthat::local_mocked_bindings():
#
#   test_that("hbm() resolves family correctly", {
#     testthat::local_mocked_bindings(
#       brm = .brm_stub, .package = "brms"
#     )
#     fit <- hbm(...)
#     expect_equal(fit$model$family$family, "gaussian")
#   })
#
# The stub honours the most common args used inside hbsaems:
# `formula`, `data`, `family`, `prior`, `stanvars`, `empty = TRUE`.
# Other args are accepted and silently discarded.
# ----------------------------------------------------------------------------
.brm_stub <- function(formula = NULL, data = NULL, family = NULL,
                      prior = NULL, stanvars = NULL, empty = TRUE,
                      ...) {
  if (is.null(family)) family <- list(family = "gaussian", link = "identity")
  out <- list(
    formula  = formula,
    data     = data,
    family   = family,
    prior    = prior,
    stanvars = stanvars,
    fit      = NULL,        # no Stan run
    .stubbed = TRUE
  )
  class(out) <- "brmsfit"
  out
}

.hbm_stub <- function(...) {
  args <- list(...)
  brms_obj <- .brm_stub(formula = args$formula, data = args$data,
                         family = args$family, prior = args$prior,
                         stanvars = args$stanvars)
  out <- list(
    model           = brms_obj,
    formula         = args$formula,
    data            = args$data,
    handle_missing  = args$handle_missing,
    fixed_params    = args$fixed_params,
    .stubbed        = TRUE
  )
  class(out) <- c("hbmfit", "list")
  out
}

