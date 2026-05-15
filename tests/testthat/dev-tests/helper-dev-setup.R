# tests/testthat/dev-tests/helper-dev-setup.R
# =============================================================================
# Common setup for development tests (heavy integration tests).
#
# These tests actually fit Stan models via brms.  They are slow (5-90s per
# test) and skipped on CRAN.  They are also EXCLUDED from the package
# tarball via .Rbuildignore (^tests/testthat/dev-tests$).
#
# To run them locally:
#
#   # Option 1: from R, with the package source dir as cwd:
#   Sys.setenv(NOT_CRAN = "true")
#   testthat::test_dir("tests/testthat/dev-tests")
#
#   # Option 2: from the shell, after installing the package:
#   NOT_CRAN=true R -e 'testthat::test_dir("tests/testthat/dev-tests")'
#
#   # Option 3: with devtools:
#   devtools::load_all(); devtools::test(filter = "dev-tests")
#
# Gate variable
# -------------
# Tests are wrapped in `skip_on_cran()` and additionally check
# Sys.getenv("NOT_CRAN") == "true".  If neither is set, the entire suite
# emits a single skip notice and exits.
# =============================================================================

# Lazy guard: refuse to run on CRAN even by accident
.dev_skip <- function() {
  testthat::skip_on_cran()
  if (!identical(Sys.getenv("NOT_CRAN"), "true") &&
      !interactive()) {
    testthat::skip("Set NOT_CRAN=true to run development tests.")
  }
}

# Centralised lightweight Stan defaults: keep iter low so each test
# finishes in a few seconds.  These are NOT production settings -- they
# exercise the code path, not the inference quality.
.dev_stan_args <- list(
  iter    = 100L,
  chains  = 2L,
  warmup  = 50L,
  refresh = 0L
)

# Load datasets that ship with the package (already in data/).
# We rely on package data being on the search path; testthat
# handles this when load_package = "source".
if (requireNamespace("hbsaems", quietly = TRUE)) {
  utils::data("data_fhnorm",          package = "hbsaems", envir = environment())
  utils::data("data_betalogitnorm",   package = "hbsaems", envir = environment())
  utils::data("data_binlogitnorm",    package = "hbsaems", envir = environment())
  utils::data("data_lnln",            package = "hbsaems", envir = environment())
  utils::data("adjacency_matrix_car", package = "hbsaems", envir = environment())
  utils::data("spatial_weight_sar",   package = "hbsaems", envir = environment())
}
