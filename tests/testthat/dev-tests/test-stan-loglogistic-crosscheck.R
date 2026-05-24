# tests/testthat/dev-tests/test-stan-loglogistic-crosscheck.R
# ==========================================================================
# Stan-side cross-check for the loglogistic and shifted_loglogistic
# Stan functions shipped in inst/stan/.
#
# IMPORTANT NAMING CONVENTION
# ---------------------------
# Since Stan 2.29, the language ships a BUILT-IN `loglogistic_lpdf`.
# To avoid a Stan compile-time symbol collision, hbsaems registers its
# custom families under "hbsae_loglogistic" and "hbsae_shifted_loglogistic"
# (see R/dist-loglogistic.R and R/dist-shifted-loglogistic.R).  The
# Stan function definitions live in inst/stan/hbsae_loglogistic.stan
# and inst/stan/hbsae_shifted_loglogistic.stan.
#
# This file verifies that those functions return the same log-density as
# the corresponding R-side d*(log = TRUE) helpers.
#
# It is parked under tests/testthat/dev-tests/ because:
#   * Compiling a Stan model is expensive (10-30+ seconds) and unsuitable
#     for the CRAN check budget (60 s / file).
#   * It requires rstan to be installed AND a working C++ toolchain.
#
# Run manually on developer machines with:
#
#   testthat::test_file(
#     "tests/testthat/dev-tests/test-stan-loglogistic-crosscheck.R")
#
# Or set _R_RUN_DEV_TESTS_=true before running the full suite.
#
# Reference for Stan's native loglogistic_lpdf:
#   https://mc-stan.org/docs/functions-reference/log-logistic-distribution.html
# ==========================================================================

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(hbsaems))

# Skip the entire file in standard CRAN / CI runs unless explicitly opted in.
if (!isTRUE(as.logical(Sys.getenv("_R_RUN_DEV_TESTS_", "false")))) {
  testthat::skip("Stan cross-check only runs when _R_RUN_DEV_TESTS_=true.")
}


# --------------------------------------------------------------------------
# Helper: compile a minimal Stan model that exposes the function block.
# --------------------------------------------------------------------------
.compile_stan_helpers <- function(stan_file) {
  if (!nzchar(stan_file) || !file.exists(stan_file)) return(NULL)
  fns_code <- paste(readLines(stan_file), collapse = "\n")
  stancode <- paste("functions {\n", fns_code, "\n}\nmodel { }\n", sep = "")
  tryCatch(
    rstan::stan_model(model_code = stancode, verbose = FALSE),
    error = function(e) NULL
  )
}


# --------------------------------------------------------------------------
# 1. hbsae_loglogistic_lpdf: Stan vs R dloglogistic
# --------------------------------------------------------------------------

test_that("Stan hbsae_loglogistic_lpdf matches R dloglogistic(log = TRUE)", {
  .dev_skip()
  skip_if_not_installed("rstan")

  stan_file <- system.file("stan", "hbsae_loglogistic.stan",
                            package = "hbsaems")
  if (!nzchar(stan_file))
    skip("hbsae_loglogistic.stan not in installed package.")

  mod <- .compile_stan_helpers(stan_file)
  if (is.null(mod)) skip("Could not compile hbsae_loglogistic.stan.")

  rstan::expose_stan_functions(mod)

  # Compare on a small grid
  y_vec  <- c(0.5, 1, 2, 5)
  mu_vec <- c(0.5, 1, 2, 5)
  beta   <- 2.5

  stan_val <- hbsae_loglogistic_lpdf(y_vec, mu_vec, beta)
  r_val    <- sum(dloglogistic(y_vec, mu = mu_vec, beta = beta, log = TRUE))
  expect_equal(stan_val, r_val, tolerance = 1e-8)
})


# --------------------------------------------------------------------------
# 2. hbsae_shifted_loglogistic_lpdf: Stan vs R dshifted_loglogistic
# --------------------------------------------------------------------------

test_that("Stan hbsae_shifted_loglogistic_lpdf matches R dshifted_loglogistic(log=TRUE)", {
  .dev_skip()
  skip_if_not_installed("rstan")

  stan_file <- system.file("stan", "hbsae_shifted_loglogistic.stan",
                            package = "hbsaems")
  if (!nzchar(stan_file))
    skip("hbsae_shifted_loglogistic.stan not in installed package.")

  mod <- .compile_stan_helpers(stan_file)
  if (is.null(mod))
    skip("Could not compile hbsae_shifted_loglogistic.stan.")

  rstan::expose_stan_functions(mod)

  y_vec  <- c(-1, 0, 1, 2)
  mu_vec <- rep(0, length(y_vec))
  sigma  <- 1
  xi     <- 0.3

  stan_val <- hbsae_shifted_loglogistic_lpdf(y_vec, mu_vec, sigma, xi)
  r_val    <- sum(dshifted_loglogistic(y_vec, mu = 0, sigma = sigma,
                                          xi = xi, log = TRUE))
  expect_equal(stan_val, r_val, tolerance = 1e-8)
})


# --------------------------------------------------------------------------
# 3. Sanity check: our hbsae_loglogistic agrees with Stan's BUILT-IN
#    loglogistic_lpdf (since Stan 2.29).  This is a guard against silent
#    parameterisation drift.
# --------------------------------------------------------------------------

test_that("hbsae_loglogistic_lpdf agrees with Stan native loglogistic_lpdf", {
  .dev_skip()
  skip_if_not_installed("rstan")

  # Build a fresh Stan model that uses BOTH functions and compares them.
  stan_file <- system.file("stan", "hbsae_loglogistic.stan",
                            package = "hbsaems")
  if (!nzchar(stan_file)) skip("hbsae_loglogistic.stan missing.")
  fns_code <- paste(readLines(stan_file), collapse = "\n")

  stancode <- paste(
    "functions {\n", fns_code, "\n}\n",
    "data {\n",
    "  int N;\n",
    "  vector<lower=0>[N] y;\n",
    "  vector<lower=0>[N] mu;\n",
    "  real<lower=0> beta;\n",
    "}\n",
    "parameters { }\n",
    "model { }\n",
    "generated quantities {\n",
    "  real lp_hbsae  = hbsae_loglogistic_lpdf(y | mu, beta);\n",
    "  real lp_native = 0;\n",
    "  for (i in 1:N) lp_native += loglogistic_lpdf(y[i] | mu[i], beta);\n",
    "}\n",
    sep = "")

  mod <- tryCatch(
    rstan::stan_model(model_code = stancode, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(mod))
    skip("Could not compile Stan parameterisation cross-check.")

  y_vec  <- c(0.5, 1, 2, 5)
  mu_vec <- c(0.5, 1, 2, 5)
  beta   <- 2.5

  fit <- rstan::sampling(mod,
                          data = list(N = length(y_vec),
                                        y = y_vec, mu = mu_vec, beta = beta),
                          chains = 1, iter = 1, algorithm = "Fixed_param",
                          refresh = 0, show_messages = FALSE)
  out <- rstan::extract(fit)
  expect_equal(as.numeric(out$lp_hbsae),
               as.numeric(out$lp_native),
               tolerance = 1e-8)
})
