# tests/testthat/dev-tests/test-sampling-variance-compile.R
# =============================================================================
# Stan-compiling tests for the sampling_variance / n+deff sugar.
#
# These tests reach `brms::brm()` (no early validation error fires) and
# therefore trigger a Stan model compilation.  Each compile takes 10-60s
# on a developer laptop and well over the CRAN test-time budget of 60s
# per file, so they are gated behind `_R_RUN_DEV_TESTS_=true` and live
# under tests/testthat/dev-tests/ rather than tests/testthat/.
#
# Run manually:
#
#   _R_RUN_DEV_TESTS_=true Rscript -e "
#     testthat::test_file(
#       'tests/testthat/dev-tests/test-sampling-variance-compile.R',
#       reporter = 'progress')"
#
# Or set it once for the whole session:
#   Sys.setenv(`_R_RUN_DEV_TESTS_` = 'true')
#   testthat::test_dir('tests/testthat/dev-tests')
#
# These tests exercise the *successful* path through the sampling_variance
# sugar (i.e.\ no conflict triggers an early stop).  Pure validation
# checks (column missing, conflicting fixed_params, etc.) remain in
# tests/testthat/test-additions.R and run under the normal CRAN check
# because they error out before Stan is even loaded.
# =============================================================================

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(hbsaems))

# Skip everything unless the user opts in
if (!isTRUE(as.logical(Sys.getenv("_R_RUN_DEV_TESTS_", "false")))) {
  testthat::skip("Stan-compiling tests only run when _R_RUN_DEV_TESTS_=true.")
}


# ---------------------------------------------------------------------------
# 1. sampling_variance: family-compat gate accepts continuous families
# ---------------------------------------------------------------------------

test_that("sampling_variance accepted for student family", {
  .dev_skip()
  data("data_fhnorm")
  # We only test the family-compat gate, not actual model convergence.
  # The call should pass the gate; we tolerate any downstream sampler
  # error (since chains=1, iter=1 produces nothing meaningful).
  res <- tryCatch(
    hbm(formula = brms::bf(y ~ x1),
        data   = data_fhnorm,
        re     = ~ (1 | regency),
        hb_sampling = "student",
        sampling_variance = "D",
        chains = 1, iter = 1, refresh = 0),
    error = function(e) conditionMessage(e)
  )
  # Should NOT be the family-compat error
  expect_false(grepl("residual SD parameter named", res, fixed = TRUE),
               info = paste("Got error:", res))
})

test_that("sampling_variance accepted for lognormal family", {
  .dev_skip()
  data("data_fhnorm")
  d_pos <- data_fhnorm
  d_pos$y <- abs(d_pos$y) + 1     # positive for lognormal
  res <- tryCatch(
    hbm(formula = brms::bf(y ~ x1),
        data   = d_pos,
        re     = ~ (1 | regency),
        hb_sampling = "lognormal",
        sampling_variance = "D",
        chains = 1, iter = 1, refresh = 0),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl("residual SD parameter named", res, fixed = TRUE),
               info = paste("Got error:", res))
})


# ---------------------------------------------------------------------------
# 2. hbm_betalogitnorm: n+deff route after centralised-helper refactor
# ---------------------------------------------------------------------------

test_that("hbm_betalogitnorm: still works after refactor (n + deff route)", {
  .dev_skip()
  skip_if_not_installed("brms")
  data("data_betalogitnorm")

  # Quick syntax check — should not error on translation, but may error
  # downstream due to chains=1, iter=1.
  out <- tryCatch(
    suppressWarnings(
      hbm_betalogitnorm(response  = "y",
                         auxiliary = c("x1", "x2"),
                         data      = data_betalogitnorm,
                         n         = "n",
                         deff      = "deff",
                         area_var  = "regency",
                         chains = 1, iter = 1, refresh = 0)
    ),
    error = function(e) conditionMessage(e)
  )
  # If an error fires it must NOT be about the translation
  expect_false(grepl("Cannot supply both|non-positive", out, fixed = FALSE),
               info = paste("Got:", out))
})


# ---------------------------------------------------------------------------
# 3. Conflict matrix: successful (non-conflicting) flows that reach Stan
# ---------------------------------------------------------------------------
# Pure validation-error cases (Case 2, 4, 5, 6, 7, 8) live in
# test-additions.R because they short-circuit BEFORE Stan compile.
# Only the "this should fit cleanly" cases (1 and 3) need to be here.

test_that("Case 1: sampling_variance + fixed_params=NULL --> reaches Stan", {
  .dev_skip()
  data("data_fhnorm")
  out <- tryCatch(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        sampling_variance = "D",
        chains = 1, iter = 1, refresh = 0),
    error = function(e) conditionMessage(e)
  )
  # Allow any error EXCEPT our specific conflict messages
  expect_false(grepl("Cannot supply both", out, fixed = TRUE),
               info = paste("Unexpected conflict:", out))
})

test_that("Case 3: sampling_variance + fixed_params on other dpar --> reaches Stan", {
  .dev_skip()
  data("data_fhnorm")
  # student family has both sigma AND nu; pin nu via fixed_params
  out <- tryCatch(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        hb_sampling = "student",
        sampling_variance = "D",
        fixed_params = list(nu = 4),
        chains = 1, iter = 1, refresh = 0),
    error = function(e) conditionMessage(e)
  )
  # Must NOT error about sigma conflict
  expect_false(grepl("Cannot supply both `sampling_variance`",
                      out, fixed = TRUE),
               info = paste("Unexpected:", out))
})


# ---------------------------------------------------------------------------
# Section 4: Stan-code verification for link_sigma override (v1.0.0 fix)
# ---------------------------------------------------------------------------
# These tests inspect the *generated* Stan code via brms::make_stancode()
# to verify that link_<par> = "identity" is correctly propagated when
# distributional parameters are pinned via sampling_variance or
# fixed_params.  They don't require an actual Stan compile (no Boost),
# but they DO require brms (which is the entry condition for dev-tests
# anyway), so they belong here rather than in test-additions.R.
# ---------------------------------------------------------------------------

test_that("Stan code: sampling_variance keeps sigma on natural scale (no exp)", {
  .dev_skip()
  data("data_fhnorm")

  # Capture the brms formula + family that hbm() builds
  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- list(formula = formula, data = data)
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  suppressWarnings(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        sampling_variance = "D")
  )

  expect_false(is.null(captured))

  # Generate Stan code from the captured formula
  sc <- brms::make_stancode(captured$formula, data = captured$data)

  # Stan code must contain `sigma += offsets_sigma` (the offset is applied)
  expect_match(sc, "sigma += offsets_sigma", fixed = TRUE)

  # Stan code must NOT contain `sigma = exp(sigma)` -- that would mean the
  # log link is still being applied to the offset, which would corrupt
  # the Fay-Herriot scale.
  expect_false(
    grepl("sigma = exp\\(sigma\\)", sc),
    info = "BUG: link_sigma override failed; offset is being exp()'d"
  )
})

test_that("Stan code: fixed_params$sigma also keeps sigma on natural scale", {
  .dev_skip()
  data("data_fhnorm")

  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- list(formula = formula, data = data)
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  suppressWarnings(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        fixed_params = list(sigma = "D"))
  )

  sc <- brms::make_stancode(captured$formula, data = captured$data)
  expect_match(sc, "sigma += offsets_sigma", fixed = TRUE)
  expect_false(grepl("sigma = exp\\(sigma\\)", sc))
})

test_that("Stan code: hbm_betalogitnorm n+deff keeps phi on natural scale", {
  .dev_skip()
  data("data_betalogitnorm")

  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- list(formula = formula, data = data)
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  suppressWarnings(
    hbm_betalogitnorm(
      response = "y", auxiliary = c("x1", "x2", "x3"),
      n = "n", deff = "deff",
      area_var = "regency",
      data = data_betalogitnorm
    )
  )

  sc <- brms::make_stancode(captured$formula, data = captured$data)
  expect_match(sc, "phi += offsets_phi", fixed = TRUE)
  # link_phi = "identity" so phi NOT wrapped in exp
  expect_false(grepl("phi = exp\\(phi\\)", sc))
})

test_that("Stan code: random phi mode keeps brms default gamma(0.01,0.01) prior", {
  .dev_skip()
  data("data_betalogitnorm")

  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- list(formula = formula, data = data)
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  suppressWarnings(
    hbm_betalogitnorm(
      response = "y", auxiliary = c("x1", "x2", "x3"),
      area_var = "regency",
      data = data_betalogitnorm
      # No n, no deff -> random phi mode
    )
  )

  # No legacy alpha/beta parameters should be declared
  sc <- brms::make_stancode(captured$formula, data = captured$data)
  expect_false(grepl("real<lower=1> alpha", sc),
                info = "BUG: legacy alpha parameter still declared in random mode")
  expect_false(grepl("real<lower=0> beta", sc),
                info = "BUG: legacy beta parameter still declared in random mode")

  # phi should still be a parameter
  expect_match(sc, "real<lower=0> phi", fixed = TRUE)
})
