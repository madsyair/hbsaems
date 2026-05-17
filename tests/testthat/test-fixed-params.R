# tests/testthat/test-fixed-params.R
# =============================================================================
# Tests for the v1.0.0 `fixed_params` mechanism and the refactored
# hbm_betalogitnorm() wrapper.  CRAN-safe: no Stan compilation.
# =============================================================================


# ---- .resolve_fixed_param -------------------------------------------------

test_that(".resolve_fixed_param accepts column name", {
  d <- data.frame(y = 1:5, phi = c(10, 20, 30, 40, 50))
  expect_equal(
    hbsaems:::.resolve_fixed_param("phi", "phi", d),
    c(10, 20, 30, 40, 50)
  )
})

test_that(".resolve_fixed_param broadcasts scalar", {
  d <- data.frame(y = 1:5)
  expect_equal(
    hbsaems:::.resolve_fixed_param("phi", 3.14, d),
    rep(3.14, 5)
  )
})

test_that(".resolve_fixed_param accepts length-n vector", {
  d <- data.frame(y = 1:5)
  expect_equal(
    hbsaems:::.resolve_fixed_param("phi", 1:5, d),
    as.numeric(1:5)
  )
})

test_that(".resolve_fixed_param evaluates one-sided formula", {
  d <- data.frame(y = 1:5, n = c(50, 60, 55, 65, 52), deff = c(1.2, 1.1, 1.3, 1.0, 1.15))
  expect_equal(
    hbsaems:::.resolve_fixed_param("phi", ~ I(n / deff - 1), d),
    d$n / d$deff - 1,
    tolerance = 1e-10
  )
})

test_that(".resolve_fixed_param errors on missing column", {
  d <- data.frame(y = 1:5)
  expect_error(
    hbsaems:::.resolve_fixed_param("phi", "nonexistent_column", d),
    "not a column in"
  )
})

test_that(".resolve_fixed_param errors on wrong vector length", {
  d <- data.frame(y = 1:5)
  expect_error(
    hbsaems:::.resolve_fixed_param("phi", c(1, 2, 3), d),
    "must be length 1.*or length 5"
  )
})

test_that(".resolve_fixed_param errors on non-numeric column", {
  d <- data.frame(y = 1:5, foo = letters[1:5])
  expect_error(
    hbsaems:::.resolve_fixed_param("phi", "foo", d),
    "must be numeric"
  )
})

test_that(".resolve_fixed_param errors on bad spec type", {
  d <- data.frame(y = 1:5)
  expect_error(
    hbsaems:::.resolve_fixed_param("phi", list(1, 2), d),
    "spec must be"
  )
})


# ---- .process_fixed_params --------------------------------------------------

test_that(".process_fixed_params handles NULL input", {
  res <- hbsaems:::.process_fixed_params(NULL, data.frame(y = 1:5))
  expect_length(res$resolved,  0L)
  expect_length(res$col_names, 0L)
})

test_that(".process_fixed_params handles empty list", {
  res <- hbsaems:::.process_fixed_params(list(), data.frame(y = 1:5))
  expect_length(res$resolved,  0L)
  expect_length(res$col_names, 0L)
})

test_that(".process_fixed_params validates names", {
  d <- data.frame(y = 1:5)
  expect_error(
    hbsaems:::.process_fixed_params(list(3.14), d),
    "non-empty names"
  )
})

test_that(".process_fixed_params catches NAs", {
  d <- data.frame(y = 1:5, phi = c(1, 2, NA, 4, 5))
  expect_error(
    hbsaems:::.process_fixed_params(list(phi = "phi"), d),
    "contains NA"
  )
})

test_that(".process_fixed_params catches non-finite values", {
  d <- data.frame(y = 1:5, phi = c(1, 2, Inf, 4, 5))
  expect_error(
    hbsaems:::.process_fixed_params(list(phi = "phi"), d),
    "non-finite"
  )
})

test_that(".process_fixed_params produces correct column names", {
  d <- data.frame(y = 1:5)
  res <- hbsaems:::.process_fixed_params(
    list(phi = 1.0, sigma = 2.0),
    d
  )
  expect_equal(
    unname(res$col_names),
    c(".hbsaems_phi_fixed", ".hbsaems_sigma_fixed")
  )
})


# ---- .attach_fixed_columns --------------------------------------------------

test_that(".attach_fixed_columns appends new columns", {
  d <- data.frame(y = 1:5)
  proc <- hbsaems:::.process_fixed_params(list(phi = 1:5), d)
  d2 <- hbsaems:::.attach_fixed_columns(d, proc)
  expect_true(".hbsaems_phi_fixed" %in% names(d2))
  expect_equal(d2$.hbsaems_phi_fixed, 1:5)
})


# ---- hbm_betalogitnorm wrapper input validation -----------------------------

test_that("hbm_betalogitnorm: response outside (0,1) is rejected", {
  d <- data.frame(y = c(0, 0.5, 1), x1 = 1:3)
  expect_error(
    hbm_betalogitnorm(response = "y", auxiliary = "x1", data = d,
                       chains = 1, iter = 10),
    "must lie strictly in \\(0, 1\\)"
  )
})

test_that("hbm_betalogitnorm: n without deff is rejected", {
  d <- data.frame(y = c(0.3, 0.5, 0.7), x1 = 1:3, n = 10:12)
  expect_error(
    hbm_betalogitnorm(response = "y", auxiliary = "x1", data = d,
                       n = "n",
                       chains = 1, iter = 10),
    "Both `n` and `deff`"
  )
})

test_that("hbm_betalogitnorm: deff without n is rejected", {
  d <- data.frame(y = c(0.3, 0.5, 0.7), x1 = 1:3, deff = c(1.1, 1.2, 1.3))
  expect_error(
    hbm_betalogitnorm(response = "y", auxiliary = "x1", data = d,
                       deff = "deff",
                       chains = 1, iter = 10),
    "Both `n` and `deff`"
  )
})

test_that("hbm_betalogitnorm: non-existent n column is rejected", {
  d <- data.frame(y = c(0.3, 0.5, 0.7), x1 = 1:3)
  expect_error(
    hbm_betalogitnorm(response = "y", auxiliary = "x1", data = d,
                       n = "nope", deff = "nope2",
                       chains = 1, iter = 10),
    "`n = \"nope\"` is not a column"
  )
})

test_that("hbm_betalogitnorm: negative deff produces clear error", {
  d <- data.frame(y = c(0.3, 0.5, 0.7), x1 = 1:3,
                  n = c(50, 60, 70), deff = c(1.1, -1.0, 1.2))
  expect_error(
    hbm_betalogitnorm(response = "y", auxiliary = "x1", data = d,
                       n = "n", deff = "deff",
                       chains = 1, iter = 10),
    "strictly positive"
  )
})


# ---- .merge_betalogitnorm_priors  (internal helper) -------------------------

test_that(".merge_betalogitnorm_priors fills in defaults when prior is NULL", {
  testthat::skip_if_not_installed("brms")
  out <- hbsaems:::.merge_betalogitnorm_priors(NULL, phi_is_fixed = FALSE)
  classes <- out$class
  expect_true("Intercept" %in% classes)
  expect_true("b"          %in% classes)
  expect_true("phi"        %in% classes)
})

test_that(".merge_betalogitnorm_priors omits phi prior when fixed", {
  testthat::skip_if_not_installed("brms")
  out <- hbsaems:::.merge_betalogitnorm_priors(NULL, phi_is_fixed = TRUE)
  expect_false("phi" %in% out$class)
})

test_that(".merge_betalogitnorm_priors errors on phi prior + fixed mode", {
  testthat::skip_if_not_installed("brms")
  p <- brms::set_prior("normal(0, 1)", class = "phi")
  expect_error(
    hbsaems:::.merge_betalogitnorm_priors(p, phi_is_fixed = TRUE),
    "Cannot supply a prior on `phi`"
  )
})

test_that(".merge_betalogitnorm_priors preserves user-supplied b prior", {
  testthat::skip_if_not_installed("brms")
  p <- brms::set_prior("normal(0, 0.5)", class = "b")
  out <- hbsaems:::.merge_betalogitnorm_priors(p, phi_is_fixed = FALSE)
  # Find the b prior; ensure it's the user's one
  b_idx <- which(out$class == "b" & (is.na(out$coef) | out$coef == ""))
  expect_equal(out$prior[b_idx[1L]], "normal(0, 0.5)")
})


# ---- .extract_model_block_vars  (internal helper) ---------------------------

test_that(".extract_model_block_vars finds alpha and beta in stanvars", {
  testthat::skip_if_not_installed("brms")
  sv <- brms::stanvar(scode = "alpha ~ gamma(2,1);", block = "model") +
        brms::stanvar(scode = "beta  ~ gamma(1,1);", block = "model")
  expect_setequal(
    hbsaems:::.extract_model_block_vars(sv),
    c("alpha", "beta")
  )
})

test_that(".extract_model_block_vars returns empty for NULL", {
  expect_length(hbsaems:::.extract_model_block_vars(NULL), 0L)
})


# ---- Integration: fixed_params goes through to brms (mocked) ----------------

test_that("fixed_params end-to-end flow attaches column to data", {
  testthat::skip_if_not_installed("brms")

  # Stub brms::brm so we don't actually compile Stan; capture the call
  captured_data <- NULL
  captured_formula <- NULL
  .stub <- function(formula, data, ...) {
    captured_data    <<- data
    captured_formula <<- formula
    stop("STUB", call. = FALSE)
  }
  testthat::local_mocked_bindings(brm = .stub, .package = "brms")

  d <- data.frame(y = runif(20, 0.1, 0.9), x1 = rnorm(20),
                  n = sample(50:100, 20, replace = TRUE),
                  deff = runif(20, 1.0, 1.5),
                  group = rep(1:5, 4))

  tryCatch(
    hbm_betalogitnorm(response = "y", auxiliary = "x1",
                       n = "n", deff = "deff",
                       area_var = "group",      # column happens to be named "group"
                       data = d,
                       chains = 1, iter = 10),
    error = function(e) NULL
  )

  # The augmented data should have .hbsaems_phi_fixed
  expect_true(".hbsaems_phi_fixed" %in% names(captured_data))
  # Values should equal n/deff - 1
  expect_equal(captured_data$.hbsaems_phi_fixed,
                d$n / d$deff - 1,
                tolerance = 1e-10)
})


# ---- v1.0.0 conflict checks --------------------------------------------------
# These verify the new strict conflict detection in hbm() Section 10b
# and in hbm_betalogitnorm() Mode-2 stanvars validation.

test_that(".extract_stanvar_model_targets recognises sampling LHS", {
  testthat::skip_if_not_installed("brms")
  sv <- brms::stanvar(scode = "phi ~ gamma(2, 0.5);", block = "model") +
        brms::stanvar(scode = "alpha ~ gamma(2, 1);", block = "model")
  expect_setequal(
    hbsaems:::.extract_stanvar_model_targets(sv),
    c("phi", "alpha")
  )
})

test_that(".extract_stanvar_model_targets ignores non-model blocks", {
  testthat::skip_if_not_installed("brms")
  sv <- brms::stanvar(scode = "real foo;", block = "parameters") +
        brms::stanvar(scode = "phi ~ gamma(2, 0.5);", block = "model")
  out <- hbsaems:::.extract_stanvar_model_targets(sv)
  expect_setequal(out, "phi")
})

test_that(".extract_stanvar_model_targets returns empty for NULL", {
  expect_length(hbsaems:::.extract_stanvar_model_targets(NULL), 0L)
})

test_that("hbm_betalogitnorm: hyperprior on alpha/beta + fixed phi -> error", {
  testthat::skip_if_not_installed("brms")
  d <- data.frame(
    y    = runif(10, 0.2, 0.8),
    x1   = rnorm(10),
    n    = sample(50:100, 10, replace = TRUE),
    deff = runif(10, 1.0, 1.5)
  )
  expect_error(
    hbm_betalogitnorm(
      response = "y", auxiliary = "x1",
      n = "n", deff = "deff",
      data = d,
      stanvars = brms::stanvar(scode = "alpha ~ gamma(2,1);",
                                 block = "model"),
      chains = 1, iter = 10
    ),
    "hyperprior on `alpha`"
  )
})
