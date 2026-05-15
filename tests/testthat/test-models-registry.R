# tests/testthat/test-families-registry.R
# =============================================================================
# Unit tests for the family registry + register_hbsae_model() public API.
# All CRAN-safe (no Stan).
# =============================================================================

# -- list_hbsae_models ------------------------------------------------------

test_that("list_hbsae_models returns the four primary families", {
  fams <- list_hbsae_models()
  for (k in c("gaussian", "beta", "binomial", "lognormal"))
    expect_true(k %in% fams,
                info = sprintf("'%s' should be in registered families", k))
})

test_that("list_hbsae_models is sorted", {
  fams <- list_hbsae_models()
  expect_equal(fams, sort(fams))
})


# -- get_hbsae_model ---------------------------------------------------------

test_that("get_hbsae_model returns expected fields", {
  spec <- get_hbsae_model("lognormal")
  expect_type(spec, "list")
  expect_named(spec, c("family", "link", "discrete", "supports_mi",
                        "has_addition_term", "addition_template",
                        "response_check", "response_check_msg",
                        "default_priors"))
  expect_equal(spec$family, "lognormal")
  expect_false(spec$discrete)
  expect_true(spec$supports_mi)
})

test_that("get_hbsae_model for binomial reports discrete + addition term", {
  spec <- get_hbsae_model("binomial")
  expect_true(spec$discrete)
  expect_false(spec$supports_mi)
  expect_true(spec$has_addition_term)
  expect_equal(spec$addition_template, "%s | trials(%s) ~ %s")
})

test_that("get_hbsae_model warns on unknown key", {
  expect_warning(get_hbsae_model("unicorn"), "not registered")
})


# -- response_check functions are correct ------------------------------------

test_that("beta response_check rejects zero/one boundary values", {
  spec <- get_hbsae_model("beta")
  expect_true(spec$response_check(c(0.1, 0.5, 0.9)))
  expect_false(spec$response_check(c(0.1, 0.5, 1.0)))   # 1.0 invalid
  expect_false(spec$response_check(c(0,   0.5)))         # 0 invalid
  expect_true(spec$response_check(c(0.5, NA)))           # NA OK
})

test_that("lognormal response_check rejects non-positive", {
  spec <- get_hbsae_model("lognormal")
  expect_true(spec$response_check(c(1.0, 2.0, 3.0)))
  expect_false(spec$response_check(c(0.5, 0)))
  expect_false(spec$response_check(c(-1, 1)))
})

test_that("binomial response_check rejects non-integer / negative", {
  spec <- get_hbsae_model("binomial")
  expect_true(spec$response_check(c(0L, 5L, 10L)))
  expect_false(spec$response_check(c(0, 5, -1)))
  expect_false(spec$response_check(c(0, 5, 5.5)))
})


# -- register_hbsae_model --------------------------------------------------

test_that("register_hbsae_model adds a new family", {
  on.exit({
    rm("test_gamma", envir = hbsaems:::.hbsae_model_env)
  })

  register_hbsae_model(
    key            = "test_gamma",
    family         = "Gamma",
    link           = "log",
    discrete       = FALSE,
    supports_mi    = TRUE,
    response_check = function(y) all(y > 0, na.rm = TRUE),
    response_check_msg = "Gamma must be positive."
  )
  expect_true("test_gamma" %in% list_hbsae_models())
  spec <- get_hbsae_model("test_gamma")
  expect_equal(spec$family, "Gamma")
  expect_equal(spec$link,   "log")
})

test_that("register_hbsae_model rejects duplicate without overwrite", {
  expect_error(
    register_hbsae_model(key = "lognormal", family = "lognormal"),
    "already registered"
  )
})

test_that("register_hbsae_model allows overwrite = TRUE", {
  on.exit({
    # Restore original lognormal spec
    hbsaems:::.init_model_registry()
  })
  expect_silent(
    register_hbsae_model(key = "lognormal", family = "lognormal",
                          overwrite = TRUE)
  )
})

test_that("register_hbsae_model validates argument types", {
  expect_error(register_hbsae_model(key = 1, family = "x"),
               "single character string")
  expect_error(register_hbsae_model(key = "x", family = 1),
               "single character string")
  expect_error(register_hbsae_model(key = "x", family = "y", discrete = "no"),
               "single logical")
  expect_error(register_hbsae_model(key = "x", family = "y",
                                      response_check = "not_a_function"),
               "must be a function")
})

test_that("register_hbsae_model requires addition_template when has_addition_term", {
  expect_error(
    register_hbsae_model(key = "x", family = "y",
                          has_addition_term = TRUE,
                          addition_template = NULL),
    "addition_template' is required"
  )
})


# -- registry-based discrete check ------------------------------------------

test_that(".model_is_discrete recognises built-in discrete families", {
  expect_true(hbsaems:::.model_is_discrete("binomial"))
  expect_true(hbsaems:::.model_is_discrete("poisson"))
  expect_true(hbsaems:::.model_is_discrete("negbinomial"))
  expect_false(hbsaems:::.model_is_discrete("gaussian"))
  expect_false(hbsaems:::.model_is_discrete("lognormal"))
  expect_false(hbsaems:::.model_is_discrete("Beta"))
})

test_that(".family_supports_mi flips correctly", {
  expect_true(hbsaems:::.family_supports_mi("gaussian"))
  expect_true(hbsaems:::.family_supports_mi("lognormal"))
  expect_false(hbsaems:::.family_supports_mi("binomial"))
  expect_false(hbsaems:::.family_supports_mi("poisson"))
})


# -- hbm_flex input validation -----------------------------------------

test_that("hbm_flex rejects unknown family key", {
  d <- data.frame(y = 1:5, x = 1:5)
  expect_error(
    hbm_flex("unicorn", "y", "x", data = d),
    "Unknown family key"
  )
})

test_that("hbm_flex rejects missing response variable", {
  d <- data.frame(z = 1:5)
  expect_error(
    hbm_flex("gaussian", "y", "z", data = d),
    "Response variable 'y' not found"
  )
})

test_that("hbm_flex requires addition_var for binomial", {
  d <- data.frame(y = 1:5, x = 1:5)
  expect_error(
    hbm_flex("binomial", "y", "x", data = d),
    "requires an addition-term variable"
  )
})

test_that("hbm_flex enforces beta domain", {
  d <- data.frame(y = c(0.5, 1.0, 0.7), x = 1:3)
  expect_error(
    hbm_flex("beta", "y", "x", data = d),
    "strictly in"
  )
})

test_that("hbm_flex enforces lognormal positivity", {
  d <- data.frame(y = c(1, 2, -1), x = 1:3)
  expect_error(
    hbm_flex("lognormal", "y", "x", data = d),
    "strictly positive"
  )
})

test_that("hbm_flex rejects 'model' missing for discrete family", {
  d <- data.frame(y = c(1L, 2L, NA), n = c(10L, 10L, 10L), x = 1:3)
  expect_error(
    hbm_flex("binomial", "y", "x", data = d,
                   addition_var = "n",
                   handle_missing = "model"),
    "not supported for the 'binomial'"
  )
})
