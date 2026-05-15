# tests/testthat/test-v041-additions.R
# =============================================================================
# Unit tests for the v0.4.1 OOP-polish additions:
#   - Formal trio: new_hbmfit / validate_hbmfit / hbmfit
#   - Base class hbsaems_check + is.hbsaems_check
#   - Config helpers: hbm_control / hbm_priors / hbm_nonlinear
# All CRAN-safe (no Stan calls).
# =============================================================================


# ---------------------------------------------------------------------------
# IMP 2: validate_hbmfit + hbmfit
# ---------------------------------------------------------------------------

test_that("validate_hbmfit rejects non-hbmfit", {
  expect_error(validate_hbmfit("not_an_hbmfit"), "not of class 'hbmfit'")
  expect_error(validate_hbmfit(list()), "not of class 'hbmfit'")
})

test_that("validate_hbmfit flags missing slots", {
  x <- structure(list(model = NULL), class = "hbmfit")
  expect_error(validate_hbmfit(x), "missing required slot")
})

test_that("validate_hbmfit flags wrong model class", {
  x <- structure(
    list(model = "not_a_brmsfit", missing_method = NULL,
         data = data.frame(y = 1)),
    class = "hbmfit"
  )
  expect_error(validate_hbmfit(x), "must inherit from 'brmsfit'")
})

test_that("validate_hbmfit flags invalid missing_method", {
  fake_model <- structure(list(), class = "brmsfit")
  x <- structure(
    list(model = fake_model, missing_method = "foo",
         data = data.frame(y = 1)),
    class = "hbmfit"
  )
  expect_error(validate_hbmfit(x), "must be one of")
})

test_that("validate_hbmfit flags empty data", {
  fake_model <- structure(list(), class = "brmsfit")
  x <- structure(
    list(model = fake_model, missing_method = NULL,
         data = data.frame(y = numeric(0))),
    class = "hbmfit"
  )
  expect_error(validate_hbmfit(x), "at least one row")
})

test_that("validate_hbmfit flags disagreement of handle_missing alias", {
  fake_model <- structure(list(), class = "brmsfit")
  x <- structure(
    list(model = fake_model, missing_method = "deleted",
         handle_missing = "model",
         data = data.frame(y = 1)),
    class = "hbmfit"
  )
  expect_error(validate_hbmfit(x), "both backwards-compat slots must agree")
})

test_that("validate_hbmfit returns input invisibly on success", {
  fake_model <- structure(list(), class = "brmsfit")
  x <- structure(
    list(model = fake_model, missing_method = "deleted",
         handle_missing = "deleted",
         data = data.frame(y = 1:3)),
    class = "hbmfit"
  )
  expect_silent(out <- validate_hbmfit(x))
  expect_identical(out, x)
})


test_that("hbmfit user-helper validates and returns hbmfit", {
  fake_model <- structure(list(), class = "brmsfit")
  d <- data.frame(y = 1:3, x = 1:3)
  fit <- hbmfit(model = fake_model, data = d, missing_method = "deleted")
  expect_s3_class(fit, "hbmfit")
  expect_equal(fit$missing_method, "deleted")
})

test_that("hbmfit helper rejects bad inputs", {
  expect_error(hbmfit(model = "x", data = data.frame(y = 1)),
               "(brmsfit|inherit)")
})


# ---------------------------------------------------------------------------
# IMP 3: Base class hbsaems_check
# ---------------------------------------------------------------------------

test_that("check_data result inherits from hbsaems_check", {
  d <- data.frame(y = rnorm(5), x = 1:5)
  chk <- check_data(d, response = "y", predictors = "x")
  expect_true(is.hbsaems_check(chk))
  expect_true(inherits(chk, "hbsaems_data_check"))
  expect_true(inherits(chk, "hbsaems_check"))
})

test_that("check_spatial_weight result inherits from hbsaems_check", {
  M <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
  chk <- check_spatial_weight(M, sre_type = "car", verbose = FALSE)
  expect_true(is.hbsaems_check(chk))
  expect_true(inherits(chk, "hbsaems_spatial_check"))
  expect_true(inherits(chk, "hbsaems_check"))
})

test_that("check_shiny_deps result inherits from hbsaems_check", {
  chk <- check_shiny_deps(verbose = FALSE)
  expect_true(is.hbsaems_check(chk))
  expect_true(inherits(chk, "hbsaems_shiny_check"))
  expect_true(inherits(chk, "hbsaems_check"))
})

test_that("is.hbsaems_check rejects non-check objects", {
  expect_false(is.hbsaems_check("plain string"))
  expect_false(is.hbsaems_check(list()))
  expect_false(is.hbsaems_check(NULL))
})


# ---------------------------------------------------------------------------
# IMP 4: Config helpers
# ---------------------------------------------------------------------------

test_that("hbm_control builds a clean named list", {
  ctrl <- hbm_control(chains = 2, iter = 1000, cores = 2)
  expect_type(ctrl, "list")
  expect_equal(ctrl$chains, 2L)
  expect_equal(ctrl$iter,   1000L)
  expect_equal(ctrl$warmup, 500L)   # auto-default to iter/2
  expect_equal(ctrl$cores,  2L)
})

test_that("hbm_control merges adapt_delta into control list", {
  ctrl <- hbm_control(adapt_delta = 0.99, max_treedepth = 15)
  expect_true(is.list(ctrl$control))
  expect_equal(ctrl$control$adapt_delta, 0.99)
  expect_equal(ctrl$control$max_treedepth, 15)
})

test_that("hbm_control validates input types", {
  expect_error(hbm_control(chains = -1), "chains")
  expect_error(hbm_control(iter = 0), "iter")
  expect_error(hbm_control(adapt_delta = 1.5))
})

test_that("hbm_priors picks the right hyperparameters", {
  p_hs <- hbm_priors(prior_type = "horseshoe", hs_df = 3, hs_df_slab = 4)
  expect_equal(p_hs$prior_type, "horseshoe")
  expect_equal(p_hs$hs_df, 3)
  expect_null(p_hs$r2d2_mean_R2)   # not included for HS

  p_r <- hbm_priors(prior_type = "r2d2", r2d2_mean_R2 = 0.6)
  expect_equal(p_r$prior_type, "r2d2")
  expect_equal(p_r$r2d2_mean_R2, 0.6)
  expect_null(p_r$hs_df)            # not included for R2D2

  p_def <- hbm_priors()             # default
  expect_equal(p_def$prior_type, "default")
})

test_that("hbm_nonlinear validates terms", {
  expect_error(hbm_nonlinear(character(0)),  "non-empty")
  expect_error(hbm_nonlinear(NULL),          "non-empty")
})

test_that("hbm_nonlinear builds expected list", {
  nl <- hbm_nonlinear(c("x1", "x2"), type = "spline", k = 5)
  expect_equal(nl$nonlinear,      c("x1", "x2"))
  expect_equal(nl$nonlinear_type, "spline")
  expect_equal(nl$spline_k,       5L)
  expect_null(nl$gp_scale)

  nl_gp <- hbm_nonlinear("x1", type = "gp", gp_scale = 1.5)
  expect_equal(nl_gp$nonlinear_type, "gp")
  expect_equal(nl_gp$gp_scale,       1.5)
})

test_that("config bundles splice correctly when passed to hbm() (logic mirror)", {
  # v0.5.1+: hbm() detects hbm_config objects in `...` and splices them.
  # Here we replicate the splice logic to verify expected merging
  # behaviour without invoking Stan.
  ctrl   <- hbm_control(chains = 4, iter = 4000)
  priors <- hbm_priors(prior_type = "horseshoe", hs_df = 2)

  # Manual splice mirroring hbm()'s internal logic
  merged <- utils::modifyList(utils::modifyList(list(), ctrl), priors)
  merged <- utils::modifyList(merged, list(cores = 2))

  expect_equal(merged$chains, 4L)
  expect_equal(merged$iter,   4000L)
  expect_equal(merged$prior_type, "horseshoe")
  expect_equal(merged$hs_df,  2)
  expect_equal(merged$cores,  2)     # override took effect
})


# =============================================================================
# v0.5.0+: hbm() accepts config bundles directly via `...`
# =============================================================================

test_that("hbm_control returns S3 hbm_config_control object", {
  c1 <- hbm_control(chains = 2, iter = 1000)
  expect_s3_class(c1, "hbm_config_control")
  expect_s3_class(c1, "hbm_config")
  expect_s3_class(c1, "list")
})

test_that("hbm_priors returns S3 hbm_config_priors object", {
  p <- hbm_priors(prior_type = "horseshoe")
  expect_s3_class(p, "hbm_config_priors")
  expect_s3_class(p, "hbm_config")
})

test_that("hbm_nonlinear returns S3 hbm_config_nonlinear object", {
  nl <- hbm_nonlinear("x", type = "spline")
  expect_s3_class(nl, "hbm_config_nonlinear")
  expect_s3_class(nl, "hbm_config")
})

test_that("hbm_config inheritance lets generic predicates work", {
  expect_true(inherits(hbm_control(),    "hbm_config"))
  expect_true(inherits(hbm_priors(),     "hbm_config"))
  expect_true(inherits(hbm_nonlinear("x"), "hbm_config"))
  expect_false(inherits(list(),          "hbm_config"))
})

test_that("removed v0.5.0 aliases are no longer exported", {
  # hbm_call, hbm_generic, hbsae_wrapper were removed in v0.5.1
  exports <- getNamespaceExports("hbsaems")
  expect_false("hbm_call"      %in% exports)
  expect_false("hbm_generic"   %in% exports)
  expect_false("hbsae_wrapper" %in% exports)
})
