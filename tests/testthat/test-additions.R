# tests/testthat/test-additions.R
# =============================================================================
# Unit tests for cross-cutting additions in v1.0.0:
#
#   1. Formal trio: new_hbmfit / validate_hbmfit / hbmfit
#   2. Base class hbsaems_check + is.hbsaems_check
#   3. Config helpers: hbm_control / hbm_priors / hbm_nonlinear
#   4. Deprecation aliases for spatial-weight checks
#   5. Removed aliases sanity check
#
# Tests that overlap with focused suites (test-spatial-validate.R,
# test-spatial-weight.R, test-spatial-arg-validation.R, test-class-predicates.R,
# test-check-data.R) have been removed from here to avoid duplication.
# All CRAN-safe (no Stan calls).
# =============================================================================


# ---------------------------------------------------------------------------
# 1. validate_hbmfit + hbmfit (OOP polish)
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
# 2. Base class hbsaems_check (only the inheritance checks not covered
#    by test-class-predicates.R)
# ---------------------------------------------------------------------------

test_that("check_data result inherits from hbsaems_check", {
  d <- data.frame(y = rnorm(5), x = 1:5)
  chk <- check_data(d, response = "y", auxiliary = "x")
  expect_true(is.hbsaems_check(chk))
  expect_true(inherits(chk, "hbsaems_data_check"))
  expect_true(inherits(chk, "hbsaems_check"))
})

test_that("check_spatial_weight result inherits from hbsaems_check", {
  M <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
  chk <- check_spatial_weight(M, spatial_model = "car", verbose = FALSE)
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
# 3. Config helpers (hbm_control, hbm_priors, hbm_nonlinear)
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

test_that("hbm_nonlinear builds expected list (spline)", {
  nl <- hbm_nonlinear(c("x1", "x2"), type = "spline", k = 5)
  expect_equal(nl$nonlinear,      c("x1", "x2"))
  expect_equal(nl$nonlinear_type, "spline")
  expect_equal(nl$spline_k,       5L)
  expect_null(nl$gp_k)
  expect_null(nl$gp_c)
  
  # With custom basis type
  nl_cr <- hbm_nonlinear("x1", type = "spline", k = 8, spline_bs = "cr")
  expect_equal(nl_cr$spline_bs, "cr")
})

test_that("hbm_nonlinear builds expected list (gp)", {
  # HSGP with Matern 5/2
  nl_gp <- hbm_nonlinear("x1", type = "gp", k = 20, gp_cov = "matern25")
  expect_equal(nl_gp$nonlinear_type, "gp")
  expect_equal(nl_gp$gp_k, 20L)
  expect_equal(nl_gp$gp_cov, "matern25")
  # Use [[ to avoid $ partial matching (gp_c <-> gp_cov)
  expect_null(nl_gp[["gp_c"]])
  
  # Exact GP (NA basis)
  nl_exact <- hbm_nonlinear("x1", type = "gp", k = NA)
  expect_true(is.na(nl_exact$gp_k))
})

test_that("hbm_nonlinear: deprecated gp_scale maps to gp_c with warning", {
  expect_warning(
    nl <- hbm_nonlinear("x1", type = "gp", gp_scale = 1.5),
    "deprecated"
  )
  expect_equal(nl$gp_c, 1.5)
})

test_that("hbm_nonlinear: both gp_c and gp_scale = error", {
  expect_error(
    hbm_nonlinear("x1", type = "gp", gp_c = 1.5, gp_scale = 1.5),
    "either"
  )
})

test_that(".build_nonlinear_term produces brms-canonical strings", {
  # Splines
  expect_equal(hbsaems:::.build_nonlinear_term("x", "spline"),
               "s(x)")
  expect_equal(hbsaems:::.build_nonlinear_term("x", "spline", spline_k = 8L),
               "s(x, k = 8)")
  expect_equal(hbsaems:::.build_nonlinear_term("x", "spline",
                                               spline_bs = "cr"),
               "s(x, bs = \"cr\")")
  expect_equal(hbsaems:::.build_nonlinear_term("x", "spline",
                                               spline_k  = 10L,
                                               spline_bs = "cs"),
               "s(x, k = 10, bs = \"cs\")")
  
  # GP
  expect_equal(hbsaems:::.build_nonlinear_term("x", "gp"),
               "gp(x)")
  # When gp_k is set, c is auto-supplied (brms requirement: HSGP needs c)
  expect_equal(hbsaems:::.build_nonlinear_term("x", "gp", gp_k = 25L),
               "gp(x, k = 25, c = 1.25)")
  expect_equal(hbsaems:::.build_nonlinear_term("x", "gp",
                                               gp_k   = 20L,
                                               gp_cov = "matern25"),
               "gp(x, k = 20, cov = \"matern25\", c = 1.25)")
  # User-supplied c is respected
  expect_equal(hbsaems:::.build_nonlinear_term("x", "gp",
                                               gp_k = 15L, gp_c = 2),
               "gp(x, k = 15, c = 2)")
  
  # Invalid cov
  expect_error(hbsaems:::.build_nonlinear_term("x", "gp", gp_cov = "bogus"),
               "must be one of")
})

test_that("config bundles splice correctly when passed to hbm() (logic mirror)", {
  # hbm() detects hbm_config objects in `...` and splices them.
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


# ---------------------------------------------------------------------------
# 7. Shrinkage prior cascade: horseshoe / R2D2 with splines and GPs
# ---------------------------------------------------------------------------

test_that("Shrinkage prior cascade: linear-only model gets b only", {
  fml <- brms::bf(y ~ x1 + x2)
  p_hs <- hbsaems:::.build_shrinkage_priors_full(
    formula = fml, prior_type = "horseshoe")
  expect_s3_class(p_hs, "brmsprior")
  expect_equal(unique(p_hs$class), "b")
  # Should NOT contain main=TRUE (no companion classes)
  expect_false(any(grepl("main = TRUE", p_hs$prior)))
})

test_that("Shrinkage prior cascade: spline model gets b + sds", {
  fml <- brms::bf(y ~ x1 + s(x2))
  p_hs <- hbsaems:::.build_shrinkage_priors_full(
    formula = fml, prior_type = "horseshoe")
  expect_setequal(unique(p_hs$class), c("b", "sds"))
  # Main flag on the `b` row
  b_row <- p_hs[p_hs$class == "b", ]
  expect_true(any(grepl("main = TRUE", b_row$prior)))
})

test_that("Shrinkage prior cascade: GP model gets b + sdgp", {
  fml <- brms::bf(y ~ x1 + gp(x2, k = 10, c = 1.25))
  p_hs <- hbsaems:::.build_shrinkage_priors_full(
    formula = fml, prior_type = "horseshoe")
  expect_setequal(unique(p_hs$class), c("b", "sdgp"))
  b_row <- p_hs[p_hs$class == "b", ]
  expect_true(any(grepl("main = TRUE", b_row$prior)))
})

test_that("Shrinkage prior cascade: spline + GP gives b + sds + sdgp", {
  fml <- brms::bf(y ~ x1 + s(x2) + gp(x3, k = 10, c = 1.25))
  p_hs <- hbsaems:::.build_shrinkage_priors_full(
    formula = fml, prior_type = "horseshoe")
  expect_setequal(unique(p_hs$class), c("b", "sds", "sdgp"))
  expect_equal(nrow(p_hs), 3L)
})

test_that("Shrinkage prior cascade: R2D2 also cascades correctly", {
  fml <- brms::bf(y ~ x1 + s(x2) + gp(x3, k = 10, c = 1.25))
  p_r2 <- hbsaems:::.build_shrinkage_priors_full(
    formula      = fml,
    prior_type   = "r2d2",
    r2d2_mean_R2 = 0.7,
    r2d2_prec_R2 = 3
  )
  expect_setequal(unique(p_r2$class), c("b", "sds", "sdgp"))
  expect_true(any(grepl("R2D2", p_r2$prior)))
  expect_true(any(grepl("mean_R2 = 0.7", p_r2$prior)))
})

test_that("Shrinkage prior: default returns NULL (no cascade)", {
  fml <- brms::bf(y ~ x1 + s(x2) + gp(x3))
  expect_null(hbsaems:::.build_shrinkage_priors_full(
    formula = fml, prior_type = "default"))
  expect_null(hbsaems:::.build_shrinkage_priors_full(
    formula = fml, prior_type = NULL))
})

test_that("autoscale arguments propagate through cascade", {
  fml <- brms::bf(y ~ x1 + s(x2))
  p_hs <- hbsaems:::.build_shrinkage_priors_full(
    formula      = fml,
    prior_type   = "horseshoe",
    hs_autoscale = FALSE
  )
  # autoscale=FALSE should appear in the prior string
  expect_true(any(grepl("autoscale = FALSE", p_hs$prior)))
})

test_that("HSGP gp() with gp_k auto-supplies c (brms requirement)", {
  # brms errors if k is set without c.  We should auto-supply 5/4.
  fml_str <- hbsaems:::.build_nonlinear_term("x", "gp", gp_k = 10L)
  expect_match(fml_str, "c = 1.25")
  
  # User-supplied c respected
  fml_str2 <- hbsaems:::.build_nonlinear_term("x", "gp",
                                              gp_k = 10L, gp_c = 2)
  expect_match(fml_str2, "c = 2")
  expect_false(grepl("c = 1.25", fml_str2))
})


# ---------------------------------------------------------------------------
# 4. hbm_config S3 inheritance
# ---------------------------------------------------------------------------

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


# ---------------------------------------------------------------------------
# 5. Removed aliases sanity check
# ---------------------------------------------------------------------------

test_that("removed v1.0.0 aliases are no longer exported", {
  # hbm_call, hbm_generic, hbsae_wrapper were removed in v1.0.0
  exports <- getNamespaceExports("hbsaems")
  expect_false("hbm_call"      %in% exports)
  expect_false("hbm_generic"   %in% exports)
  expect_false("hbsae_wrapper" %in% exports)
})


# ---------------------------------------------------------------------------
# 6. Spatial-weight check structure (only what is NOT in test-spatial-validate)
# ---------------------------------------------------------------------------

test_that("check_spatial_weight returns the required component set", {
  M <- matrix(c(0,1,1,0, 1,0,0,1, 1,0,0,1, 0,1,1,0), 4, 4)
  chk <- check_spatial_weight(M, spatial_model = "car", verbose = FALSE)
  
  required <- c("is_square", "has_zero_diag", "is_symmetric",
                "detected_style", "n_isolated", "n_components",
                "issues", "warnings", "compatible")
  expect_true(all(required %in% names(chk)),
              info = paste("Missing:",
                           paste(setdiff(required, names(chk)),
                                 collapse = ", ")))
})


# ---------------------------------------------------------------------------
# 7. model_average() new method= argument
# ---------------------------------------------------------------------------

test_that("model_average: method='manual' validates weights vector", {
  # Without actually fitting models, test the argument validation
  fake_model <- structure(list(), class = c("hbmfit", "list"))
  fake_model$model <- structure(list(), class = "brmsfit")
  
  # Manual: wrong-length weights → error
  expect_error(
    model_average(fake_model, fake_model, weights = c(1, 2, 3)),
    "length\\(weights\\)"
  )
  
  # Stacking: passing weights when method != manual → error
  expect_error(
    model_average(fake_model, fake_model,
                  method = "stacking", weights = c(0.5, 0.5)),
    "must be NULL"
  )
})

test_that("model_average: bad method choice → error", {
  fake_model <- structure(list(), class = c("hbmfit", "list"))
  fake_model$model <- structure(list(), class = "brmsfit")
  expect_error(
    model_average(fake_model, fake_model, method = "bogus"),
    "should be one of"
  )
})


# ---------------------------------------------------------------------------
# 8. prior_sensitivity()
# ---------------------------------------------------------------------------

test_that("prior_sensitivity: validates input type", {
  expect_error(prior_sensitivity("not a model"),
               "hbmfit or brmsfit")
  expect_error(prior_sensitivity(NULL),
               "hbmfit or brmsfit")
})

test_that("prior_sensitivity: dispatches correctly based on priorsense availability", {
  fake <- structure(list(model = structure(list(), class = "brmsfit")),
                    class = c("hbmfit", "list"))
  
  if (requireNamespace("priorsense", quietly = TRUE)) {
    # Path A: priorsense available --- function should forward to it
    # (stub via mockery so we don't need a real fitted brmsfit)
    skip_if_not_installed("mockery")
    sentinel <- structure(data.frame(variable   = "test",
                                     prior      = 0.01,
                                     likelihood = 0.5,
                                     diagnosis  = "-"),
                          class = c("powerscale_sensitivity_summary",
                                    "data.frame"))
    mockery::stub(prior_sensitivity, "priorsense::powerscale_sensitivity",
                  function(...) sentinel)
    out <- prior_sensitivity(fake)
    expect_s3_class(out, "powerscale_sensitivity_summary")
    expect_identical(out, sentinel)
  } else {
    # Path B: priorsense not installed --- function should emit a
    # graceful message and return NULL
    expect_message(out <- prior_sensitivity(fake),
                   "priorsense.*required")
    expect_null(out)
  }
})


# ---------------------------------------------------------------------------
# 9. Conflict matrix between prior, prior_type, fixed_params, stanvars
# ---------------------------------------------------------------------------

test_that(".merge_prior_type: user global prior on class='b' wins, warns", {
  user_p <- brms::set_prior("normal(0, 1)", class = "b")
  type_p <- brms::set_prior("horseshoe(1)",  class = "b")
  expect_warning(merged <- hbsaems:::.merge_prior_type(user_p, type_p),
                 "global prior for class")
  # User's prior is retained, type_prior dropped for class b
  expect_equal(merged$prior, "normal(0, 1)")
})

test_that(".merge_prior_type: cascade — user prior on 'sds' drops only sds entry", {
  # Construct a cascade prior (b + sds + sdgp)
  type_p <- brms::set_prior("horseshoe(1, main = TRUE)", class = "b") +
    brms::set_prior("horseshoe(1)",              class = "sds") +
    brms::set_prior("horseshoe(1)",              class = "sdgp")
  user_p <- brms::set_prior("student_t(3, 0, 1)", class = "sds")
  
  expect_warning(merged <- hbsaems:::.merge_prior_type(user_p, type_p),
                 "class = 'sds'")
  # b + sdgp from type_p preserved; sds from user_p wins
  expect_setequal(unique(merged$class), c("b", "sds", "sdgp"))
  sds_row <- merged[merged$class == "sds", ]
  expect_equal(sds_row$prior, "student_t(3, 0, 1)")
})

test_that(".merge_prior_type: cascade — user priors on b and sds, only sdgp left", {
  type_p <- brms::set_prior("horseshoe(1, main = TRUE)", class = "b") +
    brms::set_prior("horseshoe(1)",              class = "sds") +
    brms::set_prior("horseshoe(1)",              class = "sdgp")
  user_p <- brms::set_prior("normal(0, 1)",        class = "b") +
    brms::set_prior("student_t(3, 0, 1)",  class = "sds")
  
  expect_warning(merged <- hbsaems:::.merge_prior_type(user_p, type_p),
                 "class = 'b', 'sds'")
  # Both user entries + sdgp from type_p
  expect_setequal(unique(merged$class), c("b", "sds", "sdgp"))
})

test_that(".merge_prior_type: coefficient-specific user prior does NOT trigger warning", {
  # User overrides only one coefficient — global shrinkage prior on 'b'
  # should still apply to the remaining coefficients.
  user_p <- brms::set_prior("normal(0, 0.5)", class = "b", coef = "x1")
  type_p <- brms::set_prior("horseshoe(1)",   class = "b")
  expect_silent(merged <- hbsaems:::.merge_prior_type(user_p, type_p))
  expect_setequal(unique(merged$class), "b")
  expect_equal(nrow(merged), 2L)
})

test_that(".merge_prior_type: type_prior=NULL passthrough", {
  user_p <- brms::set_prior("normal(0, 1)", class = "b")
  expect_identical(hbsaems:::.merge_prior_type(user_p, NULL), user_p)
})

test_that(".merge_prior_type: user_prior=NULL passthrough", {
  type_p <- brms::set_prior("horseshoe(1)", class = "b")
  expect_identical(hbsaems:::.merge_prior_type(NULL, type_p), type_p)
})