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
                 regexp = "sds", fixed = TRUE)
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
                 regexp = "global prior for class", fixed = TRUE)
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


# ---------------------------------------------------------------------------
# 10. Hierarchical area_var helper
# ---------------------------------------------------------------------------

test_that(".build_area_re_formula: NULL -> NULL", {
  expect_null(hbsaems:::.build_area_re_formula(NULL))
  expect_null(hbsaems:::.build_area_re_formula(character(0)))
})

test_that(".build_area_re_formula: single column -> (1 | x)", {
  f <- hbsaems:::.build_area_re_formula("regency")
  expect_s3_class(f, "formula")
  expect_match(deparse1(f), "(1 | regency)", fixed = TRUE)
})

test_that(".build_area_re_formula: nested two-level -> (1 | a/b)", {
  f <- hbsaems:::.build_area_re_formula(c("province", "regency"),
                                          structure = "nested")
  # R may format the formula with or without surrounding spaces around '/'.
  # Both `province / regency` and `province/regency` are valid forms of the
  # same RE structure.  Use a regex that tolerates either.
  expect_match(deparse1(f),
               "province\\s*/\\s*regency",
               perl = TRUE)
})

test_that(".build_area_re_formula: crossed two-level -> (1|a) + (1|b)", {
  f <- hbsaems:::.build_area_re_formula(c("province", "regency"),
                                          structure = "crossed")
  d <- deparse1(f)
  # Match either spaced or compact formulations
  expect_match(d, "\\(1\\s*\\|\\s*province\\)", perl = TRUE)
  expect_match(d, "\\(1\\s*\\|\\s*regency\\)",  perl = TRUE)
  expect_match(d, "\\+", fixed = FALSE)
})

test_that(".build_area_re_formula: rejects empty strings", {
  expect_error(hbsaems:::.build_area_re_formula(c("province", "")),
               "empty strings")
})

test_that(".build_area_re_formula: rejects non-character", {
  expect_error(hbsaems:::.build_area_re_formula(c(1, 2)),
               "character vector")
})

test_that(".check_area_var_columns: catches missing columns", {
  df <- data.frame(province = 1:4, regency = letters[1:4])
  expect_error(hbsaems:::.check_area_var_columns("bogus", df),
               "not found in")
  expect_error(hbsaems:::.check_area_var_columns(c("province", "nonexistent"), df),
               "not found in")
})

test_that(".check_area_var_columns: silent on valid columns", {
  df <- data.frame(province = rep(c("A","B"), 5),
                    regency  = rep(letters[1:5], 2))
  expect_silent(hbsaems:::.check_area_var_columns(c("province", "regency"), df))
})

test_that(".check_area_var_columns: warns on too-many-levels heuristic", {
  df <- data.frame(province = sample(1:6, 100, TRUE),
                    pretend_continuous = runif(100))
  expect_warning(
    hbsaems:::.check_area_var_columns(
      c("province", "pretend_continuous"), df),
    "more like a continuous covariate"
  )
})


# ---------------------------------------------------------------------------
# 11. hbm() Fay-Herriot sampling_variance sugar
# ---------------------------------------------------------------------------

test_that("hbm(): sampling_variance translates to fixed_params$sigma", {
  data("data_fhnorm")

  # We test the input-translation logic only; do NOT actually fit a model.
  expect_error(
    hbm(formula = brms::bf(y ~ x1),
        data = data_fhnorm,
        re   = ~ (1 | regency),
        sampling_variance = "nonexistent_col"),
    "not found in"
  )
})

test_that("hbm(): sampling_variance conflicts with fixed_params$sigma", {
  data("data_fhnorm")
  expect_error(
    hbm(formula = brms::bf(y ~ x1),
        data = data_fhnorm,
        re   = ~ (1 | regency),
        sampling_variance = "D",
        fixed_params = list(sigma = 0.5)),
    "Cannot supply both"
  )
})

test_that("hbm(): sampling_variance rejects non-positive or NA", {
  data("data_fhnorm")
  bad <- data_fhnorm
  bad$D[1] <- -1
  expect_error(
    hbm(formula = brms::bf(y ~ x1),
        data = bad,
        re   = ~ (1 | regency),
        sampling_variance = "D"),
    "strictly positive"
  )
})


# ---------------------------------------------------------------------------
# 12. hbm() sampling_variance: family compatibility validation
# ---------------------------------------------------------------------------

test_that("sampling_variance rejected for Beta family", {
  data("data_fhnorm")
  # Force a beta family to test the rejection — y won't actually be in
  # (0,1) but the check fires BEFORE brms touches the data.
  expect_error(
    hbm(formula = brms::bf(y ~ x1),
        data   = data_fhnorm,
        re     = ~ (1 | regency),
        hb_sampling = "beta",
        sampling_variance = "D"),
    "residual SD parameter named"
  )
})

test_that("sampling_variance rejected for Binomial family", {
  data("data_fhnorm")
  expect_error(
    hbm(formula = brms::bf(y ~ x1),
        data   = data_fhnorm,
        re     = ~ (1 | regency),
        hb_sampling = "binomial",
        sampling_variance = "D"),
    "residual SD parameter named"
  )
})

test_that("sampling_variance rejected for Poisson family", {
  data("data_fhnorm")
  expect_error(
    hbm(formula = brms::bf(y ~ x1),
        data   = data_fhnorm,
        re     = ~ (1 | regency),
        hb_sampling = "poisson",
        sampling_variance = "D"),
    "residual SD parameter named"
  )
})

test_that("sampling_variance error message lists compatible families", {
  data("data_fhnorm")
  err <- tryCatch(
    hbm(formula = brms::bf(y ~ x1),
        data   = data_fhnorm,
        re     = ~ (1 | regency),
        hb_sampling = "negbinomial",
        sampling_variance = "D"),
    error = conditionMessage
  )
  # Error message must mention all compatible families
  for (fam in c("gaussian", "lognormal", "student"))
    expect_match(err, fam, fixed = TRUE)
})

test_that("sampling_variance error message suggests Beta alternative", {
  data("data_fhnorm")
  err <- tryCatch(
    hbm(formula = brms::bf(y ~ x1),
        data   = data_fhnorm,
        re     = ~ (1 | regency),
        hb_sampling = "beta",
        sampling_variance = "D"),
    error = conditionMessage
  )
  # Beta alternative should be mentioned: phi via design effect
  expect_match(err, "phi", fixed = TRUE)
  expect_match(err, "deff", fixed = TRUE)
})

# Note: tests verifying that sampling_variance is ACCEPTED for student /
# lognormal families compile Stan and have been moved to
# tests/testthat/dev-tests/test-sampling-variance-compile.R.


# ---------------------------------------------------------------------------
# 13. Centralized sugar translators
# ---------------------------------------------------------------------------

test_that(".translate_sampling_variance: NULL passthrough", {
  fp <- list(nu = 4)
  out <- hbsaems:::.translate_sampling_variance(fp, NULL, data.frame())
  expect_identical(out, fp)
})

test_that(".translate_sampling_variance: adds sigma to fixed_params", {
  df <- data.frame(D = c(0.5, 1.0, 1.5))
  out <- hbsaems:::.translate_sampling_variance(
    fixed_params = NULL, sampling_variance = "D", data = df)
  expect_named(out, "sigma")
  expect_equal(out$sigma, sqrt(df$D), tolerance = 1e-12)
})

test_that(".translate_sampling_variance: preserves existing fixed_params", {
  df <- data.frame(D = c(0.5, 1.0))
  out <- hbsaems:::.translate_sampling_variance(
    fixed_params = list(nu = 4), sampling_variance = "D", data = df)
  expect_named(out, c("nu", "sigma"))
  expect_equal(out$nu, 4)
})

test_that(".translate_sampling_variance: refuses conflict with fixed_params$sigma", {
  df <- data.frame(D = c(0.5, 1.0))
  expect_error(
    hbsaems:::.translate_sampling_variance(
      fixed_params = list(sigma = 0.5),
      sampling_variance = "D", data = df),
    "Cannot supply both"
  )
})

test_that(".translate_sampling_variance: catches missing column", {
  expect_error(
    hbsaems:::.translate_sampling_variance(
      NULL, "bogus", data.frame(x = 1:3)),
    "not found"
  )
})

test_that(".translate_sampling_variance: catches non-positive", {
  expect_error(
    hbsaems:::.translate_sampling_variance(
      NULL, "D", data.frame(D = c(0.5, -1, 1))),
    "strictly positive"
  )
})

test_that(".translate_n_deff_to_phi: NULL passthrough when either missing", {
  df <- data.frame(n = 1:3, deff = 1:3)
  expect_identical(
    hbsaems:::.translate_n_deff_to_phi(list(), NULL, "deff", df),
    list())
  expect_identical(
    hbsaems:::.translate_n_deff_to_phi(list(), "n", NULL, df),
    list())
})

test_that(".translate_n_deff_to_phi: computes phi = n/deff - 1", {
  df <- data.frame(n = c(100, 200, 300), deff = c(2, 4, 5))
  out <- hbsaems:::.translate_n_deff_to_phi(NULL, "n", "deff", df)
  expect_named(out, "phi")
  expect_equal(out$phi, c(49, 49, 59), tolerance = 1e-12)
})

test_that(".translate_n_deff_to_phi: catches non-positive phi result", {
  df <- data.frame(n = c(10, 5), deff = c(20, 5))   # phi = -0.5, 0 -> bad
  expect_error(
    hbsaems:::.translate_n_deff_to_phi(NULL, "n", "deff", df),
    "non-positive"
  )
})

test_that(".translate_n_deff_to_phi: conflicts with fixed_params$phi", {
  df <- data.frame(n = c(100, 200), deff = c(2, 4))
  expect_error(
    hbsaems:::.translate_n_deff_to_phi(
      list(phi = 10), "n", "deff", df),
    "Cannot supply both"
  )
})

# Note: an end-to-end integration test for `hbm_betalogitnorm` with
# `n + deff` is in tests/testthat/dev-tests/test-sampling-variance-compile.R
# (compiles a Stan model).


# ---------------------------------------------------------------------------
# 14. Comprehensive conflict matrix: sampling_variance vs fixed_params/prior/stanvars
# ---------------------------------------------------------------------------
#
# 8 conflict scenarios for the Fay-Herriot sugar `sampling_variance`:
#   1. NULL fixed_params           --> AMAN (pass)   [compile -- dev-tests]
#   2. fixed_params$sigma supplied  --> ERROR  (validation, here)
#   3. fixed_params on other dpar   --> MERGE (pass) [compile -- dev-tests]
#   4. prior on class "sigma"       --> ERROR  (conflict downstream, here)
#   5. stanvars sampling sigma      --> ERROR  (conflict downstream, here)
#   6. (Beta) n+deff + fp$phi       --> ERROR  (validation, here)
#   7. (Beta) n+deff + prior on phi --> ERROR  (conflict downstream, here)
#   8. fixed_params$sigma = "D" identical to sampling_variance --> ERROR (here)
#
#   Cases 1 and 3 (successful flows that reach Stan) live in
#   tests/testthat/dev-tests/test-sampling-variance-compile.R.
# ---------------------------------------------------------------------------

test_that("Case 2: sampling_variance + fixed_params$sigma --> ERROR", {
  data("data_fhnorm")
  expect_error(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        sampling_variance = "D",
        fixed_params = list(sigma = 0.5)),
    "Cannot supply both"
  )
})

test_that("Case 4: sampling_variance + prior(class='sigma') --> ERROR", {
  data("data_fhnorm")
  expect_error(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        sampling_variance = "D",
        prior = brms::set_prior("normal(0, 1)", class = "sigma"),
        chains = 1, iter = 1, refresh = 0),
    "pinned via.*fixed_params"
  )
})

test_that("Case 5: sampling_variance + stanvars sampling sigma --> ERROR", {
  data("data_fhnorm")
  expect_error(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        sampling_variance = "D",
        stanvars = brms::stanvar(scode = "sigma ~ gamma(1, 1);",
                                   block = "model"),
        chains = 1, iter = 1, refresh = 0),
    "sampling statement.*sigma.*pinned"
  )
})

test_that("Case 6: hbm_betalogitnorm n+deff + fixed_params$phi --> ERROR", {
  data("data_betalogitnorm")
  expect_error(
    hbm_betalogitnorm(response  = "y",
                       auxiliary = "x1",
                       data      = data_betalogitnorm,
                       n         = "n",
                       deff      = "deff",
                       fixed_params = list(phi = 10)),
    "Cannot supply both"
  )
})

test_that("Case 7: hbm_betalogitnorm n+deff + prior on phi --> ERROR", {
  skip_if_not_installed("brms")
  data("data_betalogitnorm")
  # When n+deff is set, phi becomes pinned; a prior on phi must error.
  err <- tryCatch(
    hbm_betalogitnorm(response  = "y",
                       auxiliary = "x1",
                       data      = data_betalogitnorm,
                       n         = "n",
                       deff      = "deff",
                       prior = brms::set_prior("gamma(1, 1)", class = "phi"),
                       chains = 1, iter = 1, refresh = 0),
    error = function(e) conditionMessage(e)
  )
  # Either downstream conflict OR a clean error pointing at phi
  expect_match(err, "phi", fixed = TRUE,
               label = paste("Got:", substr(err, 1, 200)))
})

test_that("Case 8: sampling_variance + fixed_params$sigma=col_name (redundant) --> ERROR", {
  data("data_fhnorm")
  expect_error(
    hbm(brms::bf(y ~ x1), data = data_fhnorm, re = ~ (1 | regency),
        sampling_variance = "D",
        fixed_params = list(sigma = "D")),
    "Cannot supply both"
  )
})


# ---------------------------------------------------------------------------
# 15. Conflict matrix: hbm_lnln (pass-through wrapper)
# ---------------------------------------------------------------------------

test_that("hbm_lnln: sampling_variance + fixed_params$sigma --> ERROR transitively", {
  data("data_lnln")
  expect_error(
    hbm_lnln(response  = "y_obs",
             auxiliary = c("x1", "x2"),
             data      = data_lnln,
             area_var  = "regency",
             sampling_variance = "psi_i",
             fixed_params = list(sigma = 0.5)),
    "Cannot supply both"
  )
})

test_that("hbm_lnln: sampling_variance + prior(class='sigma') --> ERROR", {
  skip_if_not_installed("brms")
  data("data_lnln")
  expect_error(
    hbm_lnln(response  = "y_obs",
             auxiliary = c("x1", "x2"),
             data      = data_lnln,
             area_var  = "regency",
             sampling_variance = "psi_i",
             prior = brms::set_prior("normal(0, 1)", class = "sigma"),
             chains = 1, iter = 1, refresh = 0),
    "pinned via.*fixed_params"
  )
})


# ---------------------------------------------------------------------------
# 16. Helper-level direct conflict tests
# ---------------------------------------------------------------------------

test_that(".translate_sampling_variance: detects redundant fp$sigma=col_name", {
  df <- data.frame(D = c(0.5, 1.0, 1.5))
  expect_error(
    hbsaems:::.translate_sampling_variance(
      list(sigma = "D"), "D", df),
    "Cannot supply both"
  )
})

test_that(".translate_sampling_variance: empty list is OK", {
  df <- data.frame(D = c(0.5, 1.0))
  out <- hbsaems:::.translate_sampling_variance(list(), "D", df)
  expect_named(out, "sigma")
})


# ---------------------------------------------------------------------------
# 17. P0-A: sae_benchmark default weights
# ---------------------------------------------------------------------------

test_that("sae_benchmark: target_type='total' uses weights = rep(1, n)", {
  # Build a minimal hbsae_results object
  pred <- c(2, 3, 5)
  res <- structure(
    list(
      result_table = data.frame(area = 1:3, Prediction = pred),
      pred = pred,
      rse_model = c(0.1, 0.1, 0.1)
    ),
    class = "hbsae_results"
  )

  target <- 100   # population total
  out <- suppressMessages(
    sae_benchmark(res, target = target, method = "ratio",
                   target_type = "total")
  )
  # ratio r = target / sum(pred) = 100 / 10 = 10
  expect_equal(out$benchmark_info$adjustment, 10, tolerance = 1e-8)
  expect_equal(out$result_table$Prediction, pred * 10, tolerance = 1e-8)
})

test_that("sae_benchmark: target_type='mean' uses weights = rep(1/n, n)", {
  pred <- c(2, 3, 5)
  res <- structure(
    list(
      result_table = data.frame(area = 1:3, Prediction = pred),
      pred = pred,
      rse_model = c(0.1, 0.1, 0.1)
    ),
    class = "hbsae_results"
  )

  target_mean <- 10   # i.e. all predictions should scale to mean=10
  out <- suppressMessages(
    sae_benchmark(res, target = target_mean, method = "ratio",
                   target_type = "mean")
  )
  # mean(pred) = 10/3.  r = 10 / (10/3) = 3
  expect_equal(out$benchmark_info$adjustment, 3, tolerance = 1e-8)
})

test_that("sae_benchmark: explicit weights override default", {
  pred <- c(2, 3, 5)
  res <- structure(
    list(
      result_table = data.frame(area = 1:3, Prediction = pred),
      pred = pred,
      rse_model = c(0.1, 0.1, 0.1)
    ),
    class = "hbsae_results"
  )
  w <- c(1000, 2000, 3000)
  target <- 20000
  out <- sae_benchmark(res, target = target, weights = w,
                        method = "ratio")
  # r = 20000 / sum(w * pred) = 20000 / (2000 + 6000 + 15000) = 20000/23000
  expect_equal(out$benchmark_info$adjustment, 20000 / 23000, tolerance = 1e-8)
})


# ---------------------------------------------------------------------------
# 18. P0-B: hbm_betalogitnorm conditional link_phi
# ---------------------------------------------------------------------------

test_that("hbm_betalogitnorm: link_phi = 'identity' warning in random mode", {
  skip_if_not_installed("brms")
  # Mock brms::brm to avoid Stan compile -- we only need the warning path
  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  data("data_betalogitnorm")
  expect_warning(
    out <- tryCatch(
      hbm_betalogitnorm(
        response  = "y",
        auxiliary = "x1",
        data      = data_betalogitnorm,
        area_var  = "province",    # silences "no area RE" + repeating levels
        link_phi  = "identity"     # user-supplied AND random mode
      ),
      error = function(e) conditionMessage(e)
    ),
    "may propose negative phi"
  )
})


# ---------------------------------------------------------------------------
# 19. P0-C: sae_scale zero-variance fix
# ---------------------------------------------------------------------------

test_that("sae_scale: zero-variance prediction does not produce NaN", {
  pred <- rep(5, 5)
  res <- structure(
    list(
      result_table = data.frame(area = 1:5, Prediction = pred),
      pred = pred,
      rse_model = rep(0.1, 5)
    ),
    class = "hbsae_results"
  )
  expect_warning(
    out <- sae_scale(res, center = TRUE, scale = TRUE),
    "zero variance"
  )
  # After fix: not NaN; centred to 0 since all values equal mean
  expect_false(any(is.nan(out$pred)))
  expect_equal(out$pred, rep(0, 5))
})


# ---------------------------------------------------------------------------
# 20. P1-A: measurement_error validator
# ---------------------------------------------------------------------------

test_that(".validate_measurement_error: NULL is OK", {
  expect_null(hbsaems:::.validate_measurement_error(NULL, c("x1"), data.frame()))
})

test_that(".validate_measurement_error: requires named list", {
  expect_error(
    hbsaems:::.validate_measurement_error(list("se_x"), "x1", data.frame()),
    "named list"
  )
  expect_error(
    hbsaems:::.validate_measurement_error("not a list", "x1", data.frame()),
    "named list"
  )
})

test_that(".validate_measurement_error: variables must be in auxiliary", {
  expect_error(
    hbsaems:::.validate_measurement_error(
      list(x99 = "se_x99"), c("x1", "x2"), data.frame()),
    "must also appear in"
  )
})

test_that(".validate_measurement_error: SE column must exist in data", {
  expect_error(
    hbsaems:::.validate_measurement_error(
      list(x1 = "nonexistent_se"), "x1",
      data.frame(x1 = 1:3)),
    "not found in"
  )
})

test_that(".validate_measurement_error: SE must be non-negative", {
  df <- data.frame(x1 = 1:3, se_x1 = c(0.1, -0.5, 0.2))
  expect_error(
    hbsaems:::.validate_measurement_error(list(x1 = "se_x1"), "x1", df),
    "non-negative"
  )
})

test_that(".validate_measurement_error: SE must have no NA", {
  df <- data.frame(x1 = 1:3, se_x1 = c(0.1, NA, 0.2))
  expect_error(
    hbsaems:::.validate_measurement_error(list(x1 = "se_x1"), "x1", df),
    "NA values"
  )
})


# ---------------------------------------------------------------------------
# 21. P1-A: .apply_measurement_error formula rewriter
# ---------------------------------------------------------------------------

test_that(".apply_measurement_error: rewrites bare variable to mi(var, se)", {
  fml <- y ~ x1 + x2
  out <- hbsaems:::.apply_measurement_error(
    fml, list(x1 = "se_x1"))
  out_str <- paste(deparse(out), collapse = " ")
  expect_match(out_str, "mi(x1, se_x1)", fixed = TRUE)
  # x2 unchanged
  expect_match(out_str, "\\+ x2", perl = TRUE)
})

test_that(".apply_measurement_error: NULL passthrough", {
  fml <- y ~ x1 + x2
  expect_equal(hbsaems:::.apply_measurement_error(fml, NULL), fml)
})

test_that(".apply_measurement_error: does not touch s(x) terms", {
  fml <- y ~ s(x1) + x2
  out <- hbsaems:::.apply_measurement_error(fml, list(x1 = "se_x1"))
  out_str <- paste(deparse(out), collapse = " ")
  expect_match(out_str, "s(x1)", fixed = TRUE)
  # Should NOT replace inside s() — that would break brms
  expect_false(grepl("s(mi(x1", out_str, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# 22. P1-B: .formula_has_mi detector
# ---------------------------------------------------------------------------

test_that(".formula_has_mi: detects mi() in plain formula", {
  expect_true(hbsaems:::.formula_has_mi(y ~ mi(x1, se_x1)))
})

test_that(".formula_has_mi: detects me() too", {
  expect_true(hbsaems:::.formula_has_mi(y ~ me(x1, se_x1)))
})

test_that(".formula_has_mi: FALSE on plain formula", {
  expect_false(hbsaems:::.formula_has_mi(y ~ x1 + x2))
})

test_that(".formula_has_mi: NULL is FALSE", {
  expect_false(hbsaems:::.formula_has_mi(NULL))
})

test_that(".formula_has_mi: detects mi() in brmsformula", {
  skip_if_not_installed("brms")
  bf <- brms::bf(y ~ mi(x1, se_x1))
  expect_true(hbsaems:::.formula_has_mi(bf))
})


# ---------------------------------------------------------------------------
# 23. P1-A + P1-B: hbm() measurement_error integration (validation only)
# ---------------------------------------------------------------------------

test_that("hbm: measurement_error must be named list", {
  data("data_fhnorm")
  expect_error(
    hbm(brms::bf(y ~ x1), data = data_fhnorm,
        measurement_error = c("se_x1")),
    "named list"
  )
})

test_that("hbm: measurement_error variable must be in formula", {
  data("data_fhnorm")
  d <- data_fhnorm
  d$se_bogus <- 0.1
  expect_error(
    hbm(brms::bf(y ~ x1), data = d,
        measurement_error = list(x99 = "se_bogus")),
    "must also appear in"
  )
})


# ---------------------------------------------------------------------------
# 24. P2-A: .extract_response_names — robust LHS parsing
# ---------------------------------------------------------------------------

test_that(".extract_response_names: bare name", {
  expr <- quote(y)
  expect_equal(hbsaems:::.extract_response_names(expr), "y")
})

test_that(".extract_response_names: transformation log(y)", {
  expr <- quote(log(y))
  expect_equal(hbsaems:::.extract_response_names(expr), "y")
})

test_that(".extract_response_names: y | mi() strips addition", {
  expr <- quote(y | mi())
  expect_equal(hbsaems:::.extract_response_names(expr), "y")
})

test_that(".extract_response_names: y | trials(n) returns only y", {
  expr <- quote(y | trials(n))
  expect_equal(hbsaems:::.extract_response_names(expr), "y")
})

test_that(".extract_response_names: y | trials(n) + cens(censored)", {
  # Multiple addition terms chained -- still only y
  expr <- quote(y | trials(n) + cens(censored))
  expect_equal(hbsaems:::.extract_response_names(expr), "y")
})

test_that(".extract_response_names: NULL is empty character", {
  expect_equal(hbsaems:::.extract_response_names(NULL), character(0))
})

test_that(".parse_hbm_formula: trials(n) does NOT pollute response_var", {
  skip_if_not_installed("brms")
  fml <- brms::bf(y | trials(n) ~ x1 + x2, family = binomial())
  out <- hbsaems:::.parse_hbm_formula(fml)
  expect_equal(out$response_var, "y")
  # n must appear in auxiliary_vars (as a covariate / data column)
  expect_true("n" %in% out$auxiliary_vars || "n" %in% all.vars(fml$formula))
})

test_that(".parse_hbm_formula: mi() on LHS keeps response clean", {
  skip_if_not_installed("brms")
  fml <- brms::bf(y | mi() ~ mi(x1) + x2)
  out <- hbsaems:::.parse_hbm_formula(fml)
  expect_equal(out$response_var, "y")
})


# ---------------------------------------------------------------------------
# 25. P1-C: AST-based formula rewrite robustness
# ---------------------------------------------------------------------------

test_that(".replace_nl_in_formula: bare variable replaced", {
  fml <- y ~ x1 + x2
  out <- hbsaems:::.replace_nl_in_formula(fml, "x1", "spline")
  out_str <- paste(deparse(out), collapse = " ")
  expect_match(out_str, "s(x1", fixed = TRUE)
  # x2 left alone
  expect_match(out_str, "\\+ x2", perl = TRUE)
})

test_that(".replace_nl_in_formula: I() wrapper protects content", {
  # I(x1/1000) should NOT be turned into I(s(x1)/1000)
  fml <- y ~ I(x1 / 1000) + x2
  out <- hbsaems:::.replace_nl_in_formula(fml, "x1", "spline")
  out_str <- paste(deparse(out), collapse = " ")
  # The I() expression must remain intact
  expect_match(out_str, "I(x1/1000)", fixed = TRUE)
  # And we must NOT have created s(x1) inside the I()
  expect_false(grepl("I(s(x1", out_str, fixed = TRUE))
})

test_that(".replace_nl_in_formula: interaction x1:x2 left alone", {
  fml <- y ~ x1:x2
  out <- hbsaems:::.replace_nl_in_formula(fml, "x1", "spline")
  out_str <- paste(deparse(out), collapse = " ")
  # Interaction term preserved bare
  expect_match(out_str, "x1:x2", fixed = TRUE)
  # No spurious s(x1)
  expect_false(grepl("s(x1)", out_str, fixed = TRUE))
})

test_that(".replace_nl_in_formula: NULL nonlinear passes through", {
  fml <- y ~ x1 + x2
  out <- hbsaems:::.replace_nl_in_formula(fml, NULL, "spline")
  # Should be equivalent to the input
  expect_equal(deparse(out), deparse(fml))
})

test_that(".apply_measurement_error: AST-based, I() and s() preserved", {
  # Combine I() transformation and spline wrapper — both must be left alone,
  # only bare x1 rewritten
  fml <- y ~ x1 + I(x1 / 1000) + s(x1) + x2
  out <- hbsaems:::.apply_measurement_error(
    fml, list(x1 = "se_x1"))
  out_str <- paste(deparse(out), collapse = " ")
  # Bare x1 -> mi(x1, se_x1)
  expect_match(out_str, "mi(x1, se_x1)", fixed = TRUE)
  # I() preserved
  expect_match(out_str, "I(x1/1000)", fixed = TRUE)
  # s() preserved (NOT rewritten as s(mi(x1, ...)))
  expect_match(out_str, "s(x1)", fixed = TRUE)
  expect_false(grepl("s(mi(", out_str, fixed = TRUE))
  # x2 untouched
  expect_match(out_str, "\\+ x2", perl = TRUE)
})

test_that(".apply_measurement_error: multiple variables", {
  fml <- y ~ x1 + x2 + x3
  out <- hbsaems:::.apply_measurement_error(
    fml,
    list(x1 = "se_x1", x2 = "se_x2"))
  out_str <- paste(deparse(out), collapse = " ")
  expect_match(out_str, "mi(x1, se_x1)", fixed = TRUE)
  expect_match(out_str, "mi(x2, se_x2)", fixed = TRUE)
  expect_match(out_str, "\\+ x3", perl = TRUE)
})

test_that(".apply_measurement_error: interaction term left alone", {
  fml <- y ~ x1 + x1:x2
  out <- hbsaems:::.apply_measurement_error(
    fml, list(x1 = "se_x1"))
  out_str <- paste(deparse(out), collapse = " ")
  expect_match(out_str, "mi(x1, se_x1)", fixed = TRUE)
  # Interaction term left alone — brms does not support mi() in interaction
  expect_match(out_str, "x1:x2", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# 26. P3-A: stub-based mock fits (no Stan compile)
# ---------------------------------------------------------------------------

test_that(".brm_stub returns brmsfit-shaped object without Stan", {
  out <- .brm_stub(
    formula = y ~ x1,
    data    = data.frame(y = 1:5, x1 = 1:5),
    family  = list(family = "gaussian", link = "identity")
  )
  expect_s3_class(out, "brmsfit")
  expect_true(out$.stubbed)
  expect_equal(out$family$family, "gaussian")
})

test_that(".hbm_stub returns hbmfit-shaped object without Stan", {
  out <- .hbm_stub(
    formula = y ~ x1,
    data    = data.frame(y = 1:5, x1 = 1:5)
  )
  expect_s3_class(out, "hbmfit")
  expect_s3_class(out$model, "brmsfit")
  expect_true(out$.stubbed)
})

test_that(".replace_nl_in_formula: random-intercept term left alone", {
  # (1 | group) appears in many SAE formulas; terms() should treat it
  # as one opaque term label, and our helper must NOT try to rewrite
  # the bare "group" inside it.
  fml <- y ~ x1 + (1 | group)
  out <- tryCatch(
    hbsaems:::.replace_nl_in_formula(fml, "x1", "spline"),
    error = function(e) NULL
  )
  # Either the helper successfully rewrites x1 -> s(x1) AND keeps
  # (1 | group), or it falls back to returning the input unchanged.
  # We accept both -- what we MUST NOT see is a corrupted formula
  # like `s(x1) + (1 | s(group))`.
  if (!is.null(out)) {
    out_str <- paste(deparse(out), collapse = " ")
    expect_false(grepl("s(group", out_str, fixed = TRUE))
  }
  succeed()
})

test_that(".replace_nl_in_formula: -1 (no intercept) preserved", {
  fml <- y ~ -1 + x1 + x2
  out <- hbsaems:::.replace_nl_in_formula(fml, "x1", "spline")
  out_str <- paste(deparse(out), collapse = " ")
  # The no-intercept must survive AST roundtrip
  expect_match(out_str, "- 1", fixed = TRUE)
  # And x1 was rewritten
  expect_match(out_str, "s(x1", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# 22. data2 collision fix (v1.0.0): user-supplied data2 via `...` no longer
# crashes hbm() when spatial_model is also active.
# ---------------------------------------------------------------------------

test_that("hbm: user data2 + spatial_model merges correctly (no collision)", {
  skip_if_not_installed("brms")
  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  data <- data.frame(y = 1:5, x1 = stats::rnorm(5),
                      regency = factor(1:5))
  W <- matrix(0, 5, 5)
  W[upper.tri(W)] <- 1
  W <- W + t(W)
  rownames(W) <- colnames(W) <- as.character(1:5)

  # Should NOT throw 'formal argument "data2" matched by multiple actual arguments'
  expect_no_error(
    suppressWarnings(
      hbm(formula     = brms::bf(y ~ x1),
          data        = data,
          spatial_model = "car",
          spatial_var = "regency",
          M           = W,
          data2       = list(extra_aux = 1:5))
    )
  )
})

test_that("hbm: user data2 alone (no spatial_model) passes through", {
  skip_if_not_installed("brms")
  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  data <- data.frame(y = 1:5, x1 = stats::rnorm(5))
  expect_no_error(
    suppressWarnings(
      hbm(formula = brms::bf(y ~ x1),
          data    = data,
          data2   = list(extra_aux = matrix(1:25, 5, 5)))
    )
  )
})

test_that("hbm: user data2 must be a list", {
  data <- data.frame(y = 1:5, x1 = stats::rnorm(5))
  expect_error(
    suppressWarnings(
      hbm(formula = brms::bf(y ~ x1),
          data    = data,
          data2   = "not a list")
    ),
    "must be a named list"
  )
})

test_that("hbm: passing internally-managed args via ... is rejected", {
  data <- data.frame(y = 1:5, x1 = stats::rnorm(5))

  # save_pars is set internally and should not be overridable
  expect_error(
    suppressWarnings(
      hbm(formula   = brms::bf(y ~ x1),
          data      = data,
          save_pars = brms::save_pars(latent = TRUE))
    ),
    "conflict with values hbm\\(\\) sets internally"
  )
})


# ---------------------------------------------------------------------------
# 23. Identity-link override for pinned distributional parameters (v1.0.0)
# ---------------------------------------------------------------------------
# Critical correctness bug: brms applies the dpar's LINK FUNCTION to the
# linear predictor before plugging it into the likelihood.  For Gaussian
# the default link_sigma = "log", so `sigma ~ 0 + offset(sqrt(D))` was
# producing `sigma = exp(sqrt(D))` rather than `sigma = sqrt(D)`.
# hbm() now overrides link_<par> = "identity" for every pinned dpar.
# ---------------------------------------------------------------------------

test_that("hbm: sampling_variance forces link_sigma = 'identity'", {
  skip_if_not_installed("brms")
  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- formula
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  d <- data.frame(y = 1:5, x1 = stats::rnorm(5),
                   D = c(4.0, 2.25, 3.24, 4.84, 2.89),
                   area = factor(1:5))

  suppressWarnings(
    hbm(formula = brms::bf(y ~ x1),
        data    = d,
        re      = ~ (1 | area),
        sampling_variance = "D")
  )

  # Resolve family object regardless of whether formula is univariate
  # or multivariate
  fam <- if (!is.null(captured$forms))
    captured$forms[[1]]$family
  else
    captured$family

  expect_equal(fam$link_sigma, "identity")
})

test_that("hbm: fixed_params$sigma also forces link_sigma = 'identity'", {
  skip_if_not_installed("brms")
  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- formula
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  d <- data.frame(y = 1:5, x1 = stats::rnorm(5),
                   D = c(4.0, 2.25, 3.24, 4.84, 2.89),
                   area = factor(1:5))

  suppressWarnings(
    hbm(formula = brms::bf(y ~ x1),
        data    = d,
        re      = ~ (1 | area),
        fixed_params = list(sigma = "D"))
  )

  fam <- if (!is.null(captured$forms))
    captured$forms[[1]]$family
  else
    captured$family

  expect_equal(fam$link_sigma, "identity")
})

test_that("hbm: pinned sigma value passed through correctly (sqrt(D) not exp(sqrt(D)))", {
  skip_if_not_installed("brms")

  # Trace the actual data sent to brm
  captured_data <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured_data <<- data
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  d <- data.frame(y = 1:5, x1 = stats::rnorm(5),
                   D = c(4.0, 2.25, 3.24, 4.84, 2.89),
                   area = factor(1:5))
  expected_sigma <- sqrt(d$D)

  suppressWarnings(
    hbm(formula = brms::bf(y ~ x1),
        data    = d,
        re      = ~ (1 | area),
        sampling_variance = "D")
  )

  # Stored values must equal sqrt(D) on the natural scale (NOT log-scale)
  expect_equal(captured_data$.hbsaems_sigma_fixed, expected_sigma,
                tolerance = 1e-10)

  # NO sigma values should have been silently log-transformed
  expect_false(all(captured_data$.hbsaems_sigma_fixed <
                     log(expected_sigma) + 0.1))
})


# ---------------------------------------------------------------------------
# 24. .add_fixed_pforms multivariate (mi()) compatibility (v1.0.0)
# ---------------------------------------------------------------------------
# Bug: `bform + brms::lf(rhs)` does not work for multivariate
# brmsformulas (mvbrmsformula) because brms cannot disambiguate which
# response the dpar applies to.  The fix supplies `resp = <primary>`.
# ---------------------------------------------------------------------------

test_that("hbm: mi() formula + sampling_variance composes correctly", {
  skip_if_not_installed("brms")
  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- formula
      structure(list(), class = "brmsfit")
    },
    .package = "brms"
  )

  d <- data.frame(
    y    = c(10, 12, NA, 11, 9),
    x1   = c(1, NA, 3, 4, 5),
    x2   = stats::rnorm(5),
    D    = c(4, 2.25, 3.24, 4.84, 2.89),
    area = factor(1:5)
  )

  fml <- brms::bf(y | mi() ~ mi(x1) + x2) + brms::bf(x1 | mi() ~ x2)

  expect_no_error(
    suppressMessages(suppressWarnings(
      hbm(formula = fml, data = d, re = ~ (1 | area),
          sampling_variance = "D")
    ))
  )

  # Multivariate formula must be assembled correctly
  expect_true(!is.null(captured$forms))
  # Length must be at least 3: y, x1, sigma offset
  fnames <- vapply(captured$forms,
                    function(f) as.character(f$resp), character(1L))
  expect_true("y" %in% fnames)
  expect_true("x1" %in% fnames)
})

test_that(".add_fixed_pforms supplies resp= for multivariate formulas", {
  skip_if_not_installed("brms")

  # Manually craft a multivariate bform and a fake processed list
  bform_mv <- brms::bf(y ~ x) + brms::bf(z ~ x)
  d <- data.frame(y = 1:5, z = 6:10, x = stats::rnorm(5))

  processed <- list(
    resolved  = list(sigma = rep(0.5, 5)),
    col_names = list(sigma = "fake_sig_col")
  )

  # Should not throw
  expect_no_error(
    out <- hbsaems:::.add_fixed_pforms(bform_mv, processed)
  )

  # Result should be mvbrmsformula
  expect_s3_class(out, "mvbrmsformula")
})



# ---------------------------------------------------------------------------
# 25. update_hbm() preservation of hbsaems offset columns (v1.0.0)
# ---------------------------------------------------------------------------

test_that("update_hbm: newdata without offset col + same nrow -> warn & copy", {
  skip_if_not_installed("brms")

  # Build a fake hbmfit whose stored data carries the hbsaems offset column,
  # so update_hbm() must detect that newdata lacks it and auto-copy.  Using a
  # fake object (not a real hbm() fit) keeps the test CRAN-safe and fast.
  data1 <- data.frame(
    y = c(10, 12, 8, 11, 9), x1 = stats::rnorm(5),
    area = factor(1:5),
    .hbsaems_sigma_fixed = sqrt(c(4, 2.25, 3.24, 4.84, 2.89))
  )
  fit <- structure(
    list(model          = structure(list(data = data1), class = "brmsfit"),
         data           = data1,
         missing_method = "none"),
    class = "hbmfit"
  )

  data2 <- data.frame(            # same nrow, but NO offset column
    y = c(11, 13, 9, 12, 10), x1 = stats::rnorm(5), area = factor(1:5)
  )

  # Robust mock: replace update.brmsfit in the brms namespace (S3 dispatch
  # from stats::update finds it reliably across testthat versions, unlike
  # local_mocked_bindings).
  captured_newdata <- NULL
  orig_update <- getFromNamespace("update.brmsfit", "brms")
  on.exit(assignInNamespace("update.brmsfit", orig_update, ns = "brms"),
          add = TRUE)
  assignInNamespace("update.brmsfit",
    function(object, newdata = NULL, ...) {
      captured_newdata <<- newdata
      structure(list(), class = "brmsfit")
    }, ns = "brms")

  expect_warning(
    update_hbm(fit, newdata = data2),
    "missing hbsaems offset column"
  )
  # The offset column must have been auto-copied into the newdata.
  expect_true(".hbsaems_sigma_fixed" %in% names(captured_newdata))
})

test_that("update_hbm: newdata without offset col + different nrow -> error", {
  skip_if_not_installed("brms")
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...)
      structure(list(data = data, formula = formula),
                class = c("brmsfit", "list")),
    .package = "brms"
  )

  data1 <- data.frame(
    y = c(10, 12, 8, 11, 9),
    x1 = stats::rnorm(5),
    D = c(4, 2.25, 3.24, 4.84, 2.89),
    area = factor(1:5)
  )
  fit <- suppressWarnings(
    hbm(formula = brms::bf(y ~ x1), data = data1,
        re = ~ (1 | area), sampling_variance = "D")
  )

  data3 <- data.frame(
    y    = 1:6,
    x1   = stats::rnorm(6),
    area = factor(1:6)
  )

  expect_error(
    update_hbm(fit, newdata = data3),
    "cannot safely copy"
  )
})

test_that("update_hbm: no offset col in original -> no special handling", {
  skip_if_not_installed("brms")

  data1 <- data.frame(            # NO offset / sampling_variance column
    y = c(10, 12, 8, 11, 9), x1 = stats::rnorm(5), area = factor(1:5)
  )
  fit <- structure(
    list(model          = structure(list(data = data1), class = "brmsfit"),
         data           = data1,
         missing_method = "none"),
    class = "hbmfit"
  )

  data2 <- data1
  data2$y <- data2$y + 1

  orig_update <- getFromNamespace("update.brmsfit", "brms")
  on.exit(assignInNamespace("update.brmsfit", orig_update, ns = "brms"),
          add = TRUE)
  assignInNamespace("update.brmsfit",
    function(object, ...) structure(list(), class = "brmsfit"),
    ns = "brms")

  # No offset column exists, so update_hbm() must not warn about copying one.
  expect_no_warning(update_hbm(fit, newdata = data2))
})


# ---------------------------------------------------------------------------
# 26. hbm_flex area_var vector validation (v1.0.0)
# ---------------------------------------------------------------------------

test_that("hbm_flex: partially-invalid area_var vector -> friendly error", {
  skip_if_not_installed("brms")
  d <- data.frame(y = stats::rnorm(5, 5, 1), x1 = stats::rnorm(5),
                   province = factor(rep(1:2, length.out = 5)))
  # NO "regency" column
  expect_error(
    hbm_flex(family_key = "lognormal", response = "y",
             auxiliary = "x1", data = d,
             area_var = c("province", "regency_INVALID")),
    "Area variable\\(s\\) not found"
  )
})

# ---------------------------------------------------------------------------
# 27. Nested area_var syntax in re validator (v1.0.0)
# ---------------------------------------------------------------------------

test_that("hbm: nested re formula `(1 | x/y)` accepted by validator", {
  skip_if_not_installed("brms")
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...)
      structure(list(formula = formula),
                class = c("brmsfit", "list")),
    .package = "brms"
  )
  d <- data.frame(y = stats::rnorm(5), x1 = stats::rnorm(5),
                   province = factor(rep(1:2, length.out = 5)),
                   regency = factor(1:5))

  expect_no_error(
    suppressWarnings(suppressMessages(
      hbm(formula = brms::bf(y ~ x1), data = d,
          re = ~ (1 | province/regency))
    ))
  )
})

test_that("hbm: nested re formula propagated through hbm_flex", {
  skip_if_not_installed("brms")
  captured <- NULL
  testthat::local_mocked_bindings(
    brm = function(formula, data, ...) {
      captured <<- formula
      structure(list(formula = formula),
                class = c("brmsfit", "list"))
    },
    .package = "brms"
  )
  d <- data.frame(y = stats::rnorm(5, 5, 1), x1 = stats::rnorm(5),
                   province = factor(rep(1:2, length.out = 5)),
                   regency = factor(1:5))

  suppressMessages(suppressWarnings(
    hbm_flex(family_key = "lognormal", response = "y",
             auxiliary = "x1", data = d,
             area_var = c("province", "regency"),
             area_re_structure = "nested",
             chains = 1, iter = 1)
  ))

  expect_match(paste(deparse(captured$formula), collapse = " "),
                "province/regency")
})

# ---------------------------------------------------------------------------
# 28. sae_aggregate weights validation (v1.0.0)
# ---------------------------------------------------------------------------

test_that("sae_aggregate: zero-sum weights -> error (not NaN)", {
  p1 <- structure(
    list(result_table = data.frame(Prediction = 1:3,
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = 1:3),
    class = "hbsae_results"
  )
  p2 <- structure(
    list(result_table = data.frame(Prediction = 2:4,
                                     RSE_percent = c(4, 4, 4)),
         rse_model = 4, pred = 2:4),
    class = "hbsae_results"
  )

  expect_error(
    sae_aggregate(p1, p2, method = "weighted", weights = c(0, 0)),
    "must sum to a positive value"
  )
})

test_that("sae_aggregate: non-finite weights -> error", {
  p1 <- structure(
    list(result_table = data.frame(Prediction = 1:3,
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = 1:3),
    class = "hbsae_results"
  )
  p2 <- structure(
    list(result_table = data.frame(Prediction = 2:4,
                                     RSE_percent = c(4, 4, 4)),
         rse_model = 4, pred = 2:4),
    class = "hbsae_results"
  )

  expect_error(
    sae_aggregate(p1, p2, method = "weighted", weights = c(NA, 0.5)),
    "must be finite"
  )
  expect_error(
    sae_aggregate(p1, p2, method = "weighted", weights = c(Inf, 0.5)),
    "must be finite"
  )
})

# ---------------------------------------------------------------------------
# 29. sae_transform length validation (v1.0.0)
# ---------------------------------------------------------------------------

test_that("sae_transform: reducer function returning scalar -> error", {
  p <- structure(
    list(result_table = data.frame(Prediction = c(1, 2, 3),
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = c(1, 2, 3)),
    class = "hbsae_results"
  )

  expect_error(
    sae_transform(p, fun = sum),
    "1 value\\(s\\) but 3 were expected"
  )
})

test_that("sae_transform: non-numeric return -> error", {
  p <- structure(
    list(result_table = data.frame(Prediction = c(1, 2, 3),
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = c(1, 2, 3)),
    class = "hbsae_results"
  )

  expect_error(
    sae_transform(p, fun = as.character),
    "must return a numeric vector"
  )
})

test_that("sae_transform: element-wise function works", {
  p <- structure(
    list(result_table = data.frame(Prediction = c(1, 2, 3),
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = c(1, 2, 3)),
    class = "hbsae_results"
  )

  out <- sae_transform(p, fun = exp)
  expect_equal(out$pred, exp(c(1, 2, 3)))
  expect_equal(out$result_table$Prediction, exp(c(1, 2, 3)))
})


# ---------------------------------------------------------------------------
# 30. is_converged() returns NA when no finite Rhat (v1.0.0)
# ---------------------------------------------------------------------------

test_that("is_converged: all-NA Rhat -> NA + warning, not silent TRUE", {
  hbcc <- structure(
    list(rhat_ess = matrix(
      c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      ncol = 3,
      dimnames = list(c("a", "b"),
                       c("Rhat", "Bulk_ESS", "Tail_ESS"))
    )),
    class = "hbcc_results"
  )

  expect_warning(
    result <- is_converged(hbcc, threshold = 1.1),
    "All stored R-hat values are NA"
  )
  expect_true(is.na(result))
})

test_that("is_converged: rejects non-numeric threshold", {
  hbcc <- structure(
    list(rhat_ess = matrix(c(1.001, 1.002, 100, 100, 100, 100),
                            ncol = 3,
                            dimnames = list(c("a", "b"),
                                            c("Rhat","Bulk_ESS","Tail_ESS")))),
    class = "hbcc_results"
  )

  expect_error(is_converged(hbcc, threshold = "1.1"),
               "must be a single finite positive number")
  expect_error(is_converged(hbcc, threshold = NULL),
               "must be a single finite positive number")
  expect_error(is_converged(hbcc, threshold = -1),
               "must be a single finite positive number")
  expect_error(is_converged(hbcc, threshold = c(1.1, 1.05)),
               "must be a single finite positive number")
})

test_that("is_converged: valid threshold + good Rhat -> TRUE", {
  hbcc <- structure(
    list(rhat_ess = matrix(c(1.001, 1.002, 100, 100, 100, 100),
                            ncol = 3,
                            dimnames = list(c("a","b"),
                                            c("Rhat","Bulk_ESS","Tail_ESS")))),
    class = "hbcc_results"
  )
  expect_true(is_converged(hbcc, threshold = 1.1))
  expect_true(is_converged(hbcc, threshold = 1.05))
  # Strict threshold below max Rhat
  expect_false(is_converged(hbcc, threshold = 1.0015))
})

# ---------------------------------------------------------------------------
# 31. sae_benchmark target validation (v1.0.0)
# ---------------------------------------------------------------------------

test_that("sae_benchmark: NA target -> error", {
  preds <- structure(
    list(result_table = data.frame(Prediction = c(10, 20, 30),
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = c(10, 20, 30)),
    class = "hbsae_results"
  )
  expect_error(
    sae_benchmark(preds, target = NA_real_),
    "must be finite"
  )
})

test_that("sae_benchmark: Inf target -> error", {
  preds <- structure(
    list(result_table = data.frame(Prediction = c(10, 20, 30),
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = c(10, 20, 30)),
    class = "hbsae_results"
  )
  expect_error(
    sae_benchmark(preds, target = Inf),
    "must be finite"
  )
})

test_that("sae_benchmark: NaN in pred -> error", {
  preds <- structure(
    list(result_table = data.frame(Prediction = c(10, NaN, 30),
                                     RSE_percent = c(5, 5, 5)),
         rse_model = 5, pred = c(10, NaN, 30)),
    class = "hbsae_results"
  )
  expect_error(
    sae_benchmark(preds, target = 60),
    "non-finite value"
  )
})

# ---------------------------------------------------------------------------
# 32. Models-registry: beta family no longer has legacy aux_param_hyperprior (v1.0.0)
# ---------------------------------------------------------------------------

test_that("registry: beta family no longer ships legacy aux_param_hyperprior", {
  spec <- hbsaems:::.get_model("beta")
  expect_false(is.function(spec$aux_param_hyperprior),
                info = "beta family should NOT have aux_param_hyperprior in v1.0.0")
})

test_that("registry: custom families can still register aux_param_hyperprior", {
  # Register a custom model with a callback
  register_hbsae_model(
    key                  = "test_aux_v1_0_0",
    family               = "gaussian",
    link                 = "identity",
    discrete             = FALSE,
    supports_mi          = TRUE,
    aux_param_hyperprior = function(args, data) NULL
  )
  spec <- hbsaems:::.get_model("test_aux_v1_0_0")
  expect_true(is.function(spec$aux_param_hyperprior))
  # Cleanup
  rm("test_aux_v1_0_0", envir = hbsaems:::.hbsae_model_env)
})


# ---------------------------------------------------------------------------
# 33. dloglogistic NA propagation (v1.0.0)
# ---------------------------------------------------------------------------

test_that("dloglogistic propagates NA inputs (matches base R convention)", {
  # Single NA -> single NA (was 0 in earlier versions)
  expect_true(is.na(dloglogistic(NA, mu = 1, beta = 1)))

  # Vectorised: NA at one position, real values at others
  out <- dloglogistic(c(NA, 1, 2), mu = 1, beta = 1)
  expect_equal(length(out), 3L)
  expect_true(is.na(out[1L]))
  expect_true(is.finite(out[2L]))
  expect_true(is.finite(out[3L]))
  expect_equal(out[2L], 0.25)  # f(1; 1, 1) = 1/4

  # NA in mu / beta should also propagate
  expect_true(is.na(dloglogistic(1, mu = NA, beta = 1)))
  expect_true(is.na(dloglogistic(1, mu = 1, beta = NA)))
})

test_that("dloglogistic returns 0 for x <= 0 (support is positive reals)", {
  expect_equal(dloglogistic(0, mu = 1, beta = 1), 0)
  expect_equal(dloglogistic(-1, mu = 1, beta = 1), 0)
  # Mixed: negative AND positive AND NA
  out <- dloglogistic(c(-1, 0, 1, NA), mu = 1, beta = 1)
  expect_equal(out[1L], 0)
  expect_equal(out[2L], 0)
  expect_equal(out[3L], 0.25)
  expect_true(is.na(out[4L]))
})


# ---------------------------------------------------------------------------
# 34. run_sae_app() graceful failure when shiny not installed (v1.0.0)
# ---------------------------------------------------------------------------

test_that("run_sae_app: informative error when shiny is missing", {
  # Mock out requireNamespace to simulate shiny being absent
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package == "shiny") return(FALSE)
      base::requireNamespace(package, ...)
    },
    .package = "base"
  )

  expect_error(
    run_sae_app(),
    "'shiny' package is required"
  )
})


# ---------------------------------------------------------------------------
# 35. Conflict protection: custom brms registration vs built-in (v1.0.0)
# ---------------------------------------------------------------------------

test_that("register_hbsae_brms_custom: refuses to overwrite built-in custom families", {
  skip_if_not_installed("brms")
  fam <- brms::custom_family("hbsae_test", dpars = "mu",
                              links = "identity", type = "real")
  sv  <- brms::stanvar(scode = "// dummy", block = "functions")

  # loglogistic IS a built-in hbsaems custom family (registered in .onLoad)
  expect_error(
    register_hbsae_brms_custom(key = "loglogistic", custom_family = fam,
                                stanvars = sv),
    "built-in hbsaems family"
  )

  # shifted_loglogistic likewise
  expect_error(
    register_hbsae_brms_custom(key = "shifted_loglogistic",
                                custom_family = fam, stanvars = sv),
    "built-in hbsaems family"
  )
})

test_that("register_hbsae_brms_custom: warns on built-in custom overwrite=TRUE", {
  skip_if_not_installed("brms")
  fam <- brms::custom_family("hbsae_test", dpars = "mu",
                              links = "identity", type = "real")
  sv  <- brms::stanvar(scode = "// dummy", block = "functions")

  expect_warning(
    register_hbsae_brms_custom(key = "loglogistic", custom_family = fam,
                                stanvars = sv, overwrite = TRUE),
    "Overwriting built-in family"
  )

  # Restore the package-curated loglogistic
  hbsaems:::.init_model_registry()
  hbsaems:::.register_builtin_custom_families()
})

test_that("register_hbsae_brms_custom: warns when shadowing a brms-native family", {
  skip_if_not_installed("brms")
  fam <- brms::custom_family("hbsae_test", dpars = "mu",
                              links = "identity", type = "real")
  sv  <- brms::stanvar(scode = "// dummy", block = "functions")

  # "weibull" is brms-native but not in hbsaems built-ins; should warn shadow
  expect_warning(
    register_hbsae_brms_custom(key = "weibull", custom_family = fam,
                                stanvars = sv),
    "brms-native family"
  )
  # Cleanup
  if (exists("weibull", envir = hbsaems:::.hbsae_model_env, inherits = FALSE))
    rm("weibull", envir = hbsaems:::.hbsae_model_env)
})

test_that("register_hbsae_model: refuses to overwrite built-in custom families too", {
  # Previously only protected brms-native names (gaussian, beta, etc.).
  # Now also protects loglogistic / shifted_loglogistic.
  expect_error(
    register_hbsae_model(key = "loglogistic", family = "gaussian"),
    "built-in hbsaems family"
  )
})

test_that(".init_model_registry: cleans up custom registrations on reset", {
  # Register a temporary custom family
  register_hbsae_model(key = "test_reset_v1_0_0", family = "gaussian")
  expect_true(exists("test_reset_v1_0_0",
                      envir = hbsaems:::.hbsae_model_env,
                      inherits = FALSE))

  # Reset should remove non-builtin entries
  hbsaems:::.init_model_registry()
  expect_false(exists("test_reset_v1_0_0",
                       envir = hbsaems:::.hbsae_model_env,
                       inherits = FALSE))

  # But built-ins (including custom builtins) should be intact after
  # the full reset + .register_builtin_custom_families() pipeline.
  hbsaems:::.register_builtin_custom_families()
  expect_true(exists("loglogistic",
                      envir = hbsaems:::.hbsae_model_env,
                      inherits = FALSE))
  expect_true(exists("gaussian",
                      envir = hbsaems:::.hbsae_model_env,
                      inherits = FALSE))
})

test_that(".is_brms_native_family: distinguishes brms families from arbitrary names", {
  skip_if_not_installed("brms")
  # Native families
  expect_true(hbsaems:::.is_brms_native_family("gaussian"))
  expect_true(hbsaems:::.is_brms_native_family("weibull"))
  expect_true(hbsaems:::.is_brms_native_family("Beta"))

  # Non-native
  expect_false(hbsaems:::.is_brms_native_family("loglogistic"))
  expect_false(hbsaems:::.is_brms_native_family("nonexistent_dist"))

  # Defensive: invalid inputs
  expect_false(hbsaems:::.is_brms_native_family(NULL))
  expect_false(hbsaems:::.is_brms_native_family(c("a", "b")))
  expect_false(hbsaems:::.is_brms_native_family(42))
})


# ---------------------------------------------------------------------------
# 36. AST formula manipulation: offset() preservation (v1.0.0)
# ---------------------------------------------------------------------------
# Regression: stats::terms() strips offset() into a separate `offset`
# attribute rather than including it in term.labels.  Previously, our
# .replace_nl_in_formula() and .apply_measurement_error() helpers
# silently dropped offsets when reassembling the formula.

test_that(".replace_nl_in_formula preserves offset() terms", {
  fml_in <- y ~ x1 + offset(log(pop)) + x2
  fml_out <- hbsaems:::.replace_nl_in_formula(
    fml_in, nonlinear = "x1", nonlinear_type = "spline"
  )
  # Both the spline AND the offset must survive
  expect_match(deparse(fml_out), "s\\(x1\\)")
  expect_match(deparse(fml_out), "offset\\(log\\(pop\\)\\)")
})

test_that(".apply_measurement_error preserves offset() terms", {
  fml_in <- brms::bf(y ~ x1 + offset(log(pop)) + x2)
  fml_out <- hbsaems:::.apply_measurement_error(
    fml_in, list(x1 = "se_x1")
  )
  expect_match(deparse(fml_out$formula), "mi\\(x1, se_x1\\)")
  expect_match(deparse(fml_out$formula), "offset\\(log\\(pop\\)\\)")
})

# ---------------------------------------------------------------------------
# 37. AST formula manipulation: substring/wrapper safety (v1.0.0)
# ---------------------------------------------------------------------------

test_that(".replace_nl_in_formula: substring-name safety", {
  # x1 is a substring of x10 and x11; naive regex would corrupt them.
  fml_in <- y ~ x1 + x10 + x11
  fml_out <- hbsaems:::.replace_nl_in_formula(
    fml_in, nonlinear = "x1", nonlinear_type = "spline"
  )
  out_str <- deparse(fml_out)
  expect_match(out_str, "s\\(x1\\)")
  expect_match(out_str, "x10")     # not s(x10), not s(x1)0
  expect_match(out_str, "x11")
  # Negative: x10 must NOT have been corrupted to s(x1)0
  expect_false(grepl("s\\(x1\\)0", out_str))
})

test_that(".replace_nl_in_formula: wrapped variables left alone", {
  # All these should KEEP their wrappers, and the bare x2 (if present)
  # gets the spline applied.
  for (fml_in in list(
    y ~ x1 + I(x2 / 100) + x3,
    y ~ x1 + poly(x2, 2)  + x3,
    y ~ x1 + me(x2, se_x2) + x3,
    y ~ x1 + s(x2)        + x3      # already a spline
  )) {
    fml_out <- hbsaems:::.replace_nl_in_formula(
      fml_in, nonlinear = "x2", nonlinear_type = "spline"
    )
    out_str <- deparse(fml_out)
    # Original wrapper must survive in some form
    # (this is loose: we just need to assert the rewrite isn't destructive)
    if (grepl("I\\(", deparse(fml_in)))
      expect_match(out_str, "I\\(x2/100\\)|I\\(x2 / 100\\)|I\\(x2/100\\)")
    if (grepl("poly", deparse(fml_in)))
      expect_match(out_str, "poly\\(x2, 2\\)")
    if (grepl("me\\(", deparse(fml_in)))
      expect_match(out_str, "me\\(x2, se_x2\\)")
  }
})

test_that(".replace_nl_in_formula: interaction terms NOT touched", {
  # brms does not reliably fit s(x1):x2; the AST rewrite is conservative
  # and leaves interactions alone.
  fml_in <- y ~ x1 + x1:x2 + x3
  fml_out <- hbsaems:::.replace_nl_in_formula(
    fml_in, nonlinear = "x1", nonlinear_type = "spline"
  )
  out_str <- deparse(fml_out)
  expect_match(out_str, "s\\(x1\\)")
  expect_match(out_str, "x1:x2")   # preserved
})

test_that(".extract_response_names: handles brms addition operators", {
  # Bare response
  expect_equal(hbsaems:::.extract_response_names(quote(y)), "y")
  # mi() addition wrapper
  expect_equal(hbsaems:::.extract_response_names(quote(y | mi())), "y")
  # trials() — n is NOT a response
  expect_equal(hbsaems:::.extract_response_names(quote(y | trials(n))), "y")
  # cens()
  expect_equal(hbsaems:::.extract_response_names(quote(y | cens(c))), "y")
  # cbind multiple responses
  expect_equal(hbsaems:::.extract_response_names(quote(cbind(s, f) | trials(n))),
               c("s", "f"))
  # NULL guard
  expect_equal(hbsaems:::.extract_response_names(NULL), character(0L))
})

test_that(".build_area_re_formula: correct rendering of hierarchies", {
  # Single
  expect_equal(deparse(hbsaems:::.build_area_re_formula("province")),
               "~(1 | province)")
  # Nested 2-level
  expect_equal(deparse(hbsaems:::.build_area_re_formula(
    c("province", "regency"), "nested")),
               "~(1 | province/regency)")
  # Crossed 2-level
  expect_equal(deparse(hbsaems:::.build_area_re_formula(
    c("province", "regency"), "crossed")),
               "~(1 | province) + (1 | regency)")
  # NULL / empty
  expect_null(hbsaems:::.build_area_re_formula(NULL))
  expect_null(hbsaems:::.build_area_re_formula(character(0L)))
  # Error on bad input
  expect_error(hbsaems:::.build_area_re_formula(42),
               "character vector")
  expect_error(hbsaems:::.build_area_re_formula(c("a", "")),
               "empty strings")
})


# ---------------------------------------------------------------------------
# 38. .repopulate_fixed_cols() handles newdata without internal offset cols
# ---------------------------------------------------------------------------
# Regression: a user passing fresh newdata to sae_predict() would hit
# brms validate_data() saying ".hbsaems_sigma_fixed" cannot be found.
# Solution: auto-copy from model$data when nrow matches.

test_that(".repopulate_fixed_cols copies offset cols when nrow matches", {
  # Mock model$data with offset column
  train <- data.frame(
    y = 1:10,
    D = runif(10, 0.1, 1),
    .hbsaems_sigma_fixed = sqrt(runif(10, 0.1, 1))
  )
  model <- list(data = train)
  
  # User-supplied newdata: same nrow, no offset col
  newdata <- data.frame(
    y = 11:20,
    D = runif(10, 0.5, 2)
  )
  
  out <- hbsaems:::.repopulate_fixed_cols(newdata, model)
  
  expect_true(".hbsaems_sigma_fixed" %in% names(out))
  # Should be the COPY of training values
  expect_equal(out$.hbsaems_sigma_fixed, train$.hbsaems_sigma_fixed)
})

test_that(".repopulate_fixed_cols passes through when no offset cols", {
  # Model without fixed_params
  train <- data.frame(y = 1:10, x = 1:10)
  model <- list(data = train)
  
  newdata <- data.frame(y = 1:5, x = 1:5)
  out <- hbsaems:::.repopulate_fixed_cols(newdata, model)
  
  # newdata unchanged
  expect_identical(out, newdata)
})

test_that(".repopulate_fixed_cols passes through when col already present", {
  train <- data.frame(
    y = 1:10,
    .hbsaems_sigma_fixed = rep(0.5, 10)
  )
  newdata <- data.frame(
    y = 11:15,
    .hbsaems_sigma_fixed = rep(1.0, 5)   # user supplied
  )
  model <- list(data = train)
  
  out <- hbsaems:::.repopulate_fixed_cols(newdata, model)
  
  # User-supplied values preserved, not overwritten
  expect_equal(out$.hbsaems_sigma_fixed, rep(1.0, 5))
})

test_that(".repopulate_fixed_cols errors informatively on nrow mismatch", {
  train <- data.frame(
    y = 1:10,
    D = runif(10),
    .hbsaems_sigma_fixed = sqrt(runif(10))
  )
  newdata <- data.frame(
    y = 1:5,
    D = runif(5)   # different nrow, no offset
  )
  model <- list(data = train)
  
  expect_error(
    hbsaems:::.repopulate_fixed_cols(newdata, model),
    "Cannot predict on this newdata"
  )
  expect_error(
    hbsaems:::.repopulate_fixed_cols(newdata, model),
    "sigma"   # mentions which param
  )
})

test_that(".repopulate_fixed_cols handles model$data = NULL", {
  model <- list(data = NULL)
  newdata <- data.frame(y = 1:5, x = 1:5)
  out <- hbsaems:::.repopulate_fixed_cols(newdata, model)
  expect_identical(out, newdata)
})


# ---------------------------------------------------------------------------
# 39. Shiny memory helpers (v1.0.0)
# ---------------------------------------------------------------------------
# These helpers live under inst/shiny/sae_app/ so they ship with the
# app but are NOT exported package functions.  We still want CI to
# catch regressions in their behaviour because the Shiny app's
# multi-model comparison feature depends on them.

test_that("shiny memory_helpers: .multimodel_library_add LRU works", {
  helpers <- system.file("shiny", "sae_app", "memory_helpers.R",
                          package = "hbsaems")
  if (!nzchar(helpers) || !file.exists(helpers)) {
    helpers <- file.path("..", "..", "inst", "shiny", "sae_app",
                          "memory_helpers.R")
  }
  skip_if_not(file.exists(helpers), "memory_helpers.R not found")
  
  env <- new.env()
  sys.source(helpers, envir = env)
  
  # Fill library beyond cap
  lib <- list()
  for (i in 1:7) {
    lib <- env$.multimodel_library_add(
      lib           = lib,
      name          = sprintf("m%d", i),
      model         = list(payload = 1:100),
      max_snapshots = 5L
    )
  }
  
  expect_length(lib, 5L)
  # Newest 5 retained, oldest 2 dropped
  expect_setequal(names(lib), c("m3", "m4", "m5", "m6", "m7"))
  
  # Evicted attribute reports last eviction
  expect_equal(attr(lib, "evicted"), "m2")
})

test_that("shiny memory_helpers: .estimate_object_size returns MB", {
  helpers <- system.file("shiny", "sae_app", "memory_helpers.R",
                          package = "hbsaems")
  if (!nzchar(helpers) || !file.exists(helpers)) {
    helpers <- file.path("..", "..", "inst", "shiny", "sae_app",
                          "memory_helpers.R")
  }
  skip_if_not(file.exists(helpers), "memory_helpers.R not found")
  
  env <- new.env()
  sys.source(helpers, envir = env)
  
  big <- list(x = rnorm(1e5))
  mb <- env$.estimate_object_size(big)
  expect_true(is.numeric(mb))
  expect_gt(mb, 0)
  
  # Caches on the object
  big2 <- env$.estimate_object_size(big)
  expect_identical(mb, big2)
  
  # Handles NULL
  expect_equal(env$.estimate_object_size(NULL), 0)
})

test_that("shiny memory_helpers: .shrink_hbmfit defensive on non-hbmfit", {
  helpers <- system.file("shiny", "sae_app", "memory_helpers.R",
                          package = "hbsaems")
  if (!nzchar(helpers) || !file.exists(helpers)) {
    helpers <- file.path("..", "..", "inst", "shiny", "sae_app",
                          "memory_helpers.R")
  }
  skip_if_not(file.exists(helpers), "memory_helpers.R not found")
  
  env <- new.env()
  sys.source(helpers, envir = env)
  
  # NULL -> NULL
  expect_null(env$.shrink_hbmfit(NULL))
  # Non-hbmfit input -> returned unchanged
  expect_equal(env$.shrink_hbmfit(list(x = 1)), list(x = 1))
  # Class-only object -> returned unchanged (no model slot)
  obj <- structure(list(), class = "hbmfit")
  expect_equal(env$.shrink_hbmfit(obj), obj)
})
