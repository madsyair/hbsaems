# tests/testthat/test-workflow-1-1-0.R
# CRAN-safe tests for the 1.1.0 workflow improvements:
#   - sae_predict() predict_type with epred default
#   - hbm() NUTS default merge (adapt_delta = 0.95)
#   - model_compare Pareto-k / reloo helpers
# Stan-dependent success paths live in the dev tier.

test_that(".merge_nuts_defaults fills unset keys but respects the user", {
  d <- hbsaems:::.merge_nuts_defaults(list())
  expect_equal(d$adapt_delta, 0.95)
  expect_equal(d$max_treedepth, 12L)
  u <- hbsaems:::.merge_nuts_defaults(list(adapt_delta = 0.99))
  expect_equal(u$adapt_delta, 0.99)     # user value preserved
  expect_equal(u$max_treedepth, 12L)    # other key still filled
  n <- hbsaems:::.merge_nuts_defaults(NULL)
  expect_equal(n$adapt_delta, 0.95)
})

test_that("sae_predict() default predict_type is epred", {
  expect_identical(eval(formals(sae_predict)$predict_type)[1], "epred")
})

test_that("sae_predict() validates predict_type", {
  fake <- structure(list(model = structure(list(), class = "brmsfit"),
                         data = data.frame(y = 1:3), missing_method = NULL),
                    class = "hbmfit")
  expect_error(sae_predict(fake, predict_type = "bogus"),
               "should be one of")
})

test_that(".high_pareto_k flags k > 0.7 only", {
  lo <- list(diagnostics = list(pareto_k = c(0.1, 0.8, 0.5, 0.95)))
  expect_equal(hbsaems:::.high_pareto_k(lo), c(2L, 4L))
  expect_length(hbsaems:::.high_pareto_k(
    list(diagnostics = list(pareto_k = c(0.1, 0.2)))), 0L)
  expect_length(hbsaems:::.high_pareto_k(NULL), 0L)
})

test_that(".maybe_reloo warns on high-k when reloo not requested", {
  lo <- list(diagnostics = list(pareto_k = c(0.1, 0.8)))
  fakefit <- structure(list(), class = "brmsfit")
  expect_warning(
    hbsaems:::.maybe_reloo(lo, fakefit, moment_match = FALSE,
                           reloo_args = list()),
    "Pareto k > 0.7"
  )
  # no warning when k all fine
  ok <- list(diagnostics = list(pareto_k = c(0.1, 0.2)))
  expect_silent(
    hbsaems:::.maybe_reloo(ok, fakefit, moment_match = FALSE,
                           reloo_args = list())
  )
})

test_that(".maybe_reloo invokes brms::reloo when reloo_args supplied", {
  lo <- list(diagnostics = list(pareto_k = c(0.1, 0.85)))
  fakefit <- structure(list(), class = "brmsfit")
  called <- FALSE
  testthat::with_mocked_bindings(
    reloo = function(x, fit, ...) { called <<- TRUE; x },
    .package = "brms",
    {
      res <- hbsaems:::.maybe_reloo(lo, fakefit, moment_match = FALSE,
                                    reloo_args = list(chains = 1))
      expect_true(called)
      expect_identical(res, lo)
    }
  )
})

# ---- predict_type = "proportion" (binomial) -------------------------------

test_that("sae_predict() accepts predict_type = 'proportion'", {
  expect_true("proportion" %in% eval(formals(sae_predict)$predict_type))
})

test_that(".binomial_trials extracts the trials variable", {
  skip_if_not_installed("brms")
  fake <- structure(
    list(formula = brms::bf(y | trials(n) ~ x1, family = binomial())),
    class = "brmsfit")
  d <- data.frame(y = c(1, 2), n = c(10, 20), x1 = c(0, 1))
  expect_equal(hbsaems:::.binomial_trials(fake, d), c(10, 20))
})

test_that(".binomial_trials returns NULL when no trials term", {
  skip_if_not_installed("brms")
  fake <- structure(list(formula = brms::bf(y ~ x1)), class = "brmsfit")
  expect_null(hbsaems:::.binomial_trials(fake, data.frame(y = 1, x1 = 1)))
})

# ---- hbm_flex(family = ...) family-object resolution ----------------------

test_that(".resolve_family_to_key maps built-in family objects to keys", {
  skip_if_not_installed("brms")
  expect_equal(hbsaems:::.resolve_family_to_key(brms::brmsfamily("gaussian"))$key,
               "gaussian")
  expect_equal(hbsaems:::.resolve_family_to_key("binomial")$key, "binomial")
  # link carried on the object is preserved
  g <- brms::brmsfamily("gaussian", link = "log")
  expect_equal(hbsaems:::.resolve_family_to_key(g)$link, "log")
})

test_that(".resolve_family_to_key resolves registered custom families distinctly", {
  skip_if_not_installed("brms")
  ll <- brms_custom_loglogistic()$custom_family
  expect_equal(hbsaems:::.resolve_family_to_key(ll)$key, "loglogistic")
  sl <- tryCatch(brms_custom_shifted_loglogistic()$custom_family,
                 error = function(e) NULL)
  if (!is.null(sl))
    expect_equal(hbsaems:::.resolve_family_to_key(sl)$key, "shifted_loglogistic")
})

test_that(".resolve_family_to_key errors clearly for unregistered custom family", {
  skip_if_not_installed("brms")
  fake <- brms::custom_family("totallyfake", dpars = "mu",
                              links = "identity", type = "real")
  expect_error(hbsaems:::.resolve_family_to_key(fake),
               "not registered|register")
})

test_that(".resolve_family_to_key errors for unregistered native family", {
  skip_if_not_installed("brms")
  # 'weibull' is a real brms family but is intentionally NOT registered in
  # hbsaems, so resolving it must raise the informative error.
  expect_error(hbsaems:::.resolve_family_to_key(brms::weibull()),
               "not registered with hbsaems")
})

# ---- SAE-oriented families registered in v1.1.0 ---------------------------

test_that("new SAE families are registered", {
  for (k in c("gamma", "skew_normal", "student", "hurdle_lognormal"))
    expect_true(k %in% list_hbsae_models(),
                info = paste("missing registry key:", k))
})

test_that("gamma family rejects non-positive responses", {
  chk <- hbsaems:::.get_model("gamma")$response_check
  expect_true(chk(c(0.1, 2, 3)))
  expect_false(chk(c(-1, 2)))
  expect_false(chk(c(0, 2)))      # Gamma is y > 0 strictly
})

test_that("hurdle_lognormal allows zeros but not negatives", {
  chk <- hbsaems:::.get_model("hurdle_lognormal")$response_check
  expect_true(chk(c(0, 1, 2)))    # zeros allowed (the hurdle)
  expect_false(chk(c(-1, 2)))
})

test_that("student and skew_normal accept any real response", {
  expect_true(hbsaems:::.get_model("student")$response_check(c(-5, 0, 5)))
  expect_true(hbsaems:::.get_model("skew_normal")$response_check(c(-5, 0, 5)))
})

test_that("new families resolve from family objects to keys", {
  skip_if_not_installed("brms")
  expect_equal(hbsaems:::.resolve_family_to_key(brms::brmsfamily("Gamma"))$key,
               "gamma")
  expect_equal(hbsaems:::.resolve_family_to_key(brms::student())$key, "student")
  expect_equal(hbsaems:::.resolve_family_to_key(brms::skew_normal())$key,
               "skew_normal")
  expect_equal(
    hbsaems:::.resolve_family_to_key(brms::hurdle_lognormal())$key,
    "hurdle_lognormal")
})

# ---- family / family_key coexist without conflict (v1.1.0) ----------------

test_that("family and family_key are complementary, both accepted alone", {
  skip_if_not_installed("brms")
  # string key alone
  expect_equal(hbsaems:::.resolve_family_to_key("gaussian")$key, "gaussian")
  # family object alone
  expect_equal(hbsaems:::.resolve_family_to_key(gaussian())$key, "gaussian")
  # string passed via the family argument also resolves
  expect_equal(hbsaems:::.resolve_family_to_key("binomial")$key, "binomial")
})

test_that("supplying both family and family_key errors (redundant or clash)", {
  skip_if_not_installed("brms")
  d <- data.frame(y = rnorm(12), x1 = rnorm(12),
                  area = factor(rep(1:3, 4)))
  # same target -> redundant
  expect_error(
    hbm_flex(family = gaussian(), family_key = "gaussian",
             response = "y", auxiliary = "x1", data = d),
    "both refer to|Pass only one")
  # different targets -> clash
  expect_error(
    hbm_flex(family = gaussian(), family_key = "binomial",
             response = "y", auxiliary = "x1", data = d),
    "differ|Pass only one")
})

test_that("neither family nor family_key errors clearly", {
  d <- data.frame(y = rnorm(12), x1 = rnorm(12),
                  area = factor(rep(1:3, 4)))
  expect_error(
    hbm_flex(response = "y", auxiliary = "x1", data = d),
    "Provide a family")
})

# ---- family is now the primary argument; family_key is an alias -----------

test_that("family is the first/primary argument accepting string or object", {
  skip_if_not_installed("brms")
  # family arg is positionally first
  expect_identical(names(formals(hbm_flex))[1], "family")
  # family_key still present as a named alias
  expect_true("family_key" %in% names(formals(hbm_flex)))
})

test_that("family accepts a string exactly like the old family_key", {
  skip_if_not_installed("brms")
  expect_equal(hbsaems:::.resolve_family_to_key("gaussian")$key, "gaussian")
  expect_equal(hbsaems:::.resolve_family_to_key("binomial")$key, "binomial")
})

# ---- hbm(family = ...) alias for hb_sampling (v1.1.0) ---------------------

test_that("hbm() gains a family argument; hb_sampling retained", {
  fmls <- names(formals(hbm))
  expect_true("family" %in% fmls)
  expect_true("hb_sampling" %in% fmls)
})

test_that("hbm() rejects family + non-default hb_sampling together", {
  skip_if_not_installed("brms")
  d <- data.frame(y = rnorm(12), x1 = rnorm(12))
  expect_error(
    hbm(brms::bf(y ~ x1), family = gaussian(),
        hb_sampling = "binomial", data = d),
    "not both")
})

# ---- model_average forwards predict_type (NEWS accuracy) ------------------

test_that("model_average accepts and defaults predict_type", {
  fmls <- formals(model_average)
  expect_true("predict_type" %in% names(fmls))
  expect_equal(eval(fmls$predict_type)[1], "epred")
})

# ---- .validate_spatial_matrix() rowname/grouping check (v1.1.0) -----------

test_that(".validate_spatial_matrix checks CAR rownames vs grouping levels", {
  skip_if_not_installed("brms")
  M <- matrix(c(0,1,0,0,0,
                1,0,1,0,0,
                0,1,0,1,0,
                0,0,1,0,1,
                0,0,0,1,0), nrow = 5, byrow = TRUE)
  glv <- paste0("a", 1:5)
  rownames(M) <- colnames(M) <- glv

  # matching rownames -> returned unchanged
  expect_identical(
    hbsaems:::.validate_spatial_matrix(M, "car", grouping_levels = glv), M)

  # a permutation is allowed (brms reorders by name)
  Mp <- M; rownames(Mp) <- colnames(Mp) <- rev(glv)
  expect_silent(
    hbsaems:::.validate_spatial_matrix(Mp, "car", grouping_levels = glv))

  # mismatched rownames -> early, informative error
  Mw <- M; rownames(Mw) <- colnames(Mw) <- paste0("b", 1:5)
  expect_error(
    hbsaems:::.validate_spatial_matrix(Mw, "car", grouping_levels = glv),
    "do not match the levels")

  # NULL rownames with known levels -> error
  Mn <- M; rownames(Mn) <- colnames(Mn) <- NULL
  expect_error(
    hbsaems:::.validate_spatial_matrix(Mn, "car", grouping_levels = glv),
    "Row names are required")

  # backward-compatible: no grouping_levels -> no rowname check
  expect_silent(hbsaems:::.validate_spatial_matrix(Mw, "car"))
})

# ---- .validate_spatial_matrix rowname/grouping-level check (early, no fit) ----

test_that(".validate_spatial_matrix catches CAR rowname/level mismatch early", {
  skip_if_not_installed("brms")
  M <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(M) <- colnames(M) <- c("a", "b")

  # Correct levels pass.
  expect_silent(hbsaems:::.validate_spatial_matrix(M, "car",
                                                   grouping_levels = c("a", "b")))
  # Mismatched levels error early with an informative message.
  expect_error(
    hbsaems:::.validate_spatial_matrix(M, "car", grouping_levels = c("x", "y")),
    "do not match the levels")
  # Missing rownames error.
  M2 <- M; rownames(M2) <- NULL
  expect_error(
    hbsaems:::.validate_spatial_matrix(M2, "car", grouping_levels = c("a", "b")),
    "Row names are required")
  # NULL grouping_levels is backward-compatible (no rowname check).
  expect_silent(hbsaems:::.validate_spatial_matrix(M, "car",
                                                   grouping_levels = NULL))
})
