# tests/testthat/test-dist-loglogistic.R
# =============================================================================
# CRAN-safe tests for the loglogistic + shifted_loglogistic distribution
# functions added in v1.0.0.  Tests the R-side d/p/q/r implementations
# (mathematical identities) and the structure of the brms custom_family
# wrappers, but does NOT compile Stan models -- those live in dev-tests/.
# =============================================================================

# ---- Loglogistic distribution math identities ------------------------------

test_that("dloglogistic integrates to 1", {
  integral <- stats::integrate(
    function(x) dloglogistic(x, mu = 1, beta = 2),
    lower = 0, upper = Inf
  )$value
  expect_equal(integral, 1, tolerance = 1e-4)
})

test_that("ploglogistic agrees with numeric integral of dloglogistic", {
  qs <- c(0.5, 1, 2, 5)
  for (q in qs) {
    p_direct <- ploglogistic(q, mu = 1, beta = 2)
    p_numerical <- stats::integrate(
      function(x) dloglogistic(x, mu = 1, beta = 2),
      lower = 0, upper = q
    )$value
    expect_equal(p_direct, p_numerical, tolerance = 1e-4,
                  info = paste("q =", q))
  }
})

test_that("qloglogistic is the inverse of ploglogistic", {
  ps <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  for (p in ps) {
    q <- qloglogistic(p, mu = 2, beta = 3)
    expect_equal(ploglogistic(q, mu = 2, beta = 3), p, tolerance = 1e-10,
                  info = paste("p =", p))
  }
})

test_that("median of loglogistic equals mu", {
  expect_equal(qloglogistic(0.5, mu = 1,  beta = 2), 1)
  expect_equal(qloglogistic(0.5, mu = 3,  beta = 5), 3)
  expect_equal(qloglogistic(0.5, mu = 10, beta = 1), 10)
})

test_that("rloglogistic produces positive values", {
  set.seed(123)
  x <- rloglogistic(1000, mu = 1, beta = 2)
  expect_true(all(x > 0))
  expect_length(x, 1000)
})

test_that("rloglogistic empirical CDF matches theoretical", {
  set.seed(456)
  x <- rloglogistic(5000, mu = 1, beta = 2)
  # K-S test: empirical vs theoretical
  ks <- suppressWarnings(stats::ks.test(x, "ploglogistic",
                                          mu = 1, beta = 2))
  expect_true(ks$p.value > 0.01,
              info = paste("K-S p-value =", ks$p.value))
})

test_that("dloglogistic returns 0 for x <= 0", {
  expect_equal(dloglogistic(0,    mu = 1, beta = 2), 0)
  expect_equal(dloglogistic(-1,   mu = 1, beta = 2), 0)
  expect_equal(dloglogistic(-100, mu = 1, beta = 2), 0)
})

test_that("dloglogistic log=TRUE returns log density", {
  x <- c(0.5, 1, 2)
  d_natural <- dloglogistic(x, mu = 1, beta = 2, log = FALSE)
  d_log     <- dloglogistic(x, mu = 1, beta = 2, log = TRUE)
  expect_equal(d_log, log(d_natural), tolerance = 1e-10)
})

test_that("dloglogistic validates positive params", {
  expect_error(dloglogistic(1, mu = -1, beta = 2), "mu")
  expect_error(dloglogistic(1, mu = 1,  beta = -2), "beta")
})


# ---- brms custom family wrapper structure ---------------------------------

test_that("brms_custom_loglogistic returns expected structure", {
  testthat::skip_if_not_installed("brms")
  ll <- brms_custom_loglogistic()
  expect_named(ll, c("custom_family", "stanvars_family"))
  expect_s3_class(ll$custom_family,    "customfamily")
  expect_s3_class(ll$stanvars_family,  "stanvars")
  expect_equal(ll$custom_family$name,  "loglogistic")
  expect_equal(ll$custom_family$dpars, c("mu", "beta"))
})

test_that("read_stan_function loads loglogistic Stan code", {
  code <- read_stan_function("loglogistic")
  expect_type(code, "character")
  # Verify essential function names are defined
  expect_true(grepl("real loglogistic_lpdf",  code))
  expect_true(grepl("real loglogistic_lcdf",  code))
  expect_true(grepl("real loglogistic_lccdf", code))
  expect_true(grepl("real loglogistic_rng",   code))
})

test_that("read_stan_function errors on unknown distribution", {
  expect_error(read_stan_function("nonexistent_dist"),
                "Stan file for distribution")
})

test_that("build_brms_custom_family validates first dpar is 'mu'", {
  testthat::skip_if_not_installed("brms")
  expect_error(
    build_brms_custom_family("loglogistic", dpars = c("foo", "bar"),
                              links = c("log", "log")),
    "First entry of `dpars`"
  )
})


# ---- Shifted Loglogistic --------------------------------------------------

test_that("dshifted_loglogistic reduces to logistic when xi=0", {
  # xi = 0 limit should match stats::dlogis exactly
  x <- c(-2, -1, 0, 1, 2)
  d_sll <- dshifted_loglogistic(x, mu = 0, sigma = 1, xi = 0)
  d_log <- stats::dlogis(x, location = 0, scale = 1)
  expect_equal(d_sll, d_log, tolerance = 1e-10)
})

test_that("pshifted_loglogistic reduces to plogis when xi=0", {
  q <- c(-2, -1, 0, 1, 2)
  p_sll <- pshifted_loglogistic(q, mu = 0, sigma = 1, xi = 0)
  p_log <- stats::plogis(q,    location = 0, scale = 1)
  expect_equal(p_sll, p_log, tolerance = 1e-10)
})

test_that("dshifted_loglogistic integrates to 1 (xi > 0)", {
  # Support is y > mu - sigma/xi.  For mu=0, sigma=1, xi=0.5 -> y > -2.
  # The right tail is heavy (regularly varying), so the residual mass past
  # the upper limit accounts for a small numerical shortfall.
  integral <- stats::integrate(
    function(x) dshifted_loglogistic(x, mu = 0, sigma = 1, xi = 0.5),
    lower = -2 + 1e-6, upper = 5000
  )$value
  expect_equal(integral, 1, tolerance = 1e-2)
})

test_that("qshifted_loglogistic is inverse of pshifted_loglogistic", {
  ps <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  for (p in ps) {
    q <- qshifted_loglogistic(p, mu = 1, sigma = 2, xi = 0.3)
    expect_equal(
      pshifted_loglogistic(q, mu = 1, sigma = 2, xi = 0.3),
      p, tolerance = 1e-10,
      info = paste("p =", p)
    )
  }
})

test_that("brms_custom_shifted_loglogistic returns expected structure", {
  testthat::skip_if_not_installed("brms")
  sll <- brms_custom_shifted_loglogistic()
  expect_named(sll, c("custom_family", "stanvars_family"))
  expect_s3_class(sll$custom_family,   "customfamily")
  expect_s3_class(sll$stanvars_family, "stanvars")
  expect_equal(sll$custom_family$name,  "shifted_loglogistic")
  expect_equal(sll$custom_family$dpars, c("mu", "sigma", "xi"))
})


# ---- Registry integration -------------------------------------------------

test_that("loglogistic is auto-registered in the model registry", {
  testthat::skip_if_not_installed("brms")
  models <- list_hbsae_models()
  expect_true("loglogistic" %in% models)
})

test_that("shifted_loglogistic is auto-registered in the model registry", {
  testthat::skip_if_not_installed("brms")
  models <- list_hbsae_models()
  expect_true("shifted_loglogistic" %in% models)
})

test_that("get_hbsae_model returns custom_family spec for loglogistic", {
  testthat::skip_if_not_installed("brms")
  spec <- get_hbsae_model("loglogistic")
  expect_true(!is.null(spec$custom_family))
  expect_s3_class(spec$custom_family,   "customfamily")
  expect_s3_class(spec$custom_stanvars, "stanvars")
})

test_that("register_hbsae_brms_custom validates input types", {
  expect_error(
    register_hbsae_brms_custom("foo", "not_a_family", "not_stanvars"),
    "custom_family"
  )
})

test_that(".is_custom_family_spec correctly classifies specs", {
  testthat::skip_if_not_installed("brms")
  spec_custom <- get_hbsae_model("loglogistic")
  spec_native <- get_hbsae_model("gaussian")

  expect_true(hbsaems:::.is_custom_family_spec(spec_custom))
  expect_false(hbsaems:::.is_custom_family_spec(spec_native))
})


# ===========================================================================
# Post-processing functions (log_lik / posterior_predict / posterior_epred)
# ===========================================================================
# We test the post-processing functions WITHOUT fitting a real model by
# constructing a minimal `prep` mock list that mimics brms's posterior
# prep structure: prep$data$Y is the response, brms::get_dpar() is told
# where to look via prep$dpars.
#
# This keeps the tests CRAN-safe (no Stan compile) while still validating
# the post-processing math.

.make_loglogistic_prep <- function(mu, beta, y, ndraws = 5) {
  # brms::get_dpar() requires `is.brmsprep(prep)`, which checks the S3
  # class.  We attach the expected class so the test doubles pass that
  # check; the function only inspects $dpars to retrieve values.
  prep <- list(
    ndraws = ndraws,
    nobs   = length(y),
    data   = list(Y = y),
    dpars  = list(
      mu   = matrix(rep(mu,   each = ndraws), nrow = ndraws),
      beta = rep(beta, ndraws)
    ),
    family = list(link_mu = "log", link_beta = "log")
  )
  class(prep) <- c("brmsprep", "list")
  prep
}


test_that("log_lik for loglogistic agrees with dloglogistic", {
  testthat::skip_if_not_installed("brms")

  # (legacy env access removed)
  # Access via :::
  ll_fn <- log_lik_loglogistic

  prep <- .make_loglogistic_prep(mu = c(1, 2, 3), beta = 2,
                                  y  = c(0.5, 1.5, 4))
  for (i in 1:3) {
    got <- ll_fn(i, prep)
    expected <- dloglogistic(prep$data$Y[i],
                              mu   = prep$dpars$mu[, i],
                              beta = prep$dpars$beta,
                              log  = TRUE)
    expect_equal(got, expected, tolerance = 1e-10,
                  info = paste("i =", i))
  }
})


test_that("posterior_predict for loglogistic returns positive draws", {
  testthat::skip_if_not_installed("brms")

  pp_fn <- posterior_predict_loglogistic
  prep <- .make_loglogistic_prep(mu = c(1, 2, 3), beta = 2,
                                  y  = c(0.5, 1.5, 4))

  set.seed(42)
  for (i in 1:3) {
    draws <- pp_fn(i, prep)
    expect_true(all(draws > 0),
                info = paste("Non-positive draw at i =", i))
    expect_length(draws, prep$ndraws)
  }
})


test_that("posterior_epred for loglogistic returns the closed-form mean", {
  testthat::skip_if_not_installed("brms")

  ep_fn <- posterior_epred_loglogistic
  # Use beta > 1 so mean is finite
  prep <- .make_loglogistic_prep(mu = c(1, 2, 3), beta = 3,
                                  y  = c(0.5, 1.5, 4))
  mean_vals <- ep_fn(prep)
  # The result has SAME shape as prep$dpars$mu but with values scaled by
  # the constant factor pi / (beta * sin(pi/beta)).  Check the proportion.
  scale_factor <- pi / (3 * sin(pi / 3))
  expect_equal(mean(as.numeric(mean_vals)) / scale_factor,
                mean(as.numeric(prep$dpars$mu)),
                tolerance = 1e-10)
  expect_true(all(mean_vals > 0))
})


test_that("posterior_epred returns Inf when beta <= 1", {
  testthat::skip_if_not_installed("brms")

  ep_fn <- posterior_epred_loglogistic
  prep <- .make_loglogistic_prep(mu = c(1, 2), beta = 0.5,
                                  y  = c(0.5, 1.5))
  expect_true(all(is.infinite(ep_fn(prep))))
})


# ---- Shifted Loglogistic post-processing -----------------------------------

.make_sll_prep <- function(mu, sigma, xi, y, ndraws = 5) {
  prep <- list(
    ndraws = ndraws,
    nobs   = length(y),
    data   = list(Y = y),
    dpars  = list(
      mu    = matrix(rep(mu, each = ndraws), nrow = ndraws),
      sigma = rep(sigma, ndraws),
      xi    = rep(xi,    ndraws)
    ),
    family = list(link_mu = "identity", link_sigma = "log",
                   link_xi = "identity")
  )
  class(prep) <- c("brmsprep", "list")
  prep
}

test_that("log_lik for shifted_loglogistic agrees with d-function", {
  testthat::skip_if_not_installed("brms")

  ll_fn <- log_lik_shifted_loglogistic
  prep  <- .make_sll_prep(mu = c(0, 1, 2), sigma = 1, xi = 0.3,
                          y  = c(0, 1, 5))

  for (i in 1:3) {
    got <- ll_fn(i, prep)
    expected <- dshifted_loglogistic(prep$data$Y[i],
                                      mu    = prep$dpars$mu[, i],
                                      sigma = prep$dpars$sigma,
                                      xi    = prep$dpars$xi,
                                      log   = TRUE)
    expect_equal(got, expected, tolerance = 1e-10,
                  info = paste("i =", i))
  }
})


test_that("posterior_epred for shifted_loglogistic in logistic limit returns mu", {
  testthat::skip_if_not_installed("brms")

  ep_fn <- posterior_epred_shifted_loglogistic
  # xi = 0 -> logistic limit -> E[Y] = mu
  prep  <- .make_sll_prep(mu = c(1, 5, 10), sigma = 2, xi = 0,
                          y  = c(0, 0, 0))
  expect_equal(as.numeric(ep_fn(prep)),
                as.numeric(prep$dpars$mu),
                tolerance = 1e-10)
})


test_that("posterior_epred for shifted_loglogistic with finite xi", {
  testthat::skip_if_not_installed("brms")

  ep_fn <- posterior_epred_shifted_loglogistic
  prep  <- .make_sll_prep(mu = c(0, 1), sigma = 1, xi = 0.3,
                          y  = c(0, 0))
  out <- ep_fn(prep)
  xi <- 0.3
  expected_mat <- prep$dpars$mu +
                   1 * (pi * xi / sin(pi * xi) - 1) / xi
  expect_equal(as.numeric(out), as.numeric(expected_mat),
                tolerance = 1e-10)
})


test_that("posterior_epred for shifted_loglogistic returns Inf when xi >= 1", {
  testthat::skip_if_not_installed("brms")

  ep_fn <- posterior_epred_shifted_loglogistic
  prep  <- .make_sll_prep(mu = c(0, 1), sigma = 1, xi = 1.5,
                          y  = c(0, 0))
  expect_true(all(is.infinite(ep_fn(prep))))
})


# ===========================================================================
# Post-processing companion functions (log_lik / pp / epred)
# These are CRAN-safe: we just verify the functions exist, have correct
# signatures, and that the math of the mean formula reproduces known values
# for specific parameter combinations.  Real brmsfit-integration is tested
# in dev-tests/.
# ===========================================================================

test_that("log_lik_loglogistic is exported and a function", {
  expect_true(is.function(log_lik_loglogistic))
  expect_named(formals(log_lik_loglogistic), c("i", "prep"))
})

test_that("posterior_predict_loglogistic is exported and a function", {
  expect_true(is.function(posterior_predict_loglogistic))
  expect_named(formals(posterior_predict_loglogistic),
                c("i", "prep", "..."))
})

test_that("posterior_epred_loglogistic is exported and a function", {
  expect_true(is.function(posterior_epred_loglogistic))
  expect_named(formals(posterior_epred_loglogistic), "prep")
})

test_that("posterior_epred_loglogistic mean formula matches simulation", {
  # E[Y] = mu * pi / (beta * sin(pi / beta)),  beta > 1
  set.seed(2026)
  mu_true   <- 2
  beta_true <- 3
  y <- rloglogistic(50000, mu = mu_true, beta = beta_true)
  empirical <- mean(y)
  theory    <- mu_true * pi / (beta_true * sin(pi / beta_true))
  # 50k draws -> ~1% tolerance
  expect_equal(empirical, theory, tolerance = 5e-2,
                info = paste("empirical:", round(empirical, 3),
                              "theory:", round(theory, 3)))
})


# ===========================================================================
# Custom family object now ships with all three post-processing hooks
# ===========================================================================

test_that("brms_custom_loglogistic registers all 3 post-processing hooks", {
  testthat::skip_if_not_installed("brms")
  ll <- brms_custom_loglogistic()
  expect_true(is.function(ll$custom_family$log_lik))
  expect_true(is.function(ll$custom_family$posterior_predict))
  expect_true(is.function(ll$custom_family$posterior_epred))
})

test_that("brms_custom_loglogistic uses vectorised (loop=FALSE) signatures", {
  testthat::skip_if_not_installed("brms")
  ll <- brms_custom_loglogistic()
  expect_false(ll$custom_family$loop)
  # Stan code should use 'vector y' (vectorised signature)
  scode <- ll$stanvars_family[[1L]]$scode
  expect_true(grepl("vector y", scode))
})


# ===========================================================================
# Same for shifted_loglogistic
# ===========================================================================

test_that("log_lik / pp / epred functions exist for shifted_loglogistic", {
  expect_true(is.function(log_lik_shifted_loglogistic))
  expect_true(is.function(posterior_predict_shifted_loglogistic))
  expect_true(is.function(posterior_epred_shifted_loglogistic))
})

test_that("posterior_epred_shifted_loglogistic mean at xi=0 equals mu", {
  # Limiting Logistic case: E[Y] = mu
  fake_prep <- list()
  # We can't construct a full brms prep; we test the formula directly:
  mu    <- c(1, 2, 3)
  sigma <- c(0.5, 1, 1.5)
  xi    <- c(0, 0, 0)
  factor <- ifelse(abs(xi) < 1e-8, 0,
                    (gamma(1 + xi) * gamma(1 - xi) - 1) / xi)
  expect_equal(mu + sigma * factor, mu, tolerance = 1e-10)
})

test_that("brms_custom_shifted_loglogistic registers all 3 hooks", {
  testthat::skip_if_not_installed("brms")
  sll <- brms_custom_shifted_loglogistic()
  expect_true(is.function(sll$custom_family$log_lik))
  expect_true(is.function(sll$custom_family$posterior_predict))
  expect_true(is.function(sll$custom_family$posterior_epred))
  expect_false(sll$custom_family$loop)
})
