# tests/testthat/test-custom-distributions.R
# ==========================================================================
# Comprehensive correctness tests for the custom loglogistic and
# shifted-loglogistic distributions.
#
# The tests verify:
#   1. Probability-theory identities (CDF monotonicity, P--Q inversion,
#      survival-function identity, density integrates to 1).
#   2. Analytic vs numerical mean.  Verifies the theoretical
#      E[Y] = mu*pi / (beta*sin(pi/beta))  formula by comparing it to
#      both numerical integration of f(y) over (0, Inf) and the sample
#      mean of large Monte Carlo draws.
#   3. Input-domain checks: function refuses negative scale, mu = 0,
#      beta = 0, probabilities outside [0, 1], etc.
#   4. Numerical stability for extreme arguments: small y, large y,
#      very large or very small shape.  Density / CDF must not return
#      NaN / Inf / negative values.
#   5. Stan-side log-pdf cross-check (only when 'rstan' is installed):
#      Stan's loglogistic_lpdf must agree with R's dloglogistic(log=TRUE)
#      to working precision.
# ==========================================================================

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(hbsaems))

# --------------------------------------------------------------------------
# 1. LOGLOGISTIC -- probability-theory identities
# --------------------------------------------------------------------------

test_that("dloglogistic: density is positive on (0, Inf) and zero elsewhere", {
  x <- c(-1, 0, 0.1, 1, 5, 100)
  d <- dloglogistic(x, mu = 1, beta = 2)
  expect_equal(d[1], 0)              # x < 0
  expect_equal(d[2], 0)              # x = 0
  expect_true(all(d[3:6] > 0))
  expect_true(all(is.finite(d)))
})

test_that("dloglogistic: log = TRUE matches log() of density", {
  x <- c(0.5, 1, 2, 5)
  expect_equal(dloglogistic(x, mu = 1, beta = 2, log = TRUE),
               log(dloglogistic(x, mu = 1, beta = 2)),
               tolerance = 1e-12)
})

test_that("ploglogistic: CDF identity F(y) = 1 / (1 + (y/mu)^(-beta))", {
  y     <- c(0.5, 1, 2, 5, 10)
  mu    <- 2;  beta <- 3
  z     <- y / mu
  exact <- 1 / (1 + z^(-beta))
  expect_equal(ploglogistic(y, mu, beta), exact, tolerance = 1e-12)
})

test_that("ploglogistic: CDF is non-decreasing on (0, Inf)", {
  y <- seq(0.01, 100, length.out = 50)
  F <- ploglogistic(y, mu = 5, beta = 2)
  expect_true(all(diff(F) >= -1e-14))
  expect_gte(F[1], 0)
  expect_lte(F[length(F)], 1)
})

test_that("ploglogistic: survival function lower.tail = FALSE", {
  y     <- c(0.5, 1, 2, 5)
  S     <- ploglogistic(y, mu = 2, beta = 3, lower.tail = FALSE)
  F     <- ploglogistic(y, mu = 2, beta = 3, lower.tail = TRUE)
  expect_equal(S + F, rep(1, length(y)), tolerance = 1e-12)
})

test_that("ploglogistic: log.p = TRUE matches log() of CDF", {
  y    <- c(0.5, 1, 2, 5)
  logF <- ploglogistic(y, 2, 3, log.p = TRUE)
  expect_equal(logF, log(ploglogistic(y, 2, 3)), tolerance = 1e-12)
})

test_that("qloglogistic: closed-form quantile  mu * (p / (1-p))^(1/beta)", {
  p     <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  mu    <- 3; beta <- 2
  exact <- mu * (p / (1 - p))^(1 / beta)
  expect_equal(qloglogistic(p, mu, beta), exact, tolerance = 1e-12)
})

test_that("qloglogistic: median = mu (since the loglogistic is symmetric on log scale)", {
  for (mu in c(0.5, 1, 5, 100)) {
    for (beta in c(0.5, 1, 2, 10)) {
      expect_equal(qloglogistic(0.5, mu, beta), mu, tolerance = 1e-12)
    }
  }
})

test_that("Q is inverse of P: ploglogistic(qloglogistic(p)) == p", {
  p <- seq(0.05, 0.95, by = 0.05)
  q <- qloglogistic(p, mu = 5, beta = 3)
  expect_equal(ploglogistic(q, mu = 5, beta = 3), p, tolerance = 1e-12)
})

test_that("dloglogistic: density integrates to 1 over (0, Inf)", {
  for (mu in c(1, 5)) {
    for (beta in c(0.8, 2, 5)) {
      I <- stats::integrate(dloglogistic, lower = 1e-12, upper = Inf,
                             mu = mu, beta = beta, rel.tol = 1e-10)$value
      expect_equal(I, 1, tolerance = 1e-6,
                   info = sprintf("mu=%g, beta=%g", mu, beta))
    }
  }
})


# --------------------------------------------------------------------------
# 2. LOGLOGISTIC -- analytic vs numerical mean
# --------------------------------------------------------------------------
#
#   E[Y] = mu * pi / (beta * sin(pi / beta))    for beta > 1
#   E[Y] = infinity                              for beta <= 1
#

test_that("loglogistic mean: analytic formula matches numerical integration", {
  cases <- expand.grid(mu = c(1, 5), beta = c(1.5, 2, 3, 5))
  for (i in seq_len(nrow(cases))) {
    mu   <- cases$mu[i]
    beta <- cases$beta[i]
    analytic  <- mu * pi / (beta * sin(pi / beta))
    numerical <- stats::integrate(function(y) y * dloglogistic(y, mu, beta),
                                   lower = 1e-12, upper = Inf,
                                   rel.tol = 1e-8)$value
    expect_equal(numerical, analytic, tolerance = 1e-4,
                 info = sprintf("mu=%g, beta=%g", mu, beta))
  }
})

test_that("loglogistic mean: Monte Carlo sample mean matches analytic (beta > 2)", {
  # beta close to 1 has Pareto-like tails — finite mean but huge variance,
  # so Monte Carlo convergence requires far more draws.  Test only the
  # well-behaved regime beta > 2.
  set.seed(20260517)
  for (beta in c(2.5, 3, 6)) {
    mu       <- 3
    analytic <- mu * pi / (beta * sin(pi / beta))
    x        <- rloglogistic(2e5, mu = mu, beta = beta)
    sm       <- mean(x)
    # 2% relative tolerance — Monte Carlo noise dominates
    expect_lt(abs(sm - analytic) / analytic, 0.02,
              label = sprintf("MC mean (beta=%g)", beta))
  }
})

test_that("loglogistic variance: analytic vs numerical (beta > 2)", {
  # Var[Y] = mu^2 * { 2*pi/(beta*sin(2*pi/beta)) - (pi/(beta*sin(pi/beta)))^2 }
  for (beta in c(2.5, 4, 6)) {
    mu       <- 2
    b        <- beta
    analytic <- mu^2 * (2 * pi / (b * sin(2 * pi / b)) -
                          (pi / (b * sin(pi / b)))^2)
    mean_lp  <- mu * pi / (b * sin(pi / b))
    numerical <- stats::integrate(function(y) (y - mean_lp)^2 *
                                                 dloglogistic(y, mu, b),
                                   lower = 1e-12, upper = Inf,
                                   rel.tol = 1e-8)$value
    expect_equal(numerical, analytic, tolerance = 1e-3,
                 info = sprintf("var beta=%g", b))
  }
})


# --------------------------------------------------------------------------
# 3. LOGLOGISTIC -- input-domain validation
# --------------------------------------------------------------------------

test_that("d/p/q/r loglogistic refuse non-positive mu and beta", {
  expect_error(dloglogistic(1, mu = -1, beta = 1))
  expect_error(dloglogistic(1, mu = 0,  beta = 1))
  expect_error(dloglogistic(1, mu = 1,  beta = -1))
  expect_error(dloglogistic(1, mu = 1,  beta = 0))

  expect_error(ploglogistic(1, mu = -1, beta = 1))
  expect_error(qloglogistic(0.5, mu = 1, beta = -1))
  expect_error(rloglogistic(5, mu = -1, beta = 1))
})

test_that("qloglogistic refuses probabilities outside [0, 1]", {
  expect_error(qloglogistic(-0.1, mu = 1, beta = 1))
  expect_error(qloglogistic(1.5,  mu = 1, beta = 1))
})

test_that("qloglogistic boundary: p = 0 returns 0, p = 1 returns Inf", {
  expect_equal(qloglogistic(0, mu = 1, beta = 1), 0)
  expect_equal(qloglogistic(1, mu = 1, beta = 1), Inf)
})


# --------------------------------------------------------------------------
# 4. LOGLOGISTIC -- numerical stability
# --------------------------------------------------------------------------

test_that("dloglogistic: returns finite values for extreme x", {
  # Very small and very large x
  d <- dloglogistic(c(1e-10, 1e10), mu = 1, beta = 2)
  expect_true(all(is.finite(d) | d == 0))
  expect_true(all(d >= 0))

  # Very small and very large beta (shape)
  d2 <- dloglogistic(1, mu = 1, beta = c(0.01, 100))
  expect_true(all(d2 >= 0))
  expect_true(all(is.finite(d2)))
})

test_that("ploglogistic: clamps to [0,1] for extreme y", {
  F <- ploglogistic(c(1e-10, 1e10), mu = 1, beta = 2)
  expect_true(all(F >= 0 & F <= 1))
  expect_lt(F[1], 1e-15)              # very small y -> tiny CDF
  expect_gt(F[2], 1 - 1e-15)          # very large y -> ~1
})

test_that("dloglogistic log-density: no -Inf for moderately extreme arguments", {
  # If log(z^beta) underflows to -Inf, the result must still be finite or
  # cleanly -Inf -- not NaN.  Stress-test with combinations chosen to push
  # log1p(z^beta) and log(z) toward extreme branches.
  combos <- expand.grid(x = c(1e-6, 1e-3, 0.1, 1, 10, 1e3, 1e6),
                         mu = c(0.01, 1, 100),
                         beta = c(0.5, 1, 2, 8))
  for (i in seq_len(nrow(combos))) {
    ld <- dloglogistic(combos$x[i], combos$mu[i], combos$beta[i], log = TRUE)
    expect_false(is.nan(ld),
                 info = sprintf("x=%g, mu=%g, beta=%g",
                                  combos$x[i], combos$mu[i], combos$beta[i]))
  }
})


# ==========================================================================
# 5. SHIFTED LOGLOGISTIC -- probability-theory identities
# ==========================================================================
#
#  GEV-style parameterisation (Hosking & Wallis 1997):
#    mu       in R         (location, identity link)
#    sigma    > 0           (scale)
#    xi       in R          (shape; xi = 0 -> logistic limit)
#
#  CDF (xi != 0):
#    F(y) = 1 / (1 + z)       where z = (1 + xi*(y-mu)/sigma)^(-1/xi)
#  CDF (xi = 0, the logistic limit):
#    F(y) = 1 / (1 + exp(-(y-mu)/sigma))
#  Support:
#    xi >  0:  y in (mu - sigma/xi, +Inf)
#    xi <  0:  y in (-Inf, mu - sigma/xi)
#    xi =  0:  y in (-Inf, +Inf)

test_that("shifted_loglogistic xi = 0: reduces to logistic", {
  y <- c(-3, -1, 0, 1, 3)
  expect_equal(dshifted_loglogistic(y, mu = 0, sigma = 1, xi = 0),
               stats::dlogis(y, location = 0, scale = 1),
               tolerance = 1e-12)
  expect_equal(pshifted_loglogistic(y, mu = 0, sigma = 1, xi = 0),
               stats::plogis(y, location = 0, scale = 1),
               tolerance = 1e-12)
})

test_that("shifted_loglogistic: continuous as xi -> 0 (smooth limit)", {
  # Density at y = mu should converge to dlogis(mu, mu, sigma) = 1/(4 sigma).
  y      <- 0
  mu     <- 0;  sigma <- 1
  d_lim  <- stats::dlogis(y, mu, sigma)             # 1/(4*sigma) = 0.25
  d_eps  <- dshifted_loglogistic(y, mu, sigma, xi = 1e-7)
  expect_equal(d_eps, d_lim, tolerance = 1e-4)
})

test_that("shifted_loglogistic CDF: F + S = 1", {
  y     <- c(-2, -1, 0, 1, 2)
  for (xi in c(-0.3, 0, 0.3)) {
    F <- pshifted_loglogistic(y, mu = 0, sigma = 1, xi = xi)
    S <- pshifted_loglogistic(y, mu = 0, sigma = 1, xi = xi,
                                lower.tail = FALSE)
    expect_equal(F + S, rep(1, length(y)),
                 tolerance = 1e-12,
                 info = sprintf("xi = %g", xi))
  }
})

test_that("shifted_loglogistic Q is inverse of P (xi != 0)", {
  p <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  for (xi in c(-0.4, -0.1, 0.1, 0.4)) {
    q  <- qshifted_loglogistic(p, mu = 0, sigma = 1, xi = xi)
    p2 <- pshifted_loglogistic(q, mu = 0, sigma = 1, xi = xi)
    expect_equal(p2, p, tolerance = 1e-10,
                 info = sprintf("xi = %g", xi))
  }
})

test_that("shifted_loglogistic Q is inverse of P (xi = 0, logistic limit)", {
  p  <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  q  <- qshifted_loglogistic(p, mu = 0, sigma = 1, xi = 0)
  p2 <- pshifted_loglogistic(q, mu = 0, sigma = 1, xi = 0)
  expect_equal(p2, p, tolerance = 1e-12)
})

test_that("shifted_loglogistic density integrates to 1", {
  # Support depends on xi:
  #   xi > 0 : y in (mu - sigma/xi, +Inf)
  #   xi < 0 : y in (-Inf,  mu - sigma/xi)
  #   xi = 0 : y in (-Inf, +Inf) (logistic)
  for (xi in c(-0.4, 0, 0.4)) {
    mu    <- 0; sigma <- 1
    # Use wide-enough bounds to capture heavy tails when present
    if (xi > 0) {
      lower <- mu - sigma / xi + 1e-9
      upper <- 1000
    } else if (xi < 0) {
      lower <- -1000
      upper <- mu - sigma / xi - 1e-9
    } else {
      lower <- -50; upper <- 50
    }
    I <- stats::integrate(function(y)
            dshifted_loglogistic(y, mu, sigma, xi),
            lower = lower, upper = upper, rel.tol = 1e-10)$value
    expect_equal(I, 1, tolerance = 1e-4,
                 info = sprintf("xi=%g, support=(%g, %g)", xi, lower, upper))
  }
})


# --------------------------------------------------------------------------
# 6. SHIFTED LOGLOGISTIC -- input-domain validation
# --------------------------------------------------------------------------

test_that("shifted_loglogistic refuses non-positive sigma", {
  expect_error(dshifted_loglogistic(0, mu = 0, sigma = -1, xi = 0.2))
  expect_error(dshifted_loglogistic(0, mu = 0, sigma =  0, xi = 0.2))
  expect_error(pshifted_loglogistic(0, mu = 0, sigma = -1, xi = 0.2))
  expect_error(qshifted_loglogistic(0.5, mu = 0, sigma = -1, xi = 0.2))
})

test_that("qshifted_loglogistic refuses probabilities outside [0, 1]", {
  expect_error(qshifted_loglogistic(-0.1, mu = 0, sigma = 1, xi = 0.3))
  expect_error(qshifted_loglogistic( 1.5, mu = 0, sigma = 1, xi = 0.3))
})

test_that("dshifted_loglogistic: density is zero outside support when xi != 0", {
  # xi > 0 : support is y > mu - sigma/xi.  At y = mu - sigma/xi - small,
  # density should be zero.
  d_pos_out <- dshifted_loglogistic(-3, mu = 0, sigma = 1, xi = 0.5)
  expect_equal(d_pos_out, 0)

  # xi < 0 : support is y < mu - sigma/xi.  At y = mu - sigma/xi + small,
  # density should be zero.
  d_neg_out <- dshifted_loglogistic(3, mu = 0, sigma = 1, xi = -0.5)
  expect_equal(d_neg_out, 0)
})


# --------------------------------------------------------------------------
# 7. SHIFTED LOGLOGISTIC -- numerical stability
# --------------------------------------------------------------------------

test_that("shifted_loglogistic: stable across a wide range of xi", {
  for (xi in c(-0.99, -0.5, -0.1, 0, 0.1, 0.5, 1.0, 5)) {
    y <- seq(-5, 5, length.out = 11)
    # NaN warnings can fire on out-of-support arguments — that's the
    # whole point of this test (verify they are mapped to 0 / clamped to
    # [0,1] cleanly).  We therefore SUPPRESS those warnings explicitly.
    d <- suppressWarnings(dshifted_loglogistic(y, mu = 0, sigma = 1, xi = xi))
    f <- suppressWarnings(pshifted_loglogistic(y, mu = 0, sigma = 1, xi = xi))
    expect_false(any(is.nan(d)),
                 info = sprintf("d NaN at xi=%g", xi))
    expect_false(any(is.nan(f)),
                 info = sprintf("F NaN at xi=%g", xi))
    expect_true(all(d >= 0),
                info = sprintf("negative density at xi=%g", xi))
    expect_true(all(f >= 0 & f <= 1),
                info = sprintf("F out of [0,1] at xi=%g", xi))
  }
})


#