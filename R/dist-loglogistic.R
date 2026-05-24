# R/dist-loglogistic.R
# =============================================================================
# Loglogistic distribution -- d/p/q/r helpers, post-processing functions for
# brms (log_lik / posterior_predict / posterior_epred), and the
# brms_custom_loglogistic() wrapper.
#
# Stan-side
# ---------
# The Stan function definitions live in inst/stan/hbsae_loglogistic.stan
# and are loaded at runtime via build_brms_custom_family().  The Stan
# function names are prefixed with `hbsae_` (e.g.\ `hbsae_loglogistic_lpdf`)
# to avoid a symbol collision with Stan's BUILT-IN `loglogistic_lpdf`
# (Stan >= 2.29).  The user-facing R helpers dloglogistic(),
# ploglogistic(), qloglogistic(), and rloglogistic() KEEP their natural
# names because they live in the R namespace, not the Stan namespace.
#
# Parameterisation
# ----------------
# Y ~ Loglogistic(mu, beta) with mu > 0 (scale) and beta > 0 (shape).
#
#   PDF: f(y) = (beta/mu) * (y/mu)^(beta - 1) / [1 + (y/mu)^beta]^2,  y > 0
#   CDF: F(y) = 1 / [1 + (y/mu)^(-beta)]
#   Median: mu;   Mean (when beta > 1): mu * pi / (beta * sin(pi/beta))
# =============================================================================


#' Loglogistic Distribution Functions
#'
#' Density, distribution function, quantile function, and random generation
#' for the loglogistic (Fisk) distribution with scale parameter
#' \code{mu > 0} and shape parameter \code{beta > 0}.
#'
#' @section Parameterisation:
#' This implementation follows the canonical Wikipedia / \pkg{flexsurv} /
#' \pkg{eha} parameterisation (Jackson 2016; Bennett 1983):
#' \deqn{Y \sim \mathrm{LogLogistic}(\mu, \beta),
#'   \quad \mu > 0, \quad \beta > 0,}
#' with probability density function
#' \deqn{f(y \mid \mu, \beta) =
#'   \frac{(\beta/\mu)(y/\mu)^{\beta - 1}}{\{1 + (y/\mu)^{\beta}\}^{2}},
#'   \quad y > 0,}
#' cumulative distribution function
#' \deqn{F(y \mid \mu, \beta) = \{1 + (y/\mu)^{-\beta}\}^{-1},}
#' median \eqn{\mu}, and mean
#' \eqn{E[Y] = \mu \pi / [\beta \sin(\pi / \beta)]} when
#' \eqn{\beta > 1}.  Equivalently, \eqn{\log(Y) \sim
#'   \mathrm{Logistic}(\log\mu, \, 1/\beta)}.
#'
#' \strong{Why not match the brms \code{lognormal} convention?}  The
#' \code{brms::lognormal()} family parameterises \eqn{\mu} on the log
#' scale (so \eqn{\mu} is unconstrained and uses an identity link).
#' Doing the same for the log-logistic would require redefining
#' \eqn{\mu = \log(\mathrm{median}(Y))} -- which deviates from every
#' standard R reference (\pkg{flexsurv}, \pkg{eha}, Wolfram, scipy,
#' Stata).  We deliberately follow the survival-analysis convention
#' instead: \eqn{\mu} is the median (positive, log link), keeping
#' interpretation simple and posterior summaries comparable with the
#' rest of the R survival ecosystem.
#'
#' @param x,q Vector of quantiles (\code{x > 0} for non-zero density).
#' @param p Vector of probabilities (\code{0 <= p <= 1}).
#' @param n Number of random draws.
#' @param mu Scale parameter (\code{mu > 0}; equals the median).
#' @param beta Shape parameter (\code{beta > 0}).
#' @param log,log.p Logical; if \code{TRUE}, return the log density / log
#'   probability.
#' @param lower.tail Logical; if \code{TRUE} (default) probabilities are
#'   \eqn{P[Y \le q]}, otherwise \eqn{P[Y > q]}.
#'
#' @return Numeric vector of the same length as the input.
#'
#' @references
#' Bennett, S. (1983). Log-logistic regression models for survival data.
#' \emph{Journal of the Royal Statistical Society, Series C}, 32(2),
#' 165-171. \doi{10.2307/2347295}
#'
#' Jackson, C. H. (2016). flexsurv: A platform for parametric survival
#' modelling in R. \emph{Journal of Statistical Software}, 70(8), 1-33.
#' \doi{10.18637/jss.v070.i08}
#'
#' Kleiber, C., & Kotz, S. (2003). \emph{Statistical Size Distributions
#' in Economics and Actuarial Sciences}.  Wiley.
#'
#' @examples
#' dloglogistic(c(0.5, 1, 2),  mu = 1, beta = 2)
#' ploglogistic(c(0.5, 1, 2),  mu = 1, beta = 2)
#' qloglogistic(c(0.25, 0.75), mu = 1, beta = 2)
#' set.seed(1); rloglogistic(5, mu = 1, beta = 2)
#'
#' @name loglogistic
NULL


#' @rdname loglogistic
#' @export
dloglogistic <- function(x, mu = 1, beta = 1, log = FALSE) {
  # Strict positivity check on parameters, EXCEPT for NA (which we
  # propagate as NA in the result, matching base R density conventions
  # like dnorm(NA) -> NA).  Pre-v1.0.0 the stopifnot() tripped on
  # `all(NA > 0)` (which is NA) because stopifnot() treats NA as
  # failure.
  if (!all(mu[!is.na(mu)] > 0))
    stop("all(mu > 0) is not TRUE", call. = FALSE)
  if (!all(beta[!is.na(beta)] > 0))
    stop("all(beta > 0) is not TRUE", call. = FALSE)

  # Determine the output length by R's standard recycling rules.
  n_out <- max(length(x), length(mu), length(beta))
  if (length(x)    < n_out) x    <- rep(x,    length.out = n_out)
  if (length(mu)   < n_out) mu   <- rep(mu,   length.out = n_out)
  if (length(beta) < n_out) beta <- rep(beta, length.out = n_out)

  log_pdf <- rep(-Inf, n_out)
  pos <- which(x > 0 & !is.na(mu) & !is.na(beta))
  if (length(pos) > 0L) {
    z <- x[pos] / mu[pos]
    log_pdf[pos] <- log(beta[pos]) - log(mu[pos]) +
                     (beta[pos] - 1) * log(z) -
                     2 * log1p(z^beta[pos])
  }
  # Propagate NA from any input (base R convention: dnorm(NA) -> NA).
  na_idx <- is.na(x) | is.na(mu) | is.na(beta)
  log_pdf[!is.finite(log_pdf)] <- -Inf
  log_pdf[na_idx] <- NA_real_
  if (log) log_pdf else exp(log_pdf)
}


#' @rdname loglogistic
#' @export
ploglogistic <- function(q, mu = 1, beta = 1,
                          lower.tail = TRUE, log.p = FALSE) {
  stopifnot(all(mu > 0), all(beta > 0))
  z   <- q / mu
  cdf <- ifelse(q <= 0, 0, 1 / (1 + z^(-beta)))
  if (!lower.tail) cdf <- 1 - cdf
  if (log.p) log(cdf) else cdf
}


#' @rdname loglogistic
#' @export
qloglogistic <- function(p, mu = 1, beta = 1,
                          lower.tail = TRUE, log.p = FALSE) {
  stopifnot(all(mu > 0), all(beta > 0))
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  if (any(p < 0 | p > 1, na.rm = TRUE))
    stop("Probabilities must be in [0, 1].", call. = FALSE)
  mu * (p / (1 - p))^(1 / beta)
}


#' @rdname loglogistic
#' @export
rloglogistic <- function(n, mu = 1, beta = 1) {
  stopifnot(all(mu > 0), all(beta > 0))
  qloglogistic(stats::runif(n), mu = mu, beta = beta)
}


# --- Post-processing companions for brms ------------------------------------
# These follow the signature contract required by brms when post-processing
# methods (loo, posterior_predict, posterior_epred, fitted) are called on a
# custom-family brmsfit.  The three functions are passed to
# brms::custom_family() via the matching arguments inside
# brms_custom_loglogistic() below.
#
# Each function receives a `prep` object built by brms; we use
# brms::get_dpar() to extract the per-observation linear predictor (mu)
# and the auxiliary parameter (beta).
# ---------------------------------------------------------------------------

#' brms Post-Processing Functions for the Loglogistic Family
#'
#' Companion functions to \code{\link{brms_custom_loglogistic}} that enable
#' \code{brms::loo()}, \code{brms::posterior_predict()}, and
#' \code{brms::posterior_epred()} (plus \code{conditional_effects()},
#' \code{pp_check()}, etc.) on fitted brms models that use the loglogistic
#' custom family.
#'
#' Users typically do not call these directly -- they are wired into the
#' custom-family object automatically by \code{brms_custom_loglogistic()}.
#'
#' @param i Integer observation index (1-based).
#' @param prep brms preparation object passed by post-processing methods.
#' @param ... Additional arguments forwarded by brms (ignored here).
#'
#' @return
#' \describe{
#'   \item{\code{log_lik_loglogistic}}{Vector of length \code{ndraws}
#'     containing log densities of \eqn{y_i} per posterior draw.}
#'   \item{\code{posterior_predict_loglogistic}}{Vector of length
#'     \code{ndraws} of posterior predictive draws for \eqn{y_i}.}
#'   \item{\code{posterior_epred_loglogistic}}{Matrix of conditional means
#'     \eqn{E[Y \mid X]} of size \code{ndraws x N}.}
#' }
#'
#' @details
#' The closed-form mean of the loglogistic distribution exists only when
#' \eqn{\beta > 1}:
#' \deqn{E[Y] = \mu \pi / [\beta \sin(\pi / \beta)].}
#' For \eqn{\beta \le 1}, the mean is undefined; \code{posterior_epred_loglogistic}
#' returns \code{NaN} for the offending posterior draws.
#'
#' @name loglogistic-brms-posthooks
#' @keywords internal
NULL

#' @rdname loglogistic-brms-posthooks
#' @export
log_lik_loglogistic <- function(i, prep) {
  mu   <- brms::get_dpar(prep, "mu",   i = i)
  beta <- brms::get_dpar(prep, "beta", i = i)
  y    <- prep$data$Y[i]
  dloglogistic(y, mu = mu, beta = beta, log = TRUE)
}

#' @rdname loglogistic-brms-posthooks
#' @export
posterior_predict_loglogistic <- function(i, prep, ...) {
  mu   <- brms::get_dpar(prep, "mu",   i = i)
  beta <- brms::get_dpar(prep, "beta", i = i)
  # One draw per posterior sample, vectorised over draws by ifelse.
  rloglogistic(length(mu), mu = mu, beta = beta)
}

#' @rdname loglogistic-brms-posthooks
#' @export
posterior_epred_loglogistic <- function(prep) {
  mu   <- brms::get_dpar(prep, "mu")    # ndraws x N matrix
  beta <- brms::get_dpar(prep, "beta")
  # E[Y] = mu * pi / (beta * sin(pi / beta)), defined ONLY when beta > 1.
  # When beta <= 1, the mean is improper -- return Inf explicitly rather
  # than relying on sin(pi/beta) being numerically zero (it is not, when
  # beta = 1/k for integer k).
  factor_mean <- ifelse(beta > 1,
                         pi / (beta * sin(pi / beta)),
                         Inf)
  mu * factor_mean
}


# --- brms custom_family object ---------------------------------------------

#' Loglogistic as a Custom Distribution Family for brms
#'
#' Returns the \code{custom_family} + \code{stanvars} pair required to fit a
#' \pkg{brms} model with a Loglogistic response distribution.  The Stan code
#' is loaded from \code{inst/stan/loglogistic.stan} via
#' \code{\link{build_brms_custom_family}}; post-processing functions
#' (\code{log_lik}, \code{posterior_predict}, \code{posterior_epred}) are
#' wired in automatically.
#'
#' Two parameters:
#' \describe{
#'   \item{\code{mu}}{Scale (\code{mu > 0}, log link).}
#'   \item{\code{beta}}{Shape (\code{beta > 0}, log link).}
#' }
#'
#' @return A list with elements \code{custom_family} and
#'   \code{stanvars_family} ready for use with \code{brms::brm()}.  The
#'   returned \code{custom_family} object has \code{log_lik},
#'   \code{posterior_predict}, and \code{posterior_epred} registered so
#'   that \code{brms::loo()}, \code{brms::posterior_predict()}, and
#'   \code{brms::posterior_epred()} work without further setup.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' ll <- brms_custom_loglogistic()
#' # fit <- brm(y ~ x, data = d,
#' #            family   = ll$custom_family,
#' #            stanvars = ll$stanvars_family)
#' # loo(fit)                     # uses log_lik_loglogistic
#' # posterior_predict(fit)       # uses posterior_predict_loglogistic
#' # posterior_epred(fit)         # uses posterior_epred_loglogistic
#' }
#'
#' @seealso \code{\link{dloglogistic}},
#'   \code{\link{build_brms_custom_family}},
#'   \code{\link{register_hbsae_brms_custom}}.
#' @export
brms_custom_loglogistic <- function() {
  build_brms_custom_family(
    # The Stan-side family name is "hbsae_loglogistic" (rather than just
    # "loglogistic") to avoid a symbol collision with Stan's built-in
    # `loglogistic_lpdf` (Stan >= 2.29).  brms will generate
    # `target += hbsae_loglogistic_lpdf(...)` from the name we pass here.
    # The user-facing R-side names dloglogistic(), ploglogistic(),
    # qloglogistic(), and rloglogistic() are unaffected because they
    # live in the R namespace and not the Stan namespace.
    name              = "hbsae_loglogistic",
    dpars             = c("mu", "beta"),
    links             = c("log", "log"),
    lb                = c(0,    0),
    ub                = c(NA,   NA),
    type              = "real",
    loop              = FALSE,
    log_lik           = log_lik_loglogistic,
    posterior_predict = posterior_predict_loglogistic,
    posterior_epred   = posterior_epred_loglogistic
  )
}
