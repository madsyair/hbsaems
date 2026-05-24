# R/dist-shifted-loglogistic.R
# =============================================================================
# Shifted (3-parameter) Loglogistic distribution -- R-side d/p/q/r helpers,
# brms post-processing companions, and the custom-family wrapper.
#
# Stan-side
# ---------
# The Stan function definitions live in
# inst/stan/hbsae_shifted_loglogistic.stan; their names are prefixed with
# `hbsae_` (e.g.\ `hbsae_shifted_loglogistic_lpdf`) for consistency with
# the loglogistic family and to avoid potential symbol collisions with
# future Stan built-ins.
# =============================================================================


#' Shifted (3-Parameter) Loglogistic Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the shifted log-logistic (generalised log-logistic) distribution
#' with location \code{mu} (real), scale \code{sigma > 0} and shape
#' \code{xi} (real).  The two-parameter logistic distribution is
#' recovered as \code{xi -> 0}.
#'
#' @section Parameterisation:
#' This implementation uses the \strong{GEV-style parameterisation} of
#' Hosking & Wallis (1997) and the Flood Estimation Handbook (Robson &
#' Reed 1999), in which \eqn{\mu} is a pure location parameter (the
#' median), \eqn{\sigma} a pure scale parameter and \eqn{\xi} a pure
#' shape parameter:
#' \deqn{F(x \mid \mu, \sigma, \xi) =
#'   \{1 + (1 + \xi z)^{-1/\xi}\}^{-1},
#'   \qquad z = (x - \mu) / \sigma,}
#' with corresponding density
#' \deqn{f(x \mid \mu, \sigma, \xi) =
#'   \frac{(1 + \xi z)^{-(1/\xi + 1)}}
#'        {\sigma \{1 + (1 + \xi z)^{-1/\xi}\}^{2}}.}
#'
#' The support depends on \eqn{\xi}:
#' \itemize{
#'   \item \eqn{\xi > 0}: \eqn{x \ge \mu - \sigma/\xi} (bounded below).
#'   \item \eqn{\xi < 0}: \eqn{x \le \mu - \sigma/\xi} (bounded above).
#'   \item \eqn{\xi = 0}: \eqn{x \in \mathbb{R}} (logistic limit).
#' }
#' The median is always \eqn{\mu}; the mean exists when \eqn{|\xi| < 1}
#' and is \eqn{\mu + \sigma (\alpha\csc\alpha - 1)/\xi},
#' \eqn{\alpha = \pi\xi}.  Reducing further, the family contains:
#' \itemize{
#'   \item the standard log-logistic when \eqn{\xi = 1} (reparameterised);
#'   \item the logistic distribution as \eqn{\xi \to 0};
#'   \item the generalised Pareto family at \eqn{\xi = -1}.
#' }
#'
#' \strong{Why this parameterisation?}  An alternative
#' "simple-shift" form, \eqn{Y - \delta \sim \mathrm{LogLogistic}},
#' exists in the literature (Geskus 2001) and is closer in spirit to
#' \code{brms::shifted_lognormal()}'s positive shift \code{ndt}.  We
#' deliberately follow the GEV-style parameterisation because
#' \enumerate{
#'   \item it provides a \emph{smooth} limit to the logistic
#'         distribution at \eqn{\xi = 0};
#'   \item the parameters \eqn{(\mu, \sigma, \xi)} are
#'         orthogonally interpretable (location / scale / shape);
#'   \item it is the canonical form in hydrology and extreme-value
#'         applications (Hosking & Wallis 1997).
#' }
#'
#' @param x,q Numeric vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of random draws.
#' @param mu Location parameter (real; equals the median).
#' @param sigma Scale parameter (\code{sigma > 0}).
#' @param xi Shape parameter (real; \code{xi = 0} gives the logistic
#'   distribution).
#' @param log,log.p Logical.  See \code{\link{Distributions}}.
#' @param lower.tail Logical.  See \code{\link{Distributions}}.
#'
#' @return Numeric vector.
#'
#' @references
#' Geskus, R. B. (2001). Methods for estimating the AIDS incubation time
#' distribution when date of seroconversion is censored.
#' \emph{Statistics in Medicine}, 20(5), 795-812.
#'
#' Hosking, J. R. M., & Wallis, J. R. (1997). \emph{Regional Frequency
#' Analysis: An Approach Based on L-Moments}.  Cambridge University
#' Press.  ISBN 0-521-43045-3.
#'
#' Robson, A., & Reed, D. (1999). Flood Estimation Handbook, Volume 3:
#' Statistical Procedures for Flood Frequency Estimation.  Institute of
#' Hydrology, Wallingford, UK.
#'
#' @examples
#' dshifted_loglogistic(c(1, 2, 5),    mu = 0, sigma = 1, xi = 0.5)
#' pshifted_loglogistic(c(1, 2, 5),    mu = 0, sigma = 1, xi = 0.5)
#' qshifted_loglogistic(c(0.25, 0.75), mu = 0, sigma = 1, xi = 0.5)
#' set.seed(1); rshifted_loglogistic(5, mu = 0, sigma = 1, xi = 0.5)
#'
#' @name shifted_loglogistic
NULL


# Internal tolerance below which we switch to the Logistic limit.
.SLL_TOL <- 1e-8

#' @rdname shifted_loglogistic
#' @export
dshifted_loglogistic <- function(x, mu = 0, sigma = 1, xi = 0, log = FALSE) {
  stopifnot(all(sigma > 0))
  if (all(abs(xi) < .SLL_TOL)) {
    log_pdf <- stats::dlogis(x, location = mu, scale = sigma, log = TRUE)
  } else {
    z <- 1 + xi * (x - mu) / sigma
    log_pdf <- ifelse(z > 0,
      -log(sigma) - (1 + 1 / xi) * log(z) - 2 * log1p(z^(-1 / xi)),
      -Inf
    )
  }
  if (log) log_pdf else exp(log_pdf)
}


#' @rdname shifted_loglogistic
#' @export
pshifted_loglogistic <- function(q, mu = 0, sigma = 1, xi = 0,
                                  lower.tail = TRUE, log.p = FALSE) {
  stopifnot(all(sigma > 0))
  if (all(abs(xi) < .SLL_TOL)) {
    return(stats::plogis(q, location = mu, scale = sigma,
                         lower.tail = lower.tail, log.p = log.p))
  }
  z   <- 1 + xi * (q - mu) / sigma
  cdf <- ifelse(z <= 0,
                ifelse(xi > 0, 0, 1),
                1 / (1 + z^(-1 / xi)))
  if (!lower.tail) cdf <- 1 - cdf
  if (log.p) log(cdf) else cdf
}


#' @rdname shifted_loglogistic
#' @export
qshifted_loglogistic <- function(p, mu = 0, sigma = 1, xi = 0,
                                  lower.tail = TRUE, log.p = FALSE) {
  stopifnot(all(sigma > 0))
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  if (any(p < 0 | p > 1, na.rm = TRUE))
    stop("Probabilities must be in [0, 1].", call. = FALSE)
  if (all(abs(xi) < .SLL_TOL))
    return(stats::qlogis(p, location = mu, scale = sigma))
  z <- (p / (1 - p))^xi
  mu + sigma * (z - 1) / xi
}


#' @rdname shifted_loglogistic
#' @export
rshifted_loglogistic <- function(n, mu = 0, sigma = 1, xi = 0) {
  qshifted_loglogistic(stats::runif(n), mu = mu, sigma = sigma, xi = xi)
}


# --- Post-processing companions for brms ------------------------------------
# Same contract as for the 2-parameter loglogistic; an extra dpar (xi) is
# threaded through.
# ---------------------------------------------------------------------------

#' brms Post-Processing Functions for the Shifted Loglogistic Family
#'
#' Companion functions to \code{\link{brms_custom_shifted_loglogistic}} that
#' enable \code{brms::loo()}, \code{brms::posterior_predict()}, and
#' \code{brms::posterior_epred()} (plus related helpers) on brmsfits using
#' the shifted-loglogistic custom family.
#'
#' @param i Integer observation index (1-based).
#' @param prep brms preparation object.
#' @param ... Forwarded extra arguments (ignored).
#'
#' @return
#' \describe{
#'   \item{\code{log_lik_shifted_loglogistic}}{Log-density vector.}
#'   \item{\code{posterior_predict_shifted_loglogistic}}{One predictive
#'     draw per posterior sample.}
#'   \item{\code{posterior_epred_shifted_loglogistic}}{Conditional mean
#'     matrix.  The mean is finite only when \eqn{\xi < 1}; otherwise the
#'     return is \code{NaN} for those draws.}
#' }
#'
#' @details
#' The conditional mean of the shifted loglogistic is
#' \deqn{E[Y] = \mu + \sigma \, (\Gamma(1 + \xi) \Gamma(1 - \xi) - 1) / \xi
#'        \quad (\xi < 1, \xi \neq 0),}
#' with the logistic limit \eqn{E[Y] = \mu} at \eqn{\xi = 0}.
#'
#' @name shifted_loglogistic-brms-posthooks
#' @keywords internal
NULL

#' @rdname shifted_loglogistic-brms-posthooks
#' @export
log_lik_shifted_loglogistic <- function(i, prep) {
  mu    <- brms::get_dpar(prep, "mu",    i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  xi    <- brms::get_dpar(prep, "xi",    i = i)
  y     <- prep$data$Y[i]
  dshifted_loglogistic(y, mu = mu, sigma = sigma, xi = xi, log = TRUE)
}

#' @rdname shifted_loglogistic-brms-posthooks
#' @export
posterior_predict_shifted_loglogistic <- function(i, prep, ...) {
  mu    <- brms::get_dpar(prep, "mu",    i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  xi    <- brms::get_dpar(prep, "xi",    i = i)
  rshifted_loglogistic(length(mu), mu = mu, sigma = sigma, xi = xi)
}

#' @rdname shifted_loglogistic-brms-posthooks
#' @export
posterior_epred_shifted_loglogistic <- function(prep) {
  mu    <- brms::get_dpar(prep, "mu")
  sigma <- brms::get_dpar(prep, "sigma")
  xi    <- brms::get_dpar(prep, "xi")
  # Closed-form mean:
  #   xi = 0 -> mu (logistic limit)
  #   |xi| < 1, xi != 0 -> mu + sigma * (gamma(1+xi) gamma(1-xi) - 1) / xi
  #   |xi| >= 1 -> mean undefined (heavy tail); return Inf with the sign
  #                of xi to flag the issue without silently producing NaN.
  factor <- ifelse(abs(xi) < .SLL_TOL,
                    0,
                    ifelse(abs(xi) < 1,
                           (gamma(1 + xi) * gamma(1 - xi) - 1) / xi,
                           Inf))
  mu + sigma * factor
}


# --- brms custom_family object ----------------------------------------------

#' Shifted Loglogistic as a Custom Distribution Family for brms
#'
#' Returns the \code{custom_family} + \code{stanvars} pair required to fit a
#' brms model with a Shifted Loglogistic response distribution.  The Stan
#' code is loaded from \code{inst/stan/shifted_loglogistic.stan} via
#' \code{\link{build_brms_custom_family}}; post-processing functions
#' (\code{log_lik}, \code{posterior_predict}, \code{posterior_epred}) are
#' wired in automatically.
#'
#' Three parameters:
#' \describe{
#'   \item{\code{mu}}{Location parameter (identity link).}
#'   \item{\code{sigma}}{Scale parameter (\code{sigma > 0}, log link).}
#'   \item{\code{xi}}{Shape parameter (identity link).}
#' }
#'
#' @return A list with elements \code{custom_family} and
#'   \code{stanvars_family} ready for use with \code{brms::brm()}.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' sll <- brms_custom_shifted_loglogistic()
#' # fit <- brm(y ~ x, data = d,
#' #            family   = sll$custom_family,
#' #            stanvars = sll$stanvars_family)
#' }
#'
#' @seealso \code{\link{dshifted_loglogistic}},
#'   \code{\link{build_brms_custom_family}},
#'   \code{\link{register_hbsae_brms_custom}}.
#' @export
brms_custom_shifted_loglogistic <- function() {
  build_brms_custom_family(
    # Prefixed with `hbsae_` for consistency with the loglogistic family
    # and to leave the bare `shifted_loglogistic` namespace open should
    # Stan ever ship a built-in with that name.
    name              = "hbsae_shifted_loglogistic",
    dpars             = c("mu", "sigma", "xi"),
    links             = c("identity", "log", "identity"),
    lb                = c(NA, 0, NA),
    ub                = c(NA, NA, NA),
    type              = "real",
    loop              = FALSE,
    log_lik           = log_lik_shifted_loglogistic,
    posterior_predict = posterior_predict_shifted_loglogistic,
    posterior_epred   = posterior_epred_shifted_loglogistic
  )
}
