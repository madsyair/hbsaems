// =============================================================================
// inst/stan/loglogistic.stan
//
// Stan function definitions for the Loglogistic (Fisk) distribution.
//
// IMPORTANT NAMING CONVENTION
// ---------------------------
// Since Stan 2.29, the language provides a built-in function called
//   `loglogistic_lpdf(y | alpha, beta)`
// (see https://mc-stan.org/docs/functions-reference/log-logistic-distribution.html).
//
// brms's custom_family() will generate a target += loglogistic_lpdf(...)
// call from the family name we pass to brms::custom_family().  If we
// reuse the bare name `loglogistic` for our custom family, Stan would
// see TWO functions with the same signature (our user-defined one and
// the built-in) and refuse to compile -- an unrecoverable collision.
//
// Solution: register the custom family under the name "hbsae_loglogistic"
// (defined in R/dist-loglogistic.R via brms_custom_loglogistic()),
// so brms generates target += hbsae_loglogistic_lpdf(...) and there is
// no ambiguity with the Stan built-in.  The user-facing R helpers
// dloglogistic(), ploglogistic(), qloglogistic(), rloglogistic() KEEP
// their natural names since they live in the R namespace, not the
// Stan namespace.
//
// Parameterisation
// ----------------
//   Y ~ Loglogistic(mu, beta) with mu > 0 (scale = median) and
//   beta > 0 (shape).  This matches the Fisk parameterisation used by
//   flexsurv and eha, and matches Stan native loglogistic_lpdf with
//   alpha replaced by mu.
//
//   PDF:  f(y) = (beta / mu) * (y / mu)^(beta - 1) / [1 + (y / mu)^beta]^2
//   CDF:  F(y) = 1 / [1 + (y / mu)^(-beta)]
//
// Conventions
// -----------
//   * VECTORISED signatures (neodistr convention: loop = FALSE in
//     custom_family).  lpdf / lcdf / lccdf take vectors of y and mu and
//     return a single accumulated log-density / log-CDF / log-CCDF.
//   * rng remains scalar (one draw per call) since brms post-processing
//     does the per-observation looping in R.
//   * Function names match brms's required pattern:
//       hbsae_loglogistic_lpdf  /  hbsae_loglogistic_lcdf  /
//       hbsae_loglogistic_lccdf /  hbsae_loglogistic_rng
//
// References
// ----------
//   Bennett, S. (1983). Log-logistic regression models for survival
//     data. JRSS-C, 32(2), 165-171.  <doi:10.2307/2347295>
//   Kleiber, C., & Kotz, S. (2003). Statistical Size Distributions in
//     Economics and Actuarial Sciences. Wiley.
// =============================================================================

real hbsae_loglogistic_lpdf(vector y, vector mu, real beta) {
  int N = num_elements(y);
  vector[N] z = y ./ mu;
  // sum_i [ log(beta) - log(mu_i) + (beta - 1) log(z_i) - 2 log1p(z_i^beta) ]
  return  N * log(beta)
          - sum(log(mu))
          + (beta - 1) * sum(log(z))
          - 2 * sum(log1p(pow(z, beta)));
}

real hbsae_loglogistic_lcdf(vector y, vector mu, real beta) {
  int N = num_elements(y);
  vector[N] z = y ./ mu;
  // log F(y_i) = -log1p(z_i^(-beta));  return sum over i.
  return -sum(log1p(pow(z, -beta)));
}

real hbsae_loglogistic_lccdf(vector y, vector mu, real beta) {
  int N = num_elements(y);
  vector[N] z = y ./ mu;
  // log(1 - F(y_i)) = -log1p(z_i^beta);  return sum over i.
  return -sum(log1p(pow(z, beta)));
}

real hbsae_loglogistic_rng(real mu, real beta) {
  // Inverse CDF method: q = mu * (u / (1 - u))^(1 / beta)
  real u = uniform_rng(0, 1);
  return mu * pow(u / (1 - u), 1.0 / beta);
}
