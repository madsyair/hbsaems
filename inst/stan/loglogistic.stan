// =============================================================================
// inst/stan/loglogistic.stan
//
// Stan function definitions for the Loglogistic distribution.
//
// Parameterisation:
//   Y ~ Loglogistic(mu, beta) with mu > 0 (scale) and beta > 0 (shape).
//   PDF: f(y) = (beta / mu) * (y / mu)^(beta - 1) / [1 + (y / mu)^beta]^2
//   CDF: F(y) = 1 / [1 + (y / mu)^(-beta)]
//
// Conventions:
//   * VECTORISED signatures (neodistr convention: loop = FALSE in
//     custom_family).  lpdf / lcdf / lccdf take vectors of y and mu and
//     return a single accumulated log-density / log-CDF / log-CCDF.
//   * rng remains scalar (one draw per call) since brms post-processing
//     does the per-observation looping in R.
//   * Function names match brms's required pattern:
//       <name>_lpdf  /  <name>_lcdf  /  <name>_lccdf  /  <name>_rng
// =============================================================================

real loglogistic_lpdf(vector y, vector mu, real beta) {
  int N = num_elements(y);
  vector[N] z = y ./ mu;
  // sum_i [ log(beta) - log(mu_i) + (beta - 1) log(z_i) - 2 log1p(z_i^beta) ]
  return  N * log(beta)
          - sum(log(mu))
          + (beta - 1) * sum(log(z))
          - 2 * sum(log1p(pow(z, beta)));
}

real loglogistic_lcdf(vector y, vector mu, real beta) {
  int N = num_elements(y);
  vector[N] z = y ./ mu;
  // log F(y_i) = -log1p(z_i^(-beta));  return sum over i.
  return -sum(log1p(pow(z, -beta)));
}

real loglogistic_lccdf(vector y, vector mu, real beta) {
  int N = num_elements(y);
  vector[N] z = y ./ mu;
  // log(1 - F(y_i)) = -log1p(z_i^beta);  return sum over i.
  return -sum(log1p(pow(z, beta)));
}

real loglogistic_rng(real mu, real beta) {
  // Inverse CDF method: q = mu * (u / (1 - u))^(1 / beta)
  real u = uniform_rng(0, 1);
  return mu * pow(u / (1 - u), 1.0 / beta);
}
