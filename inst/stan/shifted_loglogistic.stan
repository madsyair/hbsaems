// =============================================================================
// inst/stan/shifted_loglogistic.stan
//
// Stan function definitions for the Shifted (3-parameter) Loglogistic
// distribution.
//
// Parameterisation (Hosking & Wallis 1997):
//   Y ~ ShiftedLoglogistic(mu, sigma, xi),  mu in R, sigma > 0, xi in R.
//   z = 1 + xi * (y - mu) / sigma,  with support  z > 0.
//   PDF: f(y) = (1 / sigma) * z^(-1 - 1/xi) * (1 + z^(-1/xi))^(-2)
//   CDF: F(y) = (1 + z^(-1/xi))^(-1)
//
// Numerical stability:
//   When |xi| < tol the distribution degenerates to the Logistic.
//   We branch on this to avoid 1/xi blowing up.
//
// Conventions:
//   * VECTORISED signatures (neodistr convention: loop = FALSE).
//   * rng remains scalar.
// =============================================================================

real shifted_loglogistic_lpdf(vector y, vector mu, real sigma, real xi) {
  int N = num_elements(y);
  real tol = 1e-8;
  // Limiting Logistic when |xi| ~ 0
  if (fabs(xi) < tol) {
    real lp = 0;
    for (n in 1:N)
      lp += logistic_lpdf(y[n] | mu[n], sigma);
    return lp;
  }
  // General case: accumulate log-density
  real lp = -N * log(sigma);
  for (n in 1:N) {
    real z = 1 + xi * (y[n] - mu[n]) / sigma;
    if (z <= 0) return negative_infinity();
    lp += -(1 + 1.0 / xi) * log(z)
          - 2 * log1p(pow(z, -1.0 / xi));
  }
  return lp;
}

real shifted_loglogistic_lcdf(vector y, vector mu, real sigma, real xi) {
  int N = num_elements(y);
  real tol = 1e-8;
  if (fabs(xi) < tol) {
    real lc = 0;
    for (n in 1:N)
      lc += logistic_lcdf(y[n] | mu[n], sigma);
    return lc;
  }
  real lc = 0;
  for (n in 1:N) {
    real z = 1 + xi * (y[n] - mu[n]) / sigma;
    if (z <= 0) return negative_infinity();
    lc += -log1p(pow(z, -1.0 / xi));
  }
  return lc;
}

real shifted_loglogistic_lccdf(vector y, vector mu, real sigma, real xi) {
  int N = num_elements(y);
  real tol = 1e-8;
  if (fabs(xi) < tol) {
    real lcc = 0;
    for (n in 1:N)
      lcc += logistic_lccdf(y[n] | mu[n], sigma);
    return lcc;
  }
  real lcc = 0;
  for (n in 1:N) {
    real z = 1 + xi * (y[n] - mu[n]) / sigma;
    if (z <= 0) return 0;
    lcc += -log1p(pow(z, 1.0 / xi));
  }
  return lcc;
}

real shifted_loglogistic_rng(real mu, real sigma, real xi) {
  real tol = 1e-8;
  real u = uniform_rng(0, 1);
  if (fabs(xi) < tol) {
    return logistic_rng(mu, sigma);
  }
  real z = pow(u / (1 - u), xi);
  return mu + sigma * (z - 1) / xi;
}
