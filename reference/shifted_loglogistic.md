# Shifted (3-Parameter) Loglogistic Distribution

Density, distribution function, quantile function and random generation
for the shifted log-logistic (generalised log-logistic) distribution
with location `mu` (real), scale `sigma > 0` and shape `xi` (real). The
two-parameter logistic distribution is recovered as `xi -> 0`.

## Usage

``` r
dshifted_loglogistic(x, mu = 0, sigma = 1, xi = 0, log = FALSE)

pshifted_loglogistic(
  q,
  mu = 0,
  sigma = 1,
  xi = 0,
  lower.tail = TRUE,
  log.p = FALSE
)

qshifted_loglogistic(
  p,
  mu = 0,
  sigma = 1,
  xi = 0,
  lower.tail = TRUE,
  log.p = FALSE
)

rshifted_loglogistic(n, mu = 0, sigma = 1, xi = 0)
```

## Arguments

- x, q:

  Numeric vector of quantiles.

- mu:

  Location parameter (real; equals the median).

- sigma:

  Scale parameter (`sigma > 0`).

- xi:

  Shape parameter (real; `xi = 0` gives the logistic distribution).

- log, log.p:

  Logical. See
  [`Distributions`](https://rdrr.io/r/stats/Distributions.html).

- lower.tail:

  Logical. See
  [`Distributions`](https://rdrr.io/r/stats/Distributions.html).

- p:

  Vector of probabilities.

- n:

  Number of random draws.

## Value

Numeric vector.

## Parameterisation

This implementation uses the **GEV-style parameterisation** of Hosking &
Wallis (1997) and the Flood Estimation Handbook (Robson & Reed 1999), in
which \\\mu\\ is a pure location parameter (the median), \\\sigma\\ a
pure scale parameter and \\\xi\\ a pure shape parameter: \$\$F(x \mid
\mu, \sigma, \xi) = \\1 + (1 + \xi z)^{-1/\xi}\\^{-1}, \qquad z = (x -
\mu) / \sigma,\$\$ with corresponding density \$\$f(x \mid \mu, \sigma,
\xi) = \frac{(1 + \xi z)^{-(1/\xi + 1)}} {\sigma \\1 + (1 + \xi
z)^{-1/\xi}\\^{2}}.\$\$

The support depends on \\\xi\\:

- \\\xi \> 0\\: \\x \ge \mu - \sigma/\xi\\ (bounded below).

- \\\xi \< 0\\: \\x \le \mu - \sigma/\xi\\ (bounded above).

- \\\xi = 0\\: \\x \in \mathbb{R}\\ (logistic limit).

The median is always \\\mu\\; the mean exists when \\\|\xi\| \< 1\\ and
is \\\mu + \sigma (\alpha\csc\alpha - 1)/\xi\\, \\\alpha = \pi\xi\\.
Reducing further, the family contains:

- the standard log-logistic when \\\xi = 1\\ (reparameterised);

- the logistic distribution as \\\xi \to 0\\;

- the generalised Pareto family at \\\xi = -1\\.

**Why this parameterisation?** An alternative "simple-shift" form, \\Y -
\delta \sim \mathrm{LogLogistic}\\, exists in the literature (Geskus
2001) and is closer in spirit to
[`brms::shifted_lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)'s
positive shift `ndt`. We deliberately follow the GEV-style
parameterisation because

1.  it provides a *smooth* limit to the logistic distribution at \\\xi =
    0\\;

2.  the parameters \\(\mu, \sigma, \xi)\\ are orthogonally interpretable
    (location / scale / shape);

3.  it is the canonical form in hydrology and extreme-value applications
    (Hosking & Wallis 1997).

## References

Geskus, R. B. (2001). Methods for estimating the AIDS incubation time
distribution when date of seroconversion is censored. *Statistics in
Medicine*, 20(5), 795-812.

Hosking, J. R. M., & Wallis, J. R. (1997). *Regional Frequency Analysis:
An Approach Based on L-Moments*. Cambridge University Press. ISBN
0-521-43045-3.

Robson, A., & Reed, D. (1999). Flood Estimation Handbook, Volume 3:
Statistical Procedures for Flood Frequency Estimation. Institute of
Hydrology, Wallingford, UK.

## Examples

``` r
dshifted_loglogistic(c(1, 2, 5),    mu = 0, sigma = 1, xi = 0.5)
#> [1] 0.14201183 0.08000000 0.01993592
pshifted_loglogistic(c(1, 2, 5),    mu = 0, sigma = 1, xi = 0.5)
#> [1] 0.6923077 0.8000000 0.9245283
qshifted_loglogistic(c(0.25, 0.75), mu = 0, sigma = 1, xi = 0.5)
#> [1] -0.8452995  1.4641016
set.seed(1); rshifted_loglogistic(5, mu = 0, sigma = 1, xi = 0.5)
#> [1] -0.7975251 -0.4602975  0.3161316  4.2910001 -0.9947467
```
