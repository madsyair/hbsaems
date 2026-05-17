# Loglogistic Distribution Functions

Density, distribution function, quantile function, and random generation
for the loglogistic (Fisk) distribution with scale parameter `mu > 0`
and shape parameter `beta > 0`.

## Usage

``` r
dloglogistic(x, mu = 1, beta = 1, log = FALSE)

ploglogistic(q, mu = 1, beta = 1, lower.tail = TRUE, log.p = FALSE)

qloglogistic(p, mu = 1, beta = 1, lower.tail = TRUE, log.p = FALSE)

rloglogistic(n, mu = 1, beta = 1)
```

## Arguments

- x, q:

  Vector of quantiles (`x > 0` for non-zero density).

- mu:

  Scale parameter (`mu > 0`; equals the median).

- beta:

  Shape parameter (`beta > 0`).

- log, log.p:

  Logical; if `TRUE`, return the log density / log probability.

- lower.tail:

  Logical; if `TRUE` (default) probabilities are \\P\[Y \le q\]\\,
  otherwise \\P\[Y \> q\]\\.

- p:

  Vector of probabilities (`0 <= p <= 1`).

- n:

  Number of random draws.

## Value

Numeric vector of the same length as the input.

## Parameterisation

This implementation follows the canonical Wikipedia / flexsurv / eha
parameterisation (Jackson 2016; Bennett 1983): \$\$Y \sim
\mathrm{LogLogistic}(\mu, \beta), \quad \mu \> 0, \quad \beta \> 0,\$\$
with probability density function \$\$f(y \mid \mu, \beta) =
\frac{(\beta/\mu)(y/\mu)^{\beta - 1}}{\\1 + (y/\mu)^{\beta}\\^{2}},
\quad y \> 0,\$\$ cumulative distribution function \$\$F(y \mid \mu,
\beta) = \\1 + (y/\mu)^{-\beta}\\^{-1},\$\$ median \\\mu\\, and mean
\\E\[Y\] = \mu \pi / \[\beta \sin(\pi / \beta)\]\\ when \\\beta \> 1\\.
Equivalently, \\\log(Y) \sim \mathrm{Logistic}(\log\mu, \\ 1/\beta)\\.

**Why not match the brms `lognormal` convention?** The
[`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
family parameterises \\\mu\\ on the log scale (so \\\mu\\ is
unconstrained and uses an identity link). Doing the same for the
log-logistic would require redefining \\\mu = \log(\mathrm{median}(Y))\\
– which deviates from every standard R reference (flexsurv, eha,
Wolfram, scipy, Stata). We deliberately follow the survival-analysis
convention instead: \\\mu\\ is the median (positive, log link), keeping
interpretation simple and posterior summaries comparable with the rest
of the R survival ecosystem.

## References

Bennett, S. (1983). Log-logistic regression models for survival data.
*Journal of the Royal Statistical Society, Series C*, 32(2), 165-171.
[doi:10.2307/2347295](https://doi.org/10.2307/2347295)

Jackson, C. H. (2016). flexsurv: A platform for parametric survival
modelling in R. *Journal of Statistical Software*, 70(8), 1-33.
[doi:10.18637/jss.v070.i08](https://doi.org/10.18637/jss.v070.i08)

Kleiber, C., & Kotz, S. (2003). *Statistical Size Distributions in
Economics and Actuarial Sciences*. Wiley.

## Examples

``` r
dloglogistic(c(0.5, 1, 2),  mu = 1, beta = 2)
#> [1] 0.64 0.50 0.16
ploglogistic(c(0.5, 1, 2),  mu = 1, beta = 2)
#> [1] 0.2 0.5 0.8
qloglogistic(c(0.25, 0.75), mu = 1, beta = 2)
#> [1] 0.5773503 1.7320508
set.seed(1); rloglogistic(5, mu = 1, beta = 2)
#> [1] 0.6012374 0.7698512 1.1580658 3.1455001 0.5026267
```
