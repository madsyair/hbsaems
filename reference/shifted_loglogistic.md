# Shifted (3-Parameter) Loglogistic Distribution

Density, distribution function, quantile function and random generation
for the shifted loglogistic distribution with location `mu`, scale
`sigma > 0` and shape `xi`. The standard 2-parameter logistic is
recovered as `xi -> 0`.

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

  Location parameter (real).

- sigma:

  Scale parameter (`sigma > 0`).

- xi:

  Shape parameter (real; `xi = 0` gives logistic).

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
