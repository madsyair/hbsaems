# Loglogistic Distribution Functions

Density, distribution function, quantile function, and random generation
for the loglogistic distribution with scale parameter `mu > 0` and shape
parameter `beta > 0`.

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

## Details

The probability density function is \$\$f(y) = (\beta/\mu)
(y/\mu)^{\beta - 1} \\1 + (y/\mu)^{\beta}\\^{-2}, \quad y \> 0,\$\$ with
cumulative distribution function \$\$F(y) = \\1 +
(y/\mu)^{-\beta}\\^{-1}.\$\$

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
