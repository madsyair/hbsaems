# Compute Credible Intervals

Compute Credible Intervals

## Usage

``` r
posterior_interval(model, params = NULL, prob = 0.95, ...)
```

## Arguments

- model:

  An `hbmfit` object.

- params:

  Optional character vector of parameter names.

- prob:

  Coverage probability in \\(0, 1)\\ (default `0.95`).

- ...:

  Additional arguments forwarded to `posterior_draws`.

## Value

A matrix with two rows giving lower and upper bounds.

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | group),    # area-level random effect
             chains = 2, iter = 1000, warmup = 500,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
posterior_interval(model, prob = 0.90)
#> Error: object 'model' not found
# }
```
