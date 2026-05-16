# Compute Credible Intervals for an hbmfit Object

The
[`posterior_interval`](https://mc-stan.org/rstantools/reference/posterior_interval.html)
generic is re-exported from rstantools and an S3 method is provided that
dispatches on `hbmfit` objects. This lets users call
`posterior_interval(fit)` on the return value of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) just as
they would on a `brmsfit`.

## Usage

``` r
# S3 method for class 'hbmfit'
posterior_interval(object, prob = 0.95, params = NULL, ...)
```

## Arguments

- object:

  An `hbmfit` object.

- prob:

  Coverage probability in \\(0, 1)\\ (default `0.95`; note that
  [`rstantools::posterior_interval`](https://mc-stan.org/rstantools/reference/posterior_interval.html)'s
  own default is `0.9`).

- params:

  Optional character vector of parameter names to keep.

- ...:

  Additional arguments forwarded to
  [`posterior_draws`](https://madsyair.github.io/hbsaems/reference/posterior_draws.md).

## Value

A matrix with two rows giving lower and upper bounds.

## See also

[`posterior_interval`](https://mc-stan.org/rstantools/reference/posterior_interval.html)

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
