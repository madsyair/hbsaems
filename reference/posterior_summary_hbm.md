# Comprehensive Posterior Summary

Returns fixed effects, random effects, and model-fit statistics in a
single named list.

## Usage

``` r
posterior_summary_hbm(model, probs = c(0.025, 0.975), ...)
```

## Arguments

- model:

  An `hbmfit` object.

- probs:

  Probability bounds for credible intervals (default `c(0.025, 0.975)`).

- ...:

  Additional arguments forwarded to the underlying brms functions.

## Value

A named list with components `fixed_effects`, `random_effects`, and
`model_fit` (containing `loo`, `waic`, and `R2`).

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | regency),    # area-level random effect
             chains = 2, iter = 1000, warmup = 500,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
s <- posterior_summary_hbm(model)
#> Error: object 'model' not found
s$fixed_effects
#> Error in s$fixed_effects: object of type 'closure' is not subsettable
# }
```
