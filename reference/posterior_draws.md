# Extract Posterior Draws as a Matrix

Extract Posterior Draws as a Matrix

## Usage

``` r
posterior_draws(model, params = NULL, ...)
```

## Arguments

- model:

  An `hbmfit` object.

- params:

  Optional character vector of parameter names; `NULL` (default) returns
  all parameters.

- ...:

  Additional arguments forwarded to
  [`as_draws_matrix`](https://mc-stan.org/posterior/reference/draws_matrix.html).

## Value

A draws matrix (rows = MCMC iterations, columns = parameters).

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
draws <- posterior_draws(model)
#> Error: object 'model' not found
dim(draws)
#> Error: object 'draws' not found
# }
```
