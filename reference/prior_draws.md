# Extract Prior Draws

Requires the model to have been fit with `sample_prior = "yes"` or
`sample_prior = "only"`.

## Usage

``` r
prior_draws(model, ...)
```

## Arguments

- model:

  An `hbmfit` object.

- ...:

  Additional arguments forwarded to
  [`prior_draws`](https://paulbuerkner.com/brms/reference/prior_draws.brmsfit.html).

## Value

A `data.frame` of prior draws or `NULL` if no prior samples were stored
in the model.

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | group),    # area-level random effect
             sample_prior = "yes",
             chains = 2, iter = 1000, warmup = 500,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
pd <- prior_draws(model)
#> Error: object 'model' not found
head(pd)
#> Error: object 'pd' not found
# }
```
