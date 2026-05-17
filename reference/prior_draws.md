# Extract Prior Draws

The
[`prior_draws`](https://paulbuerkner.com/brms/reference/prior_draws.brmsfit.html)
generic is re-exported from brms and an S3 method is provided that
dispatches on `hbmfit` objects. Requires the model to have been fit with
`sample_prior = "yes"` or `sample_prior = "only"`.

## Usage

``` r
# S3 method for class 'hbmfit'
prior_draws(x, ...)
```

## Arguments

- x:

  An `hbmfit` object.

- ...:

  Additional arguments forwarded to
  [`prior_draws`](https://paulbuerkner.com/brms/reference/prior_draws.brmsfit.html).

## Value

A `data.frame` of prior draws or `NULL` if no prior samples were stored
in the model.

## See also

[`prior_draws`](https://paulbuerkner.com/brms/reference/prior_draws.brmsfit.html)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
# `sample_prior = "yes"` works best when all coefficients have a
# proper prior; supply explicit priors on the regression class.
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | regency),    # area-level random effect
             sample_prior = "yes",
             prior        = c(
               brms::prior(normal(0, 1), class = "b"),
               brms::prior(normal(0, 5), class = "Intercept")
             ),
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
