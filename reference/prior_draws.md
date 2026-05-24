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
#> Start sampling
#> Warning: There were 33 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
pd <- prior_draws(model)
head(pd)
#>     Intercept          b      sigma sd_regency
#> 1   4.5212865 -0.7260695  0.9750607 1.05467980
#> 2  -9.0425102 -0.5262104  4.6442266 0.04686317
#> 3 -15.0989406  0.3201885  1.5977276 1.40752062
#> 4   1.9226259 -1.5921734  0.3290050 0.05226818
#> 5  -0.1195674 -0.3816942 10.3937491 3.91080435
#> 6 -15.5548247  1.7450421  0.3189977 4.09202354
# }
```
