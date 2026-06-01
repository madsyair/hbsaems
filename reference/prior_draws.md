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
             chains = 4, iter = 2000, warmup = 1000,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 72 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.15, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
pd <- prior_draws(model)
head(pd)
#>   Intercept          b    sigma sd_regency
#> 1  1.203556 -0.1796873 1.500267  0.7143109
#> 2  2.775636  0.1524650 5.137378  1.3279995
#> 3  5.087399  0.9773841 3.053966  0.1240028
#> 4 -1.723898  0.6596110 3.850694  0.8130879
#> 5  2.699549 -0.3961863 3.677017  3.4173558
#> 6  6.199769 -1.4724376 1.989079  2.6545391
# }
```
