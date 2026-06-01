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
             re = ~ (1 | regency),    # area-level random effect
             chains = 4, iter = 2000, warmup = 1000,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 32 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.44, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
draws <- posterior_draws(model)
dim(draws)
#> [1] 4000  207
# }
```
