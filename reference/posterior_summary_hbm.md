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
s <- posterior_summary_hbm(model)
#> Warning: Found 72 observations with a pareto_k > 0.7 in model 'model$model'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations. 
#> Warning: 
#> 100 (100.0%) p_waic estimates greater than 0.4. We recommend trying loo instead.
s$fixed_effects
#>            Estimate Est.Error      Q2.5     Q97.5
#> Intercept 10.023630 0.1618825 9.6820909 10.340991
#> x1         1.020974 0.1526696 0.7234305  1.321132
# }
```
