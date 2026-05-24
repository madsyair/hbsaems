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
#> Start sampling
#> Warning: There were 40 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.1, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
s <- posterior_summary_hbm(model)
#> Warning: Found 58 observations with a pareto_k > 0.67 in model 'model$model'. We recommend to run more iterations to get at least about 2200 posterior draws to improve LOO-CV approximation accuracy.
#> Warning: 
#> 28 (28.0%) p_waic estimates greater than 0.4. We recommend trying loo instead.
s$fixed_effects
#>            Estimate Est.Error      Q2.5     Q97.5
#> Intercept 10.052590 0.1511257 9.7532626 10.366862
#> x1         1.014333 0.1496188 0.7078825  1.293283
# }
```
