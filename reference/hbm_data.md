# Return the Data Used to Fit an hbmfit

Return the Data Used to Fit an hbmfit

## Usage

``` r
hbm_data(model)
```

## Arguments

- model:

  An `hbmfit` object.

## Value

The original `data.frame` passed to the fitting function.

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
head(hbm_data(model))
#>           y         D         x1         x2         x3 theta_true           u
#> 1  6.982249 0.9213655 -0.9026345  0.7116302  1.4305563   8.498168 -0.85307599
#> 2  9.005801 2.6432183 -0.7526745 -0.8779797 -0.3182451   9.726882 -0.01449469
#> 3  6.216509 1.7035092 -0.2605613 -0.6066989 -1.0293040   8.535629 -1.25047984
#> 4 10.469687 0.6586857  0.0759456 -1.0766939 -1.0711613   9.678839 -0.59891604
#> 5 10.620157 1.0166936  0.2117701 -0.5022666 -0.4964679  11.856111  1.58450157
#> 6 12.045742 0.5881090  0.6544500 -0.3750112  0.3802786  12.075297  1.25014821
#>       regency    province
#> 1 regency_001 province_01
#> 2 regency_002 province_01
#> 3 regency_003 province_01
#> 4 regency_004 province_01
#> 5 regency_005 province_01
#> 6 regency_006 province_01
# }
```
