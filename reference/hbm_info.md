# Get Comprehensive Model Information

Returns a one-page summary of the fitted model's metadata: number of
observations, family, link function, formula, MCMC settings,
missing-data strategy, and so on.

## Usage

``` r
hbm_info(model)
```

## Arguments

- model:

  An `hbmfit` object.

## Value

A named list of metadata.

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
hbm_info(model)
#> $n_obs
#> [1] 100
#> 
#> $family
#> [1] "gaussian"
#> 
#> $link
#> [1] "identity"
#> 
#> $formula
#>  [1] "structure(list(formula = y ~ x1 + (1 | regency), pforms = list(), "              
#>  [2] "    pfix = list(), resp = \"y\", family = structure(list(family = \"gaussian\", "
#>  [3] "        link = \"identity\", linkfun = function (mu) "                           
#>  [4] "        link(mu, link = slink), linkinv = function (eta) "                       
#>  [5] "        inv_link(eta, link = slink), dpars = c(\"mu\", \"sigma\"), "             
#>  [6] "        type = \"real\", ybounds = c(-Inf, Inf), closed = c(NA, "                
#>  [7] "        NA), ad = c(\"weights\", \"subset\", \"se\", \"cens\", \"trunc\", "      
#>  [8] "        \"mi\", \"index\"), normalized = c(\"_time_hom\", \"_time_het\", "       
#>  [9] "        \"_lagsar\", \"_errorsar\", \"_fcor\"), specials = c(\"residuals\", "    
#> [10] "        \"rescor\"), link_sigma = \"log\"), class = c(\"brmsfamily\", "          
#> [11] "    \"family\")), mecor = TRUE), class = c(\"brmsformula\", \"bform\""           
#> [12] "))"                                                                              
#> 
#> $chains
#> [1] 2
#> 
#> $iter
#> [1] 1000
#> 
#> $warmup
#> [1] 500
#> 
#> $missing_method
#> [1] "none"
#> 
#> $has_re
#> [1] TRUE
#> 
#> $n_parameters
#> [1] 207
#> 
# }
```
