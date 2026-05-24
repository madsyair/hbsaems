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
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
hbm_info(model)
#> Error: object 'model' not found
# }
```
