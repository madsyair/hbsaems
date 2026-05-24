# Get Model Warnings

Inspects the fitted model for common convergence problems and returns a
character vector of human-readable warnings. When no warnings apply, the
string `"No warnings detected."` is returned.

## Usage

``` r
hbm_warnings(model)
```

## Arguments

- model:

  An `hbmfit` object.

## Value

A character vector of warning messages.

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
hbm_warnings(model)
#> Error: object 'model' not found
# }
```
