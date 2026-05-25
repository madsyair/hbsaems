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
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
head(hbm_data(model))
#> Error: object 'model' not found
# }
```
