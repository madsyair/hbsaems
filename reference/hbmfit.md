# User-Facing Helper to Build an hbmfit Object

Convenience constructor that calls
[`new_hbmfit`](https://madsyair.github.io/hbsaems/reference/new_hbmfit.md)
then
[`validate_hbmfit`](https://madsyair.github.io/hbsaems/reference/validate_hbmfit.md)
– the safe public entry-point if you ever need to build an `hbmfit`
object manually (e.g.\\ when adapting a non-brms model). In normal usage
you do *not* need to call this:
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) and the
distribution-specific wrappers do it for you.

## Usage

``` r
hbmfit(model, data, missing_method = NULL)
```

## Arguments

- model:

  A `brmsfit` or `brmsfit_multiple` object.

- data:

  The `data.frame` used to fit `model`.

- missing_method:

  Character scalar or `NULL`. One of `"deleted"`, `"multiple"`,
  `"model"`.

## Value

A validated `hbmfit` object.

## See also

[`new_hbmfit`](https://madsyair.github.io/hbsaems/reference/new_hbmfit.md),
[`validate_hbmfit`](https://madsyair.github.io/hbsaems/reference/validate_hbmfit.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# \donttest{
raw <- brms::brm(y ~ x1, data = data.frame(y = rnorm(10), x1 = 1:10),
                 chains = 1, iter = 200, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
fit <- hbmfit(model = raw,
              data  = data.frame(y = rnorm(10), x1 = 1:10),
              missing_method = NULL)
#> Error in new_hbmfit(model = model, missing_method = missing_method, data = data): inherits(model, c("brmsfit", "brmsfit_multiple")) is not TRUE
validate_hbmfit(fit)
#> Error: object 'fit' not found
# }
```
