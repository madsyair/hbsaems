# Validate an hbmfit Object

Public validator for the
[`hbmfit-class`](https://madsyair.github.io/hbsaems/reference/hbmfit-class.md)`{hbmfit}`
S3 class. Runs all invariants that the cheap constructor
([`new_hbmfit`](https://madsyair.github.io/hbsaems/reference/new_hbmfit.md))
is permitted to skip. Useful when reconstructing an `hbmfit` object
manually, reading one back from disk, or testing custom family wrappers.

## Usage

``` r
validate_hbmfit(x)
```

## Arguments

- x:

  An object to validate.

## Value

The input `x`, invisibly, when all invariants hold. Otherwise raises an
informative error.

## Details

Invariants verified:

1.  Object is a list with class `"hbmfit"`.

2.  Has mandatory slots: `model`, `missing_method`, `data`.

3.  `model` inherits from `brmsfit` or `brmsfit_multiple`.

4.  `missing_method` is `NULL` or a single character string in
    `c("deleted", "multiple", "model")`.

5.  `data` is a `data.frame` with \\\ge 1\\ row.

6.  The `handle_missing` alias (if present) equals `missing_method`.

## See also

[`new_hbmfit`](https://madsyair.github.io/hbsaems/reference/new_hbmfit.md),
[`hbmfit`](https://madsyair.github.io/hbsaems/reference/hbmfit.md)

## Examples

``` r
# \donttest{
# Minimal example without area-level RE (fixed-effects baseline) --
# suppress the area-RE advisory because this 5-row toy dataset cannot
# meaningfully estimate a random effect.  Uses brms-default MCMC
# settings (chains = 4, iter = 2000, warmup = 1000); on this toy
# data the fit is only used to verify the hbmfit class structure,
# not for inference.
fit <- suppressWarnings(
  hbm(brms::bf(y ~ x1), data = data.frame(y = rnorm(5), x1 = 1:5),
      chains = 4, iter = 2000, warmup = 1000, refresh = 0)
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
validate_hbmfit(fit)
#> Error: object 'fit' not found
# }
```
