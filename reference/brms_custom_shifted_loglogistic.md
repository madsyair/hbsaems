# Shifted Loglogistic as a Custom Distribution Family for brms

Returns the `custom_family` + `stanvars` pair required to fit a brms
model with a Shifted Loglogistic response distribution. The Stan code is
loaded from `inst/stan/shifted_loglogistic.stan` via
[`build_brms_custom_family`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md);
post-processing functions (`log_lik`, `posterior_predict`,
`posterior_epred`) are wired in automatically.

## Usage

``` r
brms_custom_shifted_loglogistic()
```

## Value

A list with elements `custom_family` and `stanvars_family` ready for use
with [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## Details

Three parameters:

- `mu`:

  Location parameter (identity link).

- `sigma`:

  Scale parameter (`sigma > 0`, log link).

- `xi`:

  Shape parameter (identity link).

## See also

[`dshifted_loglogistic`](https://madsyair.github.io/hbsaems/reference/shifted_loglogistic.md),
[`build_brms_custom_family`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md),
[`register_hbsae_brms_custom`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md).

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
sll <- brms_custom_shifted_loglogistic()
# fit <- brm(y ~ x, data = d,
#            family   = sll$custom_family,
#            stanvars = sll$stanvars_family)
# }
```
