# Loglogistic as a Custom Distribution Family for brms

Returns the `custom_family` + `stanvars` pair required to fit a brms
model with a Loglogistic response distribution. The Stan code is loaded
from `inst/stan/loglogistic.stan` via
[`build_brms_custom_family`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md);
post-processing functions (`log_lik`, `posterior_predict`,
`posterior_epred`) are wired in automatically.

## Usage

``` r
brms_custom_loglogistic()
```

## Value

A list with elements `custom_family` and `stanvars_family` ready for use
with [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).
The returned `custom_family` object has `log_lik`, `posterior_predict`,
and `posterior_epred` registered so that
[`brms::loo()`](https://mc-stan.org/loo/reference/loo.html),
[`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html),
and
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
work without further setup.

## Details

Two parameters:

- `mu`:

  Scale (`mu > 0`, log link).

- `beta`:

  Shape (`beta > 0`, log link).

## See also

[`dloglogistic`](https://madsyair.github.io/hbsaems/reference/loglogistic.md),
[`build_brms_custom_family`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md),
[`register_hbsae_brms_custom`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md).

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: ‘brms’
#> The following object is masked from ‘package:stats’:
#> 
#>     ar
ll <- brms_custom_loglogistic()
# fit <- brm(y ~ x, data = d,
#            family   = ll$custom_family,
#            stanvars = ll$stanvars_family)
# loo(fit)                     # uses log_lik_loglogistic
# posterior_predict(fit)       # uses posterior_predict_loglogistic
# posterior_epred(fit)         # uses posterior_epred_loglogistic
# }
```
