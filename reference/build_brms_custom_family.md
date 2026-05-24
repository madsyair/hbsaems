# Build a brms Custom Family + Stanvars Pair from a Single Spec

Convenience function that combines
[`custom_family`](https://paulbuerkner.com/brms/reference/custom_family.html)
and [`stanvar`](https://paulbuerkner.com/brms/reference/stanvar.html)
into the standard `list(custom_family, stanvars_family)` pair returned
by every `brms_custom_*` helper in hbsaems. The Stan code is read
directly from `inst/stan/<name>.stan` – the user does not need to
maintain it as an R string.

## Usage

``` r
build_brms_custom_family(
  name,
  dpars,
  links,
  lb = NA,
  ub = NA,
  type = c("real", "int"),
  loop = FALSE,
  log_lik = NULL,
  posterior_predict = NULL,
  posterior_epred = NULL,
  use_stan_native = FALSE
)
```

## Arguments

- name:

  Character. Family name; must match a `<name>.stan` file under
  `inst/stan/`.

- dpars:

  Character vector of distributional parameter names. The first MUST be
  `"mu"` (brms convention).

- links:

  Character vector of link functions, same length as `dpars`. Common
  values: `"identity"`, `"log"`, `"logit"`.

- lb, ub:

  Numeric vectors of lower / upper bounds; `NA` for none.

- type:

  `"real"` (continuous) or `"int"` (discrete).

- loop:

  Logical. `FALSE` (default) selects the vectorised brms convention
  (Stan signatures take vectors of `y` and `mu`); `TRUE` selects scalar
  Stan signatures. The default matches the neodistr convention.
  Whichever you choose, the corresponding `.stan` file under
  `inst/stan/` must use the same convention.

- log_lik:

  Optional function for computing observation-level log-likelihoods
  (used by [`loo()`](https://mc-stan.org/loo/reference/loo.html),
  [`waic()`](https://mc-stan.org/loo/reference/waic.html)). Signature
  must be `function(i, prep)`. See the brms vignette "Define Custom
  Response Distributions" for details.

- posterior_predict:

  Optional function for drawing from the posterior predictive
  distribution (used by
  [`pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html),
  [`posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)).
  Signature `function(i, prep, ...)`.

- posterior_epred:

  Optional function returning the conditional expectation \\E\[Y \mid
  X\]\\ (used by
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html)).
  Signature `function(prep)`.

- use_stan_native:

  Logical. If `TRUE`, no `stanvars` block is attached – the
  `<name>_lpdf` function is assumed to exist as a Stan **built-in**
  (e.g.\\ `loglogistic_lpdf` in Stan \\\geq\\ 2.29). When `FALSE` (the
  default), the function definition is loaded from
  `inst/stan/<name>.stan`.

## Value

A list with two elements:

- `custom_family`:

  A
  [`brms::customfamily`](https://paulbuerkner.com/brms/reference/custom_family.html)
  object.

- `stanvars_family`:

  A
  [`brms::stanvars`](https://paulbuerkner.com/brms/reference/stanvar.html)
  object with the Stan code in the `functions` block, or `NULL` when
  `use_stan_native = TRUE`.

## Details

Stan code is built once per call; if your code is reused many times,
wrap the returned object in a function and call it lazily.

## See also

[`read_stan_function`](https://madsyair.github.io/hbsaems/reference/read_stan_function.md),
[`register_hbsae_brms_custom`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md).

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)

# Build the loglogistic family.  Stan code is loaded from
# inst/stan/hbsae_loglogistic.stan — the `hbsae_` prefix avoids
# collision with Stan's BUILT-IN `loglogistic_lpdf` (Stan >= 2.29).
ll <- build_brms_custom_family(
  name             = "hbsae_loglogistic",
  dpars            = c("mu", "beta"),
  links            = c("log", "log"),
  lb               = c(0,    0),
  ub               = c(NA,   NA),
  type             = "real"
)
class(ll$custom_family)
#> [1] "customfamily" "brmsfamily"   "family"      
# }
```
