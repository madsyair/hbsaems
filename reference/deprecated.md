# Deprecated Functions

These functions were the primary API in hbsaems \\\le\\ 0.2.x. They are
retained for backwards compatibility but now call the new primary
functions and emit a deprecation warning via
[`.Deprecated`](https://rdrr.io/r/base/Deprecated.html). They will be
removed in **v2.0.0**.

## Usage

``` r
hbcc(
  model,
  diag_tests = c("rhat", "geweke", "heidel", "raftery"),
  plot_types = c("trace", "dens", "acf", "nuts_energy", "rhat", "neff"),
  ...
)

hbmc(
  model,
  model2 = NULL,
  ndraws_ppc = 100,
  moment_match = FALSE,
  moment_match_args = list(),
  reloo_args = list(),
  plot_types = c("pp_check", "params"),
  comparison_metrics = c("loo", "waic", "bf"),
  run_prior_sensitivity = FALSE,
  sensitivity_vars = NULL,
  ...
)

hbpc(model, data, response_var, ndraws_ppc = 50, ...)

hbsae(model, newdata = NULL, ...)
```

## Arguments

- model:

  An `hbmfit` object.

- diag_tests, plot_types, ndraws_ppc, moment_match, moment_match_args,
  reloo_args, comparison_metrics, run_prior_sensitivity,
  sensitivity_vars:

  Forwarded unchanged to the replacement function.

- ...:

  Additional arguments forwarded to the replacement.

- model2:

  For `hbmc`: optional second model.

- data:

  For `hbpc`: the data `data.frame`.

- response_var:

  For `hbpc`: name of the response variable.

- newdata:

  For `hbsae`: optional new data.

## Value

Identical to the corresponding replacement function.

## Migration guide

|  |  |
|----|----|
| **Deprecated** | **Replacement** |
| `hbcc(model, ...)` | [`convergence_check`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)`(model, ...)` |
| `hbmc(model, ...)` | [`model_compare`](https://madsyair.github.io/hbsaems/reference/model_compare.md)`(model, ...)` |
| `hbpc(model, data, response_var)` | [`prior_check`](https://madsyair.github.io/hbsaems/reference/prior_check.md)`(model, data, response_var)` |
| `hbsae(model, ...)` | [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)`(model, ...)` |
