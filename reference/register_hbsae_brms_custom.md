# Register a brms Custom Family with the hbsaems Model Registry

Wraps a
[`brms::custom_family`](https://paulbuerkner.com/brms/reference/custom_family.html) +
`stanvars` pair into a model spec usable by
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) and the
flexible factory
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).
The custom likelihood is then available throughout the package – in
[`run_sae_app`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md),
in
[`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md),
in the Shiny application, and so on – as if it were a built-in family.

## Usage

``` r
register_hbsae_brms_custom(
  key,
  custom_family,
  stanvars,
  response_check = NULL,
  response_check_msg = NULL,
  supports_mi = FALSE,
  discrete = FALSE,
  overwrite = FALSE
)
```

## Arguments

- key:

  Character. Unique registry key (e.g.\\ `"loglogistic"`).

- custom_family:

  A
  [`brms::custom_family`](https://paulbuerkner.com/brms/reference/custom_family.html)
  object describing the parameters, links, and constraints of the
  distribution. Typically produced by
  [`brms::custom_family()`](https://paulbuerkner.com/brms/reference/custom_family.html)
  or by a helper such as
  [`brms_custom_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_loglogistic.md).

- stanvars:

  A
  [`brms::stanvars`](https://paulbuerkner.com/brms/reference/stanvar.html)
  object containing the Stan-side function definitions (log-PDF /
  log-CDF / RNG).

- response_check:

  Optional function `function(y) -> logical` for response-domain
  validation (e.g.\\ positivity check for the loglogistic).

- response_check_msg:

  Character. Error message if `response_check` fails.

- supports_mi:

  Logical. Whether
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html) can
  impute the response under this likelihood (default `FALSE`; custom
  families typically do not support
  [`mi()`](https://paulbuerkner.com/brms/reference/mi.html)).

- discrete:

  Logical. Whether the family is discrete (default `FALSE`).

- overwrite:

  Logical. If `TRUE`, replace an existing entry with the same `key`.

## Value

Invisibly returns the registered model spec (a list).

## Details

After registration, the family is usable in any of the following ways:


      # Direct via the registry key
      fit <- hbm_flex("loglogistic", response = "y",
                       predictors = c("x1", "x2"),
                       data = d, re = ~ (1 | area))

      # Direct via hbm() (canonical):
      fit <- hbm(brms::bf(y ~ x1 + x2 + (1 | area)),
                 data = d,
                 hb_sampling = "loglogistic")

Internally, [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)
detects that the registered family is a
[`brms::custom_family`](https://paulbuerkner.com/brms/reference/custom_family.html)
and (i) passes the family object directly to
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
instead of constructing a built-in
[`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
and (ii) merges the family's stanvars with any user-supplied `stanvars`.

## See also

[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md),
[`brms_custom_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_loglogistic.md),
[`brms_custom_shifted_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_shifted_loglogistic.md),
[brms vignette on custom
families](https://paulbuerkner.com/brms/articles/brms_customfamilies.html).

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)

# Loglogistic (built in to hbsaems v0.6.0; this just shows the registration mechanism)
ll <- brms_custom_loglogistic()
register_hbsae_brms_custom(
  key             = "loglogistic_user",
  custom_family   = ll$custom_family,
  stanvars        = ll$stanvars_family,
  response_check  = function(y) all(y > 0, na.rm = TRUE),
  response_check_msg = "Loglogistic response must be positive."
)
"loglogistic_user" %in% list_hbsae_models()
#> [1] TRUE
# }
```
