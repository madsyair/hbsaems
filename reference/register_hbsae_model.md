# Register a Custom HBSAE Model

Adds a new model spec to the hbsaems model registry so that it can be
used by [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) and
the flexible factory
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).
Useful for extending the package with new likelihoods (e.g.\\ *Gamma*,
*Tweedie*, *Skew-Normal*), new link functions, or new
auxiliary-parameter hyperpriors without modifying hbsaems source.

## Usage

``` r
register_hbsae_model(
  key,
  family,
  link = "identity",
  discrete = FALSE,
  supports_mi = !discrete,
  has_addition_term = FALSE,
  addition_template = NULL,
  response_check = function(y) TRUE,
  response_check_msg = NULL,
  default_priors = function(...) NULL,
  aux_param_hyperprior = NULL,
  overwrite = FALSE
)
```

## Arguments

- key:

  Character. Unique identifier (e.g.\\ `"gamma_log"`).

- family:

  Character. The brms family name passed to `hb_sampling`.

- link:

  Character. Default link function (default `"identity"`).

- discrete:

  Logical. Is the response discrete? Affects whether
  `handle_missing = "model"` (joint Bayesian imputation via
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html)) is
  allowed (default `FALSE`).

- supports_mi:

  Logical. Whether
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html) can
  impute the response variable for this family (default `!discrete`).

- has_addition_term:

  Logical. Whether the LHS uses an addition term such as `|\ trials(n)`
  (default `FALSE`).

- addition_template:

  Character. An `sprintf` template used when `has_addition_term = TRUE`.
  Must contain three `%s` slots for response, addition variable, and
  RHS. Example: `"%s | trials(%s) ~ %s"`.

- response_check:

  Function `function(y)` returning `TRUE` when the response domain is
  valid, `FALSE` otherwise (default: accept anything).

- response_check_msg:

  Character. Error message displayed when `response_check(y)` returns
  `FALSE`.

- default_priors:

  Function `function(...)` returning a `brmsprior` object, or `NULL` to
  use brms defaults.

- aux_param_hyperprior:

  Optional function `function(args, data)` returning a list with
  components `prior` (a `brmsprior`) and `stanvars` (a
  [`stanvar`](https://paulbuerkner.com/brms/reference/stanvar.html)
  object). Used by distributions that have an auxiliary parameter
  (e.g.\\ \\\phi\\ for Beta, *shape* for Gamma) requiring a hyperprior
  expressed in raw Stan code. The `args` list contains family-specific
  user inputs forwarded through `aux_args` in
  [`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).
  Return `NULL` to skip injection for a given call.

- overwrite:

  Logical. Permit overwriting an existing key (default `FALSE`).

## Value

Invisibly returns the registered model spec (a named list).

## Details

After registering, you can fit a model directly with
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md):


    register_hbsae_model(
      key            = "gamma_log",
      family         = "Gamma",
      link           = "log",
      discrete       = FALSE,
      supports_mi    = TRUE,
      response_check = function(y) all(y > 0, na.rm = TRUE),
      response_check_msg = "Gamma response must be strictly positive."
    )

    fit <- hbm_flex(
      family_key = "gamma_log",
      response   = "expenditure",
      auxiliary  = c("x1", "x2"),
      data       = my_data
    )

## See also

[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md),
[`list_hbsae_models`](https://madsyair.github.io/hbsaems/reference/list_hbsae_models.md)
