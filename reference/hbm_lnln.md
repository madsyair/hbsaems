# Small Area Estimation under a Lognormal-Lognormal Model

Convenience wrapper that fits a Hierarchical Bayesian Small Area
Estimation model with a **lognormal** likelihood. Internally delegates
to
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
with `family_key = "lognormal"`.

## Usage

``` r
hbm_lnln(
  response,
  auxiliary = NULL,
  data,
  area_var = NULL,
  area_re_structure = c("nested", "crossed"),
  spatial_var = NULL,
  spatial_model = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  sampling_variance = NULL,
  fixed_params = NULL,
  predictors = NULL,
  sampling_var = NULL,
  group = NULL,
  sre = NULL,
  sre_type = NULL,
  ...
)
```

## Arguments

- response:

  Character. Name of the response column (must be strictly positive).

- auxiliary:

  Character vector of auxiliary (fixed-effect) variable names;
  corresponds to area-level covariates in SAE literature (see Rao &
  Molina 2015 Ch. 4).

- data:

  A `data.frame`.

- area_var:

  Optional character vector. Name(s) of a column (or columns) in `data`
  identifying the small area / domain. Length 1 adds an IID area-level
  random intercept `(1 | area_var)`; length \\\geq\\ 2 supports
  hierarchical areas – see
  [`?hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  for the nested vs.\\ crossed structures. Default: `NULL`.

- area_re_structure:

  Either `"nested"` (default) or `"crossed"`. Controls how multiple area
  columns combine. See
  [`?hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).

- spatial_var:

  Optional character. Name of a column in `data` identifying the spatial
  cluster (e.g. province). Must be supplied together with
  `spatial_model` and `M`. Default: `NULL`.

- spatial_model:

  Optional character. Type of spatial dependence: `"car"` (conditional
  autoregressive) or `"sar"` (simultaneous autoregressive). Default:
  `NULL`.

- car_type:

  Optional character. CAR sub-type passed to brms: `"escar"`,
  `"esicar"`, `"icar"` (intrinsic CAR; default when
  `spatial_model = "car"`), or `"bym2"`.

- sar_type:

  Optional character. SAR sub-type: `"lag"` (spatial-lag, default when
  `spatial_model = "sar"`) or `"error"` (spatial-error).

- M:

  Optional numeric matrix. Spatial weight matrix. Required when
  `spatial_model` is supplied.

- sampling_variance:

  Optional character. Name of a column in `data` containing the known
  per-area sampling variance \\\psi_i\\ on the **log scale**, i.e.\\ the
  variance of \\\log(\hat y_i)\\. When supplied, \\\sigma_i =
  \sqrt{\psi_i}\\ is pinned via offset, recovering the Fay–Herriot
  lognormal model. If your survey software produces \\\mathrm{Var}(\hat
  y_i)\\ on the original scale, convert with the delta-method
  approximation \\\psi_i \approx \mathrm{Var}(\hat y_i) / \hat y_i^2\\
  before passing. Default: `NULL` (sigma is random).

- fixed_params:

  Optional named list pinning distributional parameters to known values.
  See [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  the spec format. Allows power-user access to the same machinery used
  by `sampling_variance`.

- predictors:

  **Deprecated.** Use `auxiliary` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- sampling_var:

  **Deprecated.** Use `sampling_variance` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- group:

  **Deprecated.** Use `area_var` instead.

- sre:

  **Deprecated.** Use `spatial_var` instead.

- sre_type:

  **Deprecated.** Use `spatial_model` instead.

- ...:

  Additional arguments forwarded to
  [`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  (e.g.\\ `prior_type`, `nonlinear`, `handle_missing`, sampler controls
  such as `chains`, `iter`, `cores`, `seed`).

## Value

An object of class `hbmfit`.

## Details

The response \\y_i\\ in area \\i\\ is assumed to follow \$\$y_i \mid
\theta_i \sim \mathrm{Lognormal}(\theta_i, \sigma^2),\$\$ with the
log-mean linked to auxiliary variables via \$\$\log(\theta_i) = x_i^\top
\boldsymbol{\beta} + u_i, \quad u_i \sim \mathcal{N}(0, \sigma_v^2).\$\$

When the user supplies `sampling_variance = "psi_i"` (the column name of
the known per-area sampling variance \\\psi_i\\), \\\sigma_i =
\sqrt{\psi_i}\\ is pinned for each area via an offset. This recovers the
Fay-Herriot-style lognormal model in which residual variability is fully
determined by the survey design.

## Conflict policy

When the residual standard deviation \\\sigma_i = \sqrt{\psi_i}\\ is
pinned via `sampling_variance` (or via `fixed_params$sigma`), the
function refuses any additional specification that would also set
\\\sigma\\. Specifically, all of the following are rejected with an
informative error at construction time:

- `sampling_variance` *and* `fixed_params$sigma`.

- `sampling_variance` *and* a user `prior` on `class = "sigma"`.

- `sampling_variance` *and* a `stanvars` sampling statement involving
  `sigma`.

- `auxiliary` *and* the deprecated `predictors` in the same call.

## See also

[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# \donttest{
library(hbsaems)
data("data_lnln")

# -- 1. Standard lognormal SAE with area random effect ----------------------
fit1 <- hbm_lnln(
  response   = "y_obs",
  auxiliary  = c("x1", "x2", "x3"),
  area_var   = "district",
  data       = data_lnln,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Warning: Area column 'district' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')

# -- 2. Fay-Herriot style with known sampling variance ----------------------
#     (assumes psi_i column is available)
fit2 <- hbm_lnln(
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  area_var     = "district",
  sampling_variance = "psi_i",
  data         = data_lnln,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Warning: Area column 'district' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```
