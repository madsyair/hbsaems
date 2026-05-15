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
  sampling_var = NULL,
  fixed_params = NULL,
  predictors = NULL,
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

- sampling_var:

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
  by `sampling_var`.

- predictors:

  **Deprecated.** Use `auxiliary` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- ...:

  Additional arguments forwarded to
  [`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  (e.g. `group`, `sre`, `prior_type`, `nonlinear`, `handle_missing`,
  sampler controls).

## Value

An object of class `hbmfit`.

## Details

The response \\y_i\\ in area \\i\\ is assumed to follow \$\$y_i \mid
\theta_i \sim \mathrm{Lognormal}(\theta_i, \sigma^2),\$\$ with the
log-mean linked to auxiliary variables via \$\$\log(\theta_i) = x_i^\top
\boldsymbol{\beta} + u_i, \quad u_i \sim \mathcal{N}(0, \sigma_v^2).\$\$

When the user supplies `sampling_var = "psi_i"` (the column name of the
known per-area sampling variance \\\psi_i\\), \\\sigma_i =
\sqrt{\psi_i}\\ is pinned for each area via an offset. This recovers the
Fay-Herriot-style lognormal model in which residual variability is fully
determined by the survey design.

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
  group      = "group",
  data       = data_lnln,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')

# -- 2. Fay-Herriot style with known sampling variance ----------------------
#     (assumes psi_i column is available)
fit2 <- hbm_lnln(
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  group        = "group",
  sampling_var = "psi_i",
  data         = data_lnln,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```
