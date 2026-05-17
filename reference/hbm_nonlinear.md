# Nonlinear-Term Configuration for HBSAE Models

Bundles the nonlinear-term arguments of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) into a
single named list. Same opt-in pattern as
[`hbm_control`](https://madsyair.github.io/hbsaems/reference/hbm_control.md).

## Usage

``` r
hbm_nonlinear(
  terms,
  type = c("spline", "gp"),
  k = -1L,
  spline_bs = "tp",
  gp_cov = "exp_quad",
  gp_c = NULL,
  gp_scale = NULL
)
```

## Arguments

- terms:

  Character vector of predictor names to be wrapped in
  [`s()`](https://paulbuerkner.com/brms/reference/s.html) or
  [`gp()`](https://paulbuerkner.com/brms/reference/gp.html).

- type:

  Character. `"spline"` (default) or `"gp"`.

- k:

  Integer. Basis dimension. For splines: passed to
  `mgcv::s(..., k = ...)`; `-1L` (default) lets mgcv choose. For GP:
  passed to `brms::gp(..., k = ...)` for the Hilbert-space approximate
  GP (Riutort-Mayol et al.\\ 2020); `NA` = exact GP (slow, not
  recommended for \\n \> 100\\).

- spline_bs:

  Character. Spline basis type (`"tp"`, `"cr"`, `"cs"`, `"ps"`, ...).
  Default `"tp"`.

- gp_cov:

  Character. GP covariance function: `"exp_quad"`, `"matern15"`,
  `"matern25"`, `"exponential"`. Default `"exp_quad"`.

- gp_c:

  Numeric or NULL. HSGP boundary-scale factor for `brms::gp(c = ...)`.

- gp_scale:

  **Deprecated.** Use `gp_c` instead.

## Value

A named list of arguments for
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md), with class
`c("hbm_config_nonlinear", "hbm_config", "list")`.

## See also

[`hbm_control`](https://madsyair.github.io/hbsaems/reference/hbm_control.md),
[`hbm_priors`](https://madsyair.github.io/hbsaems/reference/hbm_priors.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# Penalised regression spline (auto-chosen basis dimension):
nl_spline <- hbm_nonlinear(c("x1", "x3"), type = "spline")

# Spline with cubic-regression basis and fixed k:
nl_cr <- hbm_nonlinear("x1", type = "spline", k = 8, spline_bs = "cr")

# Hilbert-space approximate GP with Matern 5/2 covariance (recommended):
nl_gp <- hbm_nonlinear("x2", type = "gp", k = 20, gp_cov = "matern25")
```
