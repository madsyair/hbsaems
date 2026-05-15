# Nonlinear-Term Configuration for HBSAE Models

Bundles the nonlinear-term arguments of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) into a
single named list. Same opt-in pattern as
[`hbm_control`](https://madsyair.github.io/hbsaems/reference/hbm_control.md).

## Usage

``` r
hbm_nonlinear(terms, type = c("spline", "gp"), k = -1L, gp_scale = NULL)
```

## Arguments

- terms:

  Character vector of predictor names to be wrapped in
  [`s()`](https://paulbuerkner.com/brms/reference/s.html) or
  [`gp()`](https://paulbuerkner.com/brms/reference/gp.html).

- type:

  Character. `"spline"` (default) or `"gp"`.

- k:

  Integer. Spline basis dimension. Default `-1L` (let mgcv choose).

- gp_scale:

  Optional numeric. GP length-scale (`c` argument of
  [`gp`](https://paulbuerkner.com/brms/reference/gp.html)).

## Value

A named list of arguments for
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md), mapping to
`nonlinear`, `nonlinear_type`, `spline_k`, `gp_scale`.

## See also

[`hbm_control`](https://madsyair.github.io/hbsaems/reference/hbm_control.md),
[`hbm_priors`](https://madsyair.github.io/hbsaems/reference/hbm_priors.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
nl_spline <- hbm_nonlinear(c("x1", "x3"), type = "spline", k = 5)
nl_gp     <- hbm_nonlinear(c("x2"),       type = "gp",     gp_scale = 1.5)
```
