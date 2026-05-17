# Prior Configuration for HBSAE Models

Bundles the shrinkage-prior arguments of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) into a
single named list. Same opt-in pattern as
[`hbm_control`](https://madsyair.github.io/hbsaems/reference/hbm_control.md).

## Usage

``` r
hbm_priors(
  prior_type = c("default", "horseshoe", "r2d2"),
  prior = NULL,
  hs_df = 1,
  hs_df_global = 1,
  hs_df_slab = 4,
  hs_scale_global = NULL,
  hs_scale_slab = 2,
  hs_par_ratio = NULL,
  hs_autoscale = TRUE,
  r2d2_mean_R2 = 0.5,
  r2d2_prec_R2 = 2,
  r2d2_cons_D2 = NULL,
  r2d2_autoscale = TRUE
)
```

## Arguments

- prior_type:

  Character. One of `"default"`, `"horseshoe"`, `"r2d2"`.

- prior:

  Optional `brmsprior` for explicit priors that override the registry
  default.

- hs_df, hs_df_global, hs_df_slab, hs_scale_global, hs_scale_slab,
  hs_par_ratio, hs_autoscale:

  Horseshoe-prior hyperparameters; see
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

- r2d2_mean_R2, r2d2_prec_R2, r2d2_cons_D2, r2d2_autoscale:

  R2D2-prior hyperparameters; see
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

## Value

A named list of arguments for
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

## See also

[`hbm_control`](https://madsyair.github.io/hbsaems/reference/hbm_control.md),
[`hbm_nonlinear`](https://madsyair.github.io/hbsaems/reference/hbm_nonlinear.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
p_hs   <- hbm_priors(prior_type = "horseshoe", hs_df = 1, hs_df_slab = 4)
p_r2d2 <- hbm_priors(prior_type = "r2d2",      r2d2_mean_R2 = 0.5)
```
