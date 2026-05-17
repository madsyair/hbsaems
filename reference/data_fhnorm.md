# Simulated Fay-Herriot Normal Data

A simulated dataset for 100 regencies under the Fay-Herriot Normal
small-area model. Used as the running example throughout the package
documentation and vignettes.

## Usage

``` r
data_fhnorm
```

## Format

A data frame with 100 rows and 9 variables:

- `y`:

  Direct (survey) estimator of the area mean.

- `D`:

  Sampling variance of the direct estimator.

- `x1`, `x2`, `x3`:

  Auxiliary covariates at the area level.

- `theta_true`:

  True area-level latent value.

- `u`:

  True area-level random effect.

- `regency`:

  Regency identifier (`"regency_001"` .. `"regency_100"`) used as the
  IID random-effect grouping variable. Use with `re = ~ (1 | regency)`
  or as `area_var = "regency"` in the wrapper functions.

- `province`:

  Province identifier (`"province_01"` .. `"province_05"`) used as the
  spatial random-effect grouping variable for CAR or SAR models. Use
  with `spatial_var = "province"` and `M = adjacency_matrix_car`.

## Source

Simulated.
