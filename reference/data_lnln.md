# Simulated Lognormal-Lognormal Data

A simulated dataset for 100 districts under a Lognormal-Lognormal model.
Suitable for strictly positive, right-skewed outcomes.

## Usage

``` r
data_lnln
```

## Format

A data frame with 100 rows and 13 variables:

- `district`:

  District identifier (`"district_001"` .. `"district_100"`) – the 100
  small areas to estimate.

- `x1`, `x2`, `x3`:

  Auxiliary covariates.

- `u_true`:

  True area-level random effect (log scale).

- `teta_true`:

  True linear predictor (log scale).

- `mu_orig_true`:

  True mean on the original scale.

- `n`:

  Area sample size.

- `y_obs`:

  Observed direct estimator.

- `lambda_dir`:

  Direct estimator scale parameter.

- `y_log_obs`:

  Observed value on the log scale.

- `psi_i`:

  Sampling variance on the log scale.

- `regency`:

  Regency identifier (`"regency_01"` .. `"regency_05"`) – coarse
  spatial-cluster level (5 regencies each containing 20 districts). Pair
  with `adjacency_matrix_car_regency`.

## Source

Simulated.
