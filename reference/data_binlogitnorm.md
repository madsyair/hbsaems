# Simulated Binomial Logit-Normal Data

A simulated dataset for 100 districts under a Binomial logit-normal
model. Each district provides a number of successes (`y`) out of `n`
trials.

## Usage

``` r
data_binlogitnorm
```

## Format

A data frame with 100 rows and 14 variables:

- `n`:

  Number of trials in the district.

- `y`:

  Number of successes.

- `p`:

  Direct proportion (`y / n`).

- `x1`, `x2`, `x3`:

  Auxiliary covariates.

- `u_true`:

  True area-level random effect (logit scale).

- `eta_true`:

  True linear predictor (logit scale).

- `p_true`:

  True success probability.

- `psi_i`:

  Sampling variance.

- `y_obs`, `p_obs`:

  Observed (direct) values.

- `district`:

  District identifier (`"district_001"` .. `"district_100"`) – the 100
  small areas to estimate.

- `regency`:

  Regency identifier (`"regency_01"` .. `"regency_05"`) – coarse
  spatial-cluster level (5 regencies each containing 20 districts). Pair
  with `adjacency_matrix_car_regency`.

## Source

Simulated.
