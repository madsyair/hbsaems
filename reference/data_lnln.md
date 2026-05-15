# Simulated Lognormal-Lognormal Data

A simulated dataset for 100 areas under a Lognormal-Lognormal model.
Suitable for strictly positive, right-skewed outcomes.

## Usage

``` r
data_lnln
```

## Format

A data frame with 100 rows and 13 variables:

- `group`:

  Area identifier (1-100).

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

- `sre`:

  Spatial-random-effect grouping variable.

## Source

Simulated.
