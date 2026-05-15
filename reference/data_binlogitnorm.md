# Simulated Binomial Logit-Normal Data

A simulated dataset for 100 areas under a Binomial logit-normal model.
Each area provides a number of successes (`y`) out of `n` trials.

## Usage

``` r
data_binlogitnorm
```

## Format

A data frame with 100 rows and 14 variables:

- `n`:

  Number of trials in the area.

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

- `group`:

  Area identifier (1-100).

- `sre`:

  Spatial-random-effect grouping variable.

## Source

Simulated.
