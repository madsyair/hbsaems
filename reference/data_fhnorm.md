# Simulated Fay-Herriot Normal Data

A simulated dataset for 100 areas under the Fay-Herriot Normal
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

- `group`:

  Area identifier (1-100) for random-effect grouping.

- `sre`:

  Spatial-random-effect grouping variable.

## Source

Simulated.
