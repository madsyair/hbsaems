# Simulated Beta Logit-Normal Data

A simulated dataset for 100 areas under a Beta logit-normal model. The
response `y` is a proportion in \\(0, 1)\\.

## Usage

``` r
data_betalogitnorm
```

## Format

A data frame with 100 rows and 9 variables:

- `y`:

  Direct estimator of the area-level proportion.

- `theta`:

  True area-level proportion.

- `x1`, `x2`, `x3`:

  Auxiliary covariates.

- `n`:

  Area sample size.

- `deff`:

  Design effect.

- `group`:

  Area identifier (1-100).

- `sre`:

  Spatial-random-effect grouping variable.

## Source

Simulated.
