# Simulated Beta Logit-Normal Data

A simulated dataset for 100 regencies under a Beta logit-normal model.
The response `y` is a proportion in \\(0, 1)\\.

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

- `regency`:

  Regency identifier (`"regency_001"` .. `"regency_100"`).

- `province`:

  Province identifier (`"province_01"` .. `"province_05"`) – spatial
  cluster level for CAR / SAR.

## Source

Simulated.
