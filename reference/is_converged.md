# Test Whether a Fitted HBM Has Converged

Returns a single `TRUE` / `FALSE` based on the Gelman-Rubin statistic.

## Usage

``` r
is_converged(model, threshold = 1.1, ...)
```

## Arguments

- model:

  An `hbmfit` or `hbcc_results` object.

- threshold:

  \\\widehat{R}\\ threshold (default `1.1`; use `1.05` for a stricter
  check, as recommended by Vehtari et al. 2021).

- ...:

  Currently unused.

## Value

A single logical.
