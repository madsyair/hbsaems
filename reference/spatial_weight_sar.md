# Spatial Weight Matrix for Simultaneous Autoregressive Models

A row-standardised spatial weight matrix for 100 regencies, used for
fitting Simultaneous Autoregressive (SAR) random effects. Pairs with
`data_fhnorm`'s `regency` column.

## Usage

``` r
spatial_weight_sar
```

## Format

A \\100 \times 100\\ numeric matrix with row- and column-names
`regency_001` .. `regency_100` and row sums equal to one (when at least
one neighbour is present).

## Source

Simulated.
