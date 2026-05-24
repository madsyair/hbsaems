# Regency-level Adjacency Matrix (Coarse Spatial Cluster)

A small example adjacency matrix used for fitting Conditional
Autoregressive (CAR) random effects on the \*\*regency\*\* (coarse
spatial-cluster, kabupaten) level. Pairs with `data_binlogitnorm` and
`data_lnln`, whose `regency` column has matching labels.

## Usage

``` r
adjacency_matrix_car_regency
```

## Format

A binary symmetric \\5 \times 5\\ matrix with row- and column-names
`regency_01` .. `regency_05`; entries are `1` for adjacent regency pairs
and `0` otherwise.

## Source

Simulated.

## Note

The naming `regency_01..05` (two-digit suffix) is reserved in this
package for the COARSE 5-level cluster used by `data_binlogitnorm` and
`data_lnln`. The 100-level FINE regency column in `data_fhnorm` and
`data_betalogitnorm` uses three-digit suffixes (`regency_001..100`) and
pairs with the larger `spatial_weight_sar` matrix. See
`vignette("hbsaems-spatial")` for the naming convention.
