# Regency-level Adjacency Matrix

A small example adjacency matrix used for fitting Conditional
Autoregressive (CAR) random effects on the \*\*regency\*\* level. Pairs
with `data_binlogitnorm` and `data_lnln`, whose `regency` column has
matching labels.

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
