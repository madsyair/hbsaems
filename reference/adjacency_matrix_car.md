# Province-level Adjacency Matrix

A small example adjacency matrix used for fitting Conditional
Autoregressive (CAR) random effects on the \*\*province\*\* level. Pairs
with `data_fhnorm` and `data_betalogitnorm`, whose `province` column has
matching labels.

## Usage

``` r
adjacency_matrix_car
```

## Format

A binary symmetric \\5 \times 5\\ matrix with row- and column-names
`province_01` .. `province_05`; entries are `1` for adjacent province
pairs and `0` otherwise.

## Source

Simulated.
