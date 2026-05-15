# Test Whether an Object Belongs to an hbsaems Result Class

Lightweight type-checking predicates for all five result classes
produced by hbsaems. Each predicate returns a single logical.

## Usage

``` r
is.hbmfit(x)

is.hbcc_results(x)

is.hbmc_results(x)

is.hbpc_results(x)

is.hbsae_results(x)
```

## Arguments

- x:

  Any R object.

## Value

A single logical value.

## Examples

``` r
is.hbmfit("not a model")   # FALSE
#> [1] FALSE
```
