# Test Whether an Object Is an hbsaems Check Result

Predicate for the `"hbsaems_check"` base class. All pre-fit inspection
functions in hbsaems
([`check_data`](https://madsyair.github.io/hbsaems/reference/check_data.md),
[`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md),
[`check_shiny_deps`](https://madsyair.github.io/hbsaems/reference/check_shiny_deps.md))
return an object that inherits from this class, enabling generic
handling.

## Usage

``` r
is.hbsaems_check(x)
```

## Arguments

- x:

  Any R object.

## Value

A single logical.

## See also

[`check_data`](https://madsyair.github.io/hbsaems/reference/check_data.md),
[`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md),
[`check_shiny_deps`](https://madsyair.github.io/hbsaems/reference/check_shiny_deps.md)

## Examples

``` r
chk <- check_data(data.frame(y = 1:5, x = 1:5),
                  response = "y", predictors = "x")
is.hbsaems_check(chk)              # TRUE
#> [1] TRUE
is.hbsaems_check("not a check")    # FALSE
#> [1] FALSE
```
