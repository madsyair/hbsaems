# Filter SAE Predictions by a Logical Condition

Filter SAE Predictions by a Logical Condition

## Usage

``` r
sae_filter(x, condition)
```

## Arguments

- x:

  An `hbsae_results` object.

- condition:

  Logical vector of length equal to the number of areas.

## Value

A new `hbsae_results` object containing only rows where `condition` is
`TRUE`.

## Examples

``` r
p <- structure(list(result_table = data.frame(Prediction = 1:5,
                                               RSE_percent = rep(5, 5)),
                     rse_model = 5, pred = 1:5),
                class = "hbsae_results")
sae_filter(p, p$pred > 2)
#> 
#> Small Area Estimates  [hbsae_results]
#> --------------------------------------
#>  Areas       : 3 
#>  Overall RSE : 5 % 
#>  Pred. range : 3 to 5 
#> 
```
