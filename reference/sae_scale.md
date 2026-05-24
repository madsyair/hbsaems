# Standardise SAE Predictions

Standardise SAE Predictions

## Usage

``` r
sae_scale(x, center = TRUE, scale = TRUE)
```

## Arguments

- x:

  An `hbsae_results` object.

- center:

  Logical or numeric centering (passed to
  [`base::scale`](https://rdrr.io/r/base/scale.html)).

- scale:

  Logical or numeric scaling (passed to
  [`base::scale`](https://rdrr.io/r/base/scale.html)).

## Value

A new `hbsae_results` object with standardised predictions.

## Examples

``` r
p <- structure(list(result_table = data.frame(Prediction = 1:5,
                                               RSE_percent = rep(5, 5)),
                     rse_model = 5, pred = 1:5),
                class = "hbsae_results")
sae_scale(p)
#> 
#> Small Area Estimates  [hbsae_results]
#> --------------------------------------
#>  Areas       : 5 
#>  Overall RSE : 5 % 
#>  Pred. range : -1.265 to 1.265 
#> 
```
