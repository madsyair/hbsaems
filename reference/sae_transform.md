# Apply a Transformation to SAE Predictions

Apply a Transformation to SAE Predictions

## Usage

``` r
sae_transform(x, fun, ...)
```

## Arguments

- x:

  An `hbsae_results` object.

- fun:

  A function applied element-wise to the predictions.

- ...:

  Additional arguments passed to `fun`.

## Value

A new `hbsae_results` object.

## Examples

``` r
p <- structure(list(result_table = data.frame(Prediction = c(2, 4, 8),
                                               RSE_percent = c(5, 5, 5)),
                     rse_model = 5, pred = c(2, 4, 8)),
                class = "hbsae_results")
sae_transform(p, log)
#> 
#> Small Area Estimates  [hbsae_results]
#> --------------------------------------
#>  Areas       : 3 
#>  Overall RSE : 5 %
#>  Pred. range : 0.693 to 2.079 
#> 
```
