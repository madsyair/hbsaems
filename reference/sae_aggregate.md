# Aggregate Predictions from Multiple hbsae_results

Combines area-level predictions across multiple `hbsae_results` objects.
All objects must report predictions for the same number of areas (in the
same order).

## Usage

``` r
sae_aggregate(..., method = c("mean", "median", "weighted"), weights = NULL)
```

## Arguments

- ...:

  Two or more `hbsae_results` objects.

- method:

  One of `"mean"` (default), `"median"`, or `"weighted"`.

- weights:

  Numeric vector of weights, required when `method = "weighted"`.
  Internally normalised to sum to 1.

## Value

An `hbsae_results` object containing the combined predictions.

## Examples

``` r
p1  <- structure(list(result_table = data.frame(Prediction = 1:3,
                                                 RSE_percent = c(5, 5, 5)),
                       rse_model = 5, pred = 1:3),
                  class = "hbsae_results")
p2  <- structure(list(result_table = data.frame(Prediction = 2:4,
                                                 RSE_percent = c(4, 4, 4)),
                       rse_model = 4, pred = 2:4),
                  class = "hbsae_results")
sae_aggregate(p1, p2, method = "mean")
#> 
#> Small Area Estimates  [hbsae_results]
#> --------------------------------------
#>  Areas       : 3 
#>  Overall RSE : 4.5 %
#>  Pred. range : 1.5 to 3.5 
#> 
sae_aggregate(p1, p2, method = "weighted", weights = c(0.6, 0.4))
#> 
#> Small Area Estimates  [hbsae_results]
#> --------------------------------------
#>  Areas       : 3 
#>  Overall RSE : 4.5 %
#>  Pred. range : 1.4 to 3.4 
#> 
```
