# Bayesian Model Averaging on Small-Area Estimates

Averages the area-level predictions across multiple fitted HBMs using a
user-supplied weight vector. Internally calls
[`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
on each model and then
[`sae_aggregate`](https://madsyair.github.io/hbsaems/reference/sae_aggregate.md)
with `method = "weighted"`.

## Usage

``` r
model_average(..., weights = NULL, newdata = NULL)
```

## Arguments

- ...:

  Two or more `hbmfit` objects.

- weights:

  Numeric weights of the same length as the number of models. `NULL`
  (default) gives equal weights. Internally normalised to sum to 1.

- newdata:

  Optional new `data.frame` forwarded to
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md).

## Value

An `hbsae_results` object of averaged predictions.

## Examples

``` r
# \donttest{
# See ?model_compare_all for the full example fitting m1 / m2.
# avg <- model_average(m1, m2, weights = c(0.6, 0.4))
# }
```
