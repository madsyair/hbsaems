# Bayesian Model Averaging on Small-Area Estimates

Averages the area-level predictions across multiple fitted HBMs. Weights
may be supplied manually *or* computed automatically from leave-one-out
cross-validation via
[`loo_model_weights`](https://mc-stan.org/loo/reference/loo_model_weights.html)
– the canonical Bayesian stacking / pseudo-BMA approach of Yao et al.\\
(2018).

## Usage

``` r
model_average(
  ...,
  weights = NULL,
  method = c("manual", "stacking", "pseudobma"),
  predict_type = c("epred", "response", "linpred", "proportion"),
  newdata = NULL
)
```

## Arguments

- ...:

  Two or more `hbmfit` objects.

- weights:

  Numeric weights of the same length as the number of models, or `NULL`.
  When `NULL` and `method = "manual"`, equal weights are used.

- method:

  Character. Weighting method: `"manual"` (default), `"stacking"` (Yao
  et al. 2018), or `"pseudobma"`. When `"stacking"` or `"pseudobma"`,
  `weights` must be `NULL`; an error is raised otherwise.

- predict_type:

  Passed to
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  for each model: one of `"epred"` (default), `"response"`, `"linpred"`,
  or `"proportion"`.

- newdata:

  Optional new `data.frame` forwarded to
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md).

## Value

An `hbsae_results` object of averaged predictions. The computed weights
are attached as an attribute `"weights"`.

## Details

Three weighting modes are supported:

- `method = "manual"` (default when `weights` is supplied): use the
  user-supplied `weights` vector directly. Internally normalised to sum
  to 1.

- `method = "stacking"`: weights are obtained from
  `loo::loo_model_weights(loo_list, method = "stacking")`, which
  optimises a log-score over a simplex. Recommended when models are
  well-specified but capture different features of the data (Yao et
  al.\\ 2018).

- `method = "pseudobma"`: weights are obtained from
  `loo::loo_model_weights(loo_list, method = "pseudobma")`, a smoothed
  pseudo-BMA+ that resembles classical BMA but uses PSIS-LOO log scores.
  Use as a robust default when one model is much better than the others.

Internally calls
[`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
on each model and then
[`sae_aggregate`](https://madsyair.github.io/hbsaems/reference/sae_aggregate.md)
with `method = "weighted"`.

## References

Yao, Y., Vehtari, A., Simpson, D., & Gelman, A. (2018). Using stacking
to average Bayesian predictive distributions (with discussion).
*Bayesian Analysis*, 13(3), 917–1007.
[doi:10.1214/17-BA1091](https://doi.org/10.1214/17-BA1091)

Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27(5), 1413–1432.

## Examples

``` r
# \donttest{
# See ?model_compare_all for the full example fitting m1 / m2.
# Manual:    avg <- model_average(m1, m2, weights = c(0.6, 0.4))
# Stacking:  avg <- model_average(m1, m2, method = "stacking")
# Pseudo-BMA: avg <- model_average(m1, m2, method = "pseudobma")
# }
```
