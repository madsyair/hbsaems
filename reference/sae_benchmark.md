# Benchmark Small-Area Estimates to Known Totals

Adjusts area-level predictions so that their weighted sum matches one or
more known aggregate “benchmark” totals (e.g.\\ official provincial or
national figures). Two modes are supported:

## Usage

``` r
sae_benchmark(
  predictions,
  target,
  weights = NULL,
  method = c("ratio", "difference", "raking"),
  groups = NULL,
  posterior = NULL,
  probs = c(0.025, 0.5, 0.975),
  max_iter = 100L,
  tol = 1e-08
)
```

## Arguments

- predictions:

  An `hbsae_results` object produced by
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md).

- target:

  Numeric. For `method = "ratio"` or `"difference"`: a single benchmark
  total \\T\\. For `method = "raking"`: a numeric vector of group totals
  (one per group; see `groups`).

- weights:

  Optional numeric vector of length equal to the number of areas in
  `predictions`. Typical choices are population sizes or sampling
  weights. Defaults to equal weights (`1 / n`).

- method:

  Character. One of:

  `"ratio"`

  :   Multiplicative: \\\hat{\theta}\_i^B = \hat{\theta}\_i \times T /
      \sum_j w_j \hat{\theta}\_j\\.

  `"difference"`

  :   Additive: \\\hat{\theta}\_i^B = \hat{\theta}\_i + (T - \sum_j w_j
      \hat{\theta}\_j) / \sum_j w_j\\.

  `"raking"`

  :   Iterative proportional fitting to multiple group totals. Requires
      `groups`.

- groups:

  Optional integer/character vector of length equal to the number of
  areas, assigning each area to a benchmarking group. Required for
  `method = "raking"`; ignored otherwise.

- posterior:

  Optional logical or matrix. Controls Bayesian mode:

  `NULL` or `FALSE` (default)

  :   Point-estimate mode – same behaviour as v1.0.0.

  `TRUE`

  :   Bayesian mode. Posterior draws are extracted from `predictions`
      automatically (requires `predictions$model` to be available).

  a numeric *matrix* (\\D \times n\\)

  :   Bayesian mode with user-supplied draws (\\D\\ draws, \\n\\ areas).

- probs:

  Numeric vector of quantile probabilities to summarise the adjusted
  posterior with (default `c(0.025, 0.5, 0.975)`). Only used in Bayesian
  mode.

- max_iter:

  Integer. Maximum iterations for raking (default `100L`).

- tol:

  Numeric. Convergence tolerance for raking (default `1e-8`).

## Value

An `hbsae_results` object with benchmarked `Prediction` values, plus an
additional element `$benchmark_info` that records `method`, `target`,
`weights`, the implied `adjustment` factor, and `converged` (logical,
raking only). In Bayesian mode the `result_table` also contains updated
`SD` and `RSE_percent` columns reflecting the post-benchmark posterior
uncertainty, plus quantile columns named after `probs`.

## Details

- **Point-estimate mode (default)**:

  Applies the adjustment only to the posterior mean. The RSE column is
  left unchanged as a working approximation.

- **Fully Bayesian mode** (`posterior = TRUE` or supply a `posterior`
  matrix):

  Applies the adjustment to every posterior draw and recomputes SD,
  quantiles, and RSE from the adjusted draws. This is the statistically
  correct procedure and produces proper uncertainty intervals after
  benchmarking.

### When to benchmark

Benchmarking is widely used in official statistics to ensure consistency
between model-based small-area estimates and published aggregate totals
from a more reliable source.

### Why fully Bayesian benchmarking matters

Applying a deterministic adjustment factor to a posterior point estimate
distorts the uncertainty structure: a multiplicative factor scales both
the mean and the SD by the same amount (so the RSE percentage is
preserved), but an additive shift does *not* change the SD, so the RSE
percentage shrinks at small areas and grows at large ones. Neither of
these post-hoc fixes is guaranteed to be correct in general. In Bayesian
mode every draw is benchmarked independently, so the resulting posterior
carries the right uncertainty.

## References

Pfeffermann, D. (2013). New important developments in small area
estimation. *Statistical Science* 28(1), 40–68.

Wang, J., Fuller, W. A., & Qu, Y. (2008). Small area estimation under a
restriction. *Survey Methodology* 34, 29–36.

## See also

[`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md),
[`sae_aggregate`](https://madsyair.github.io/hbsaems/reference/sae_aggregate.md),
[`model_average`](https://madsyair.github.io/hbsaems/reference/model_average.md)

## Examples

``` r
# Synthetic predictions (point-estimate mode)
p <- structure(
  list(
    result_table = data.frame(Prediction  = c(10, 12, 9, 11),
                               SD          = c(1, 1, 1, 1),
                               RSE_percent = c(10, 8, 11, 9)),
    rse_model    = 9.5,
    pred         = c(10, 12, 9, 11)
  ),
  class = "hbsae_results"
)
bm1 <- sae_benchmark(p, target = 50, method = "ratio")

# Fully Bayesian mode with user-supplied draws
set.seed(1)
D <- 1000
draws <- matrix(rnorm(D * 4, mean = c(10, 12, 9, 11), sd = 1),
                 nrow = D, byrow = TRUE)
bm2 <- sae_benchmark(p, target = 50, method = "ratio",
                      posterior = draws)
bm2$result_table              # SD, RSE updated from draws
#>   Prediction       SD RSE_percent     Q2.5      Q50    Q97.5
#> 1   47.61905 4.300884    9.031856 38.54522 47.34989 56.05428
#> 2   57.14286 4.312359    7.546629 48.62104 57.17462 65.55330
#> 3   42.85714 4.399850   10.266317 34.35734 42.75895 51.93634
#> 4   52.38095 4.229116    8.073767 43.66985 52.28960 60.65092
```
