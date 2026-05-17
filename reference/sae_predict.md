# Generate Small Area Estimates

Primary SAE prediction function in hbsaems (supersedes deprecated
[`hbsae`](https://madsyair.github.io/hbsaems/reference/deprecated.md)).
Computes area-level posterior predictive means, standard deviations, and
relative standard errors (RSE).

## Usage

``` r
sae_predict(model, newdata = NULL, ...)
```

## Arguments

- model:

  An `hbmfit` or `brmsfit` object.

- newdata:

  Optional new `data.frame` for prediction at unsampled areas. If `NULL`
  (default), the original data are used.

- ...:

  Additional arguments forwarded to
  [`posterior_predict`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
  (e.g.\\ `ndraws`, `re_formula`).

## Value

An `hbsae_results` object with components:

- `result_table`:

  A `data.frame` with columns `Prediction`, `SD`, `RSE_percent`.

- `rse_model`:

  Mean of `RSE_percent` across all areas.

- `pred`:

  Numeric vector of point predictions (= `result_table$Prediction`).

## Details

For each area \\i = 1, \ldots, n\\, the function computes
\$\$\widehat{y}\_i = \frac{1}{S} \sum\_{s=1}^{S} y\_{i}^{(s)}, \qquad
\widehat{\mathrm{sd}}\_i^2 = \frac{1}{S - 1} \sum\_{s=1}^{S} \left(
y\_{i}^{(s)} - \widehat{y}\_i \right)^2,\$\$ where \\y\_{i}^{(s)}\\ are
draws from the posterior predictive distribution and \\S\\ is the number
of draws. The relative standard error is \\\mathrm{RSE}\_i = 100 \cdot
\|\widehat{\mathrm{sd}}\_i / \widehat{y}\_i\|\\.

## See also

[`sae_aggregate`](https://madsyair.github.io/hbsaems/reference/sae_aggregate.md),
[`model_average`](https://madsyair.github.io/hbsaems/reference/model_average.md),
[`sae_transform`](https://madsyair.github.io/hbsaems/reference/sae_transform.md),
[`sae_scale`](https://madsyair.github.io/hbsaems/reference/sae_scale.md),
[`sae_filter`](https://madsyair.github.io/hbsaems/reference/sae_filter.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(
  formula = brms::bf(y ~ x1 + x2 + x3),
  data    = data_fhnorm,
  chains = 2, iter = 2000, warmup = 1000, cores = 1,
  seed = 123, refresh = 0
)
#> Warning: Model fitted without any area-level random effects.
#>   This is unusual for Small Area Estimation: the standard Fay-Herriot model assumes u_i ~ N(0, sigma_u^2) per area, so estimates from a purely fixed-effects model will not borrow strength across areas.
#>   Consider one of:
#>     re = ~ (1 | area_id)                                     # IID area RE
#>     spatial_var = 'area_id', spatial_model = 'car', M = W    # CAR spatial RE
#>     spatial_var = 'area_id', spatial_model = 'sar', M = W    # SAR spatial RE
#>   If a fixed-effects-only baseline is intentional, you can suppress this warning with `suppressWarnings()`.
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
est <- sae_predict(model)
#> Error: object 'model' not found
summary(est)
#> Error: object 'est' not found
plot(est, type = "predictions")
#> Error: object 'est' not found
plot(est, type = "uncertainty")
#> Error: object 'est' not found
# }
```
