# Compare Multiple Fitted HBMs

Ranks N models by LOO and/or WAIC, returning a sorted `hbm_table`.
Analogous to
[`loo_compare`](https://mc-stan.org/loo/reference/loo_compare.html).

## Usage

``` r
model_compare_all(..., criterion = c("loo", "waic", "both"))
```

## Arguments

- ...:

  Named `hbmfit` objects.

- criterion:

  One of `"loo"` (default), `"waic"`, or `"both"`.

## Value

A `hbm_table` (a sorted `data.frame`) with columns `Model`, `ELPD_LOO`,
`LOO_SE`, `LOO_rank` (and analogous `*_WAIC*` columns when requested).

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
FAST <- list(chains = 2, iter = 1000, warmup = 500, cores = 1,
             seed = 1, refresh = 0)
m1 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1),
                          data = data_fhnorm), FAST))
#> Warning: Model fitted without any area-level random effects.
#>   This is unusual for Small Area Estimation: the standard Fay-Herriot model assumes u_i ~ N(0, sigma_u^2) per area, so estimates from a purely fixed-effects model will not borrow strength across areas.
#>   Consider one of:
#>     re = ~ (1 | area_id)                                     # IID area RE
#>     spatial_var = 'area_id', spatial_model = 'car', M = W    # CAR spatial RE
#>     spatial_var = 'area_id', spatial_model = 'sar', M = W    # SAR spatial RE
#>   If a fixed-effects-only baseline is intentional, you can suppress this warning with `suppressWarnings()`.
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
m2 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1 + x2),
                          data = data_fhnorm), FAST))
#> Warning: Model fitted without any area-level random effects.
#>   This is unusual for Small Area Estimation: the standard Fay-Herriot model assumes u_i ~ N(0, sigma_u^2) per area, so estimates from a purely fixed-effects model will not borrow strength across areas.
#>   Consider one of:
#>     re = ~ (1 | area_id)                                     # IID area RE
#>     spatial_var = 'area_id', spatial_model = 'car', M = W    # CAR spatial RE
#>     spatial_var = 'area_id', spatial_model = 'sar', M = W    # SAR spatial RE
#>   If a fixed-effects-only baseline is intentional, you can suppress this warning with `suppressWarnings()`.
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
model_compare_all(simple = m1, medium = m2)
#> Error: object 'm1' not found
# }
```
