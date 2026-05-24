# Compare One or Two Fitted HBMs

Computes LOO, WAIC, and posterior predictive check diagnostics. With a
single model this gives goodness-of-fit metrics; with two models it adds
a pairwise comparison.

## Usage

``` r
model_compare(
  model,
  model2 = NULL,
  ndraws_ppc = 100,
  moment_match = FALSE,
  moment_match_args = list(),
  reloo_args = list(),
  plot_types = c("pp_check", "params"),
  comparison_metrics = c("loo", "waic", "bf"),
  run_prior_sensitivity = FALSE,
  sensitivity_vars = NULL,
  ...
)
```

## Arguments

- model:

  An `hbmfit` or `brmsfit` object.

- model2:

  Optional second `hbmfit` for pairwise comparison.

- ndraws_ppc:

  Number of draws for the posterior-predictive plot (default `100`).

- moment_match:

  Logical; use moment matching for LOO (default `FALSE`).

- moment_match_args:

  Named list of arguments for moment matching.

- reloo_args:

  Named list of arguments for
  [`reloo`](https://paulbuerkner.com/brms/reference/reloo.brmsfit.html).

- plot_types:

  Character vector. Any subset of `c("pp_check", "params")`.

- comparison_metrics:

  Character vector. Any subset of `c("loo", "waic", "bf")`.

- run_prior_sensitivity:

  Logical; run prior sensitivity analysis using priorsense (default
  `FALSE`).

- sensitivity_vars:

  Variables for the sensitivity analysis.

- ...:

  Additional arguments.

## Value

An `hbmc_results` object with components `loo1`, `waic1`, `pp_check`,
`params`, and – when `model2` is given – also `loo2`, `waic2`, `bf`, and
`model2`.

## See also

[`model_compare_all`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md),
[`model_average`](https://madsyair.github.io/hbsaems/reference/model_average.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
FAST <- list(chains = 2, iter = 2000, warmup = 1000, cores = 1,
             seed = 123, refresh = 0)

m1 <- do.call(hbm, c(list(formula = brms::bf(y ~ x1 + x2 + x3),
                          data = data_fhnorm), FAST))
#> Warning: Model fitted without any area-level random effects.
#>   This is unusual for Small Area Estimation: the standard Fay-Herriot model assumes u_i ~ N(0, sigma_u^2) per area, so estimates from a purely fixed-effects model will not borrow strength across areas.
#>   Consider one of:
#>     re = ~ (1 | area_id)                                     # IID area RE
#>     spatial_var = 'area_id', spatial_model = 'car', M = W    # CAR spatial RE
#>     spatial_var = 'area_id', spatial_model = 'sar', M = W    # SAR spatial RE
#>   If a fixed-effects-only baseline is intentional, you can suppress this warning with `suppressWarnings()`.
#> Compiling Stan program...
#> Start sampling
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
#> Start sampling

model_compare(m1)            # single-model goodness-of-fit
#> Warning: 
#> 1 (1.0%) p_waic estimates greater than 0.4. We recommend trying loo instead.
#> 
#> Model Comparison  [hbmc_results]
#> -----------------------------------
#>  ELPD-LOO  (m1): -179.10
#>  ELPD-WAIC (m1): -179.07
#> 
model_compare(m1, m2)        # pairwise comparison
#> Warning: 
#> 1 (1.0%) p_waic estimates greater than 0.4. We recommend trying loo instead.
#> Warning: 
#> 1 (1.0%) p_waic estimates greater than 0.4. We recommend trying loo instead.
#> Iteration: 1
#> Iteration: 2
#> Iteration: 3
#> Iteration: 4
#> Iteration: 5
#> Iteration: 1
#> Iteration: 2
#> Iteration: 3
#> Iteration: 4
#> Iteration: 5
#> 
#> Model Comparison  [hbmc_results]
#> -----------------------------------
#>  ELPD-LOO  (m1): -179.10
#>  ELPD-LOO  (m2): -180.88
#>  ELPD-WAIC (m1): -179.07
#>  ELPD-WAIC (m2): -180.86
#> 
# }
```
