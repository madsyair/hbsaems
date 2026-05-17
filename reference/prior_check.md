# Prior Predictive Check for Fitted HBMs

Generates prior predictive samples from a model fit with
`sample_prior = "only"` and compares them to the observed data. This is
the primary prior-check function (supersedes the deprecated
[`hbpc`](https://madsyair.github.io/hbsaems/reference/deprecated.md)).

## Usage

``` r
prior_check(model, data, response_var, ndraws_ppc = 50, ...)
```

## Arguments

- model:

  An `hbmfit` or `brmsfit` object fit with `sample_prior = "only"` (see
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)).

- data:

  A `data.frame` containing the response variable.

- response_var:

  Character scalar. Name of the response variable column.

- ndraws_ppc:

  Integer. Number of prior predictive draws to overlay on the plot
  (default `50`).

- ...:

  Currently unused; reserved for future extensions.

## Value

An `hbpc_results` object with components:

- `prior_predictive_plot`:

  A `ggplot` from
  [`pp_check`](https://mc-stan.org/bayesplot/reference/pp_check.html),
  or `NULL` if it could not be generated.

- `prior_draws`:

  A draws matrix from
  [`posterior_predict`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
  sized `ndraws_ppc \times nrow(data)`.

- `observed`:

  The observed response vector.

## Details

The prior predictive distribution is \$\$p(y\_{\text{rep}}) = \int
p(y\_{\text{rep}} \mid \theta)\\ p(\theta) \\ \mathrm{d}\theta,\$\$ that
is, the marginal distribution of new data \\y\_{\text{rep}}\\ under the
prior alone. Comparing this to the observed data is a fast sanity check:
if the prior predictive places no mass anywhere near the data, the
priors are likely too tight or in the wrong location.

## See also

[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md),
[`convergence_check`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
# `sample_prior = "only"` requires all coefficients to have a proper
# prior; supply explicit priors on the regression class.
model_prior <- hbm(
  formula      = brms::bf(y ~ x1 + x2 + x3),
  data         = data_fhnorm,
  sample_prior = "only",
  prior        = c(
    brms::prior(normal(0, 1), class = "b"),
    brms::prior(normal(0, 5), class = "Intercept")
  ),
  chains = 2, iter = 1000, warmup = 500, cores = 1,
  seed = 42, refresh = 0
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
pc <- prior_check(model_prior,
                  data         = data_fhnorm,
                  response_var = "y")
#> Error: object 'model_prior' not found
print(pc)
#> Error: object 'pc' not found
plot(pc)
#> Error: object 'pc' not found
# }
```
