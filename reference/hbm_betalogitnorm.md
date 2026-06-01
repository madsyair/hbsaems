# Small Area Estimation Under a Beta Likelihood (Logit-Normal Link)

Convenience wrapper around
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for SAE
problems where the response \\y_i \in (0, 1)\\ is modelled as \$\$y_i
\mid \theta_i, \phi_i \sim \mathrm{Beta}(\theta_i \phi_i, (1 -
\theta_i)\phi_i),\$\$ \$\$\mathrm{logit}(\theta_i) = x_i^\top
\boldsymbol{\beta} + u_i, \quad u_i \sim \mathcal{N}(0, \sigma_u^2).\$\$

## Usage

``` r
hbm_betalogitnorm(
  response,
  auxiliary = NULL,
  data,
  n = NULL,
  deff = NULL,
  area_var = NULL,
  area_re_structure = c("nested", "crossed"),
  spatial_var = NULL,
  spatial_model = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  link_phi = NULL,
  prior = NULL,
  stanvars = NULL,
  handle_missing = NULL,
  m = 5L,
  control = list(),
  chains = 4L,
  iter = 4000L,
  warmup = floor(iter/2),
  cores = 1L,
  sample_prior = "no",
  fixed_params = NULL,
  predictors = NULL,
  group = NULL,
  sre = NULL,
  sre_type = NULL,
  ...
)
```

## Arguments

- response:

  Character. Name of the response column (must lie strictly between 0
  and 1).

- auxiliary:

  Character vector of auxiliary (fixed-effect) variable names;
  corresponds to area-level covariates in SAE literature (see Rao &
  Molina 2015 Ch. 4).

- data:

  A data frame.

- n:

  Character or `NULL`. Name of the column giving the per-area sample
  size used to compute the fixed \\\phi\\. Must be supplied together
  with `deff`. When supplied, \\\phi_i = n_i / \text{deff}\_i - 1\\ is
  pinned via offset. Default: `NULL` (treats `phi` as random with brms's
  default prior).

- deff:

  Character or `NULL`. Name of the design-effect column. Required when
  `n` is supplied (and vice versa).

- area_var:

  Character vector or `NULL`. Name(s) of the area-grouping column(s).
  Length 1 adds an IID random intercept `(1 | area_var)`; length
  \\\geq\\ 2 supports hierarchical areas (e.g.\\
  `c("province", "regency")`) – see
  [`?hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  for the nested vs.\\ crossed structures.

- area_re_structure:

  Either `"nested"` (default) or `"crossed"`; controls how multiple area
  columns combine.

- spatial_var, spatial_model, car_type, sar_type, M:

  Spatial random-effect arguments, forwarded to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

- link_phi:

  Character or `NULL`. Link function for `phi`. **Default `NULL`
  resolves automatically**: `"identity"` when `phi` is pinned via the
  survey design (the `n + deff` sugar or `fixed_params$phi`, both of
  which produce raw positive offsets) and `"log"` otherwise (the brms
  default for `phi` in the Beta family; keeps \\\phi \> 0\\ while
  letting NUTS sample on \\\mathbb{R}\\). Manually setting `"identity"`
  together with an estimated `phi` can let NUTS propose negative values
  and trigger divergent transitions; an informative warning is emitted
  in that case.

- prior:

  Optional `brmsprior` object. If `NULL`, sensible defaults are filled
  in:

  - `Intercept ~ student_t(4, 0, 10)`

  - `b ~ student_t(4, 0, 2.5)`

  - `phi` – brms default `gamma(0.01, 0.01)` (in random mode; ignored in
    fixed mode).

  The user may pass a partial prior: missing default classes are filled
  in automatically. To put a custom prior on `phi` (random mode), set
  `prior = set_prior("gamma(2, 0.5)", class = "phi")` or similar.

- stanvars:

  Optional `brmsstanvars` object for power users who need to inject
  custom Stan code blocks (e.g.\\ transformed data, generated
  quantities, model-block statements not expressible via the `prior`
  argument). Passed through to brms verbatim. **Note:** As of v1.0.0,
  this wrapper no longer declares `alpha` or `beta` as Stan parameters,
  so legacy `stanvars` blocks containing sampling statements on `alpha`
  / `beta` (left over from the pre-v1.0.0 hierarchical-phi construction)
  will now raise an informative error.

- handle_missing, m, control, chains, iter, warmup, cores, sample_prior,
  ...:

  Passed through to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

- fixed_params:

  Optional named list pinning distributional parameters to known values.
  See [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  the spec format. Allows power-user access to the same machinery used
  by the `n`/`deff` arguments.

- predictors:

  **Deprecated.** Use `auxiliary` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- group:

  **Deprecated.** Use `area_var` instead.

- sre:

  **Deprecated.** Use `spatial_var` instead.

- sre_type:

  **Deprecated.** Use `spatial_model` instead.

## Value

An object of class `hbmfit`.

## Details

The precision parameter \\\phi_i\\ can either be pinned to a survey
design effect (`n` + `deff`) or sampled with brms's default
weakly-informative prior \\\phi \sim \mathrm{Gamma}(0.01, 0.01)\\.

## Migration note (v1.0.0)

Earlier versions of this wrapper introduced a hierarchical construction
\\\phi \sim \mathrm{Gamma}(\alpha, \beta),\\ \alpha \sim
\mathrm{Gamma}(1,1),\\ \beta \sim \mathrm{Gamma}(1,1)\\ for the
random-phi mode, declaring `alpha` and `beta` as Stan parameters with
hyperpriors injected via `stanvars`. Starting v1.0.0, that construction
has been removed in favour of brms's own default prior, \\\phi \sim
\mathrm{Gamma}(0.01, 0.01)\\ with lower bound 0 (mean 1, variance 100;
weakly informative on the precision scale). Three reasons:

- **Brittle priors on alpha/beta.** The hyperprior
  \\\mathrm{Gamma}(1,1)\\ on \\\alpha\\ has prior mean 1, which is on
  the boundary of the parameter space declared as `real<lower=1>`. This
  routinely produced divergent transitions when the data were not
  informative about phi.

- **Parameter blow-up.** Estimating two extra Stan parameters per
  area-level model with limited data inflated the effective posterior
  dimension and slowed convergence.

- **Cleaner brms semantics.** Letting brms apply its own default
  \\\mathrm{Gamma}(0.01, 0.01)\\ means that passing `prior = NULL` now
  does exactly what the user expects: “brms defaults”. No surprises.

**If you need to reproduce the old behaviour**, supply `stanvars`
yourself to declare `alpha`, `beta` and their hyperpriors, and pass
`prior = set_prior("gamma(alpha, beta)", class = "phi")`. Pre-v1.0.0
code that supplied `stanvars` with hyperpriors on `alpha`/`beta` will
now raise an informative error.

## Conflict policy

When the precision parameter \\\phi\\ is pinned via `n` + `deff` (or via
`fixed_params$phi`), the function refuses any additional specification
that would also set \\\phi\\. Specifically, all of the following are
rejected with an informative error at construction time:

- `n` supplied without `deff`, or vice versa.

- `n` + `deff` *and* `fixed_params$phi`.

- `n` + `deff` *and* a user `prior` on `class = "phi"`.

- `n` + `deff` *and* a `stanvars` hyperprior on `alpha` or `beta` (the
  \\\mathrm{Gamma}(\alpha, \beta)\\ hyperparameters used in
  random-\\\phi\\ mode).

- `auxiliary` *and* the deprecated `predictors` in the same call.

## References

Liu, B. (2009). *Hierarchical Bayes Estimation and Empirical Best
Prediction of Small-Area Proportions*. University of Maryland.

Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*, 2nd ed.
Wiley, p. 390.

## See also

[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_betalogitnorm")
data <- data_betalogitnorm

# -- 1. Basic model (phi random, with default hyperprior) --------------------
model1 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  area_var   = "regency",
  data       = data,
  chains = 4, iter = 2000, warmup = 1000, refresh = 0
)
#> Warning: Area column 'regency' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Start sampling
summary(model1)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : beta (link: logit )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + (1 | regency), pforms = list(),      pfix = list(), resp = "y", family = structure(list(family = "beta",          link = "logit", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "phi"),          type = "real", ybounds = c(0, 1), closed = c(FALSE, FALSE         ), ad = c("weights", "subset", "cens", "trunc", "mi",          "index"), link_phi = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#>  Family: beta 
#>   Links: mu = logit 
#> Formula: y ~ x1 + x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(0.01, 0.984074547994896, 0.01 (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.15      0.12     0.01     0.43 1.00     2027     1433
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    -0.47      0.78    -2.01     1.08 1.00     7539     3048
#> x1           -0.13      0.06    -0.24    -0.01 1.00     6874     3177
#> x2           -0.06      0.04    -0.15     0.02 1.00     7475     3069
#> x3            0.06      0.04    -0.01     0.13 1.00     5766     2844
#> 
#> Further Distributional Parameters:
#>     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> phi     0.87      0.11     0.67     1.11 1.00     4486     3195
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- 2. Fixed phi via survey design (n + deff) -------------------------------
model2 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  n          = "n",
  deff       = "deff",
  area_var   = "regency",
  data       = data,
  chains = 4, iter = 2000, warmup = 1000, refresh = 0
)
#> Warning: Area column 'regency' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Start sampling
summary(model2)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : beta (link: logit )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + (1 | regency), pforms = list(     phi = phi ~ 0 + offset(.hbsaems_phi_fixed)), pfix = list(),      resp = "y", family = structure(list(family = "beta", link = "logit",          linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "phi"),          type = "real", ybounds = c(0, 1), closed = c(FALSE, FALSE         ), ad = c("weights", "subset", "cens", "trunc", "mi",          "index"), link_phi = "identity"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#>  Family: beta 
#>   Links: mu = logit; phi = identity 
#> Formula: y ~ x1 + x2 + x3 + (1 | regency) 
#>          phi ~ 0 + offset(.hbsaems_phi_fixed)
#>    Data: structure(list(y = c(0.01, 0.984074547994896, 0.01 (Number of observations: 100) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     2.42      0.20     2.06     2.85 1.00     1227     1874
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    -1.55      1.47    -4.42     1.31 1.00      792     1602
#> x1           -0.33      0.12    -0.56    -0.11 1.00      781     1735
#> x2           -0.15      0.08    -0.31     0.01 1.00      907     1713
#> x3            0.17      0.07     0.03     0.31 1.00      804     1417
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- 3. Custom prior on phi via the standard `prior` argument ----------------
#
# Starting v1.0.0, hbm_betalogitnorm() no longer declares its own
# alpha / beta hyperparameters; phi uses brms's native default
# Gamma(0.01, 0.01).  To override that prior, pass a `brms::set_prior()`
# entry to the `prior` argument -- the legacy
# stanvar("alpha ~ gamma(...); beta ~ gamma(...)") pattern would now
# error because alpha/beta are no longer Stan parameters.
model3 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  area_var   = "regency",
  data       = data,
  prior      = brms::set_prior("gamma(2, 0.5)", class = "phi"),
  chains = 4, iter = 2000, warmup = 1000, refresh = 0
)
#> Warning: Area column 'regency' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Start sampling

# -- 4. Spatial CAR model ----------------------------------------------------
data("adjacency_matrix_car")
model4 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  n          = "n",
  deff       = "deff",
  spatial_var = "province",
  spatial_model   = "car",
  M          = adjacency_matrix_car,
  data       = data,
  chains = 4, iter = 2000, warmup = 1000, refresh = 0
)
#> Compiling Stan program...
#> Start sampling
# }
```
