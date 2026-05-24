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
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Warning: Area column 'regency' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
summary(model1)
#> Error: object 'model1' not found

# -- 2. Fixed phi via survey design (n + deff) -------------------------------
model2 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  n          = "n",
  deff       = "deff",
  area_var   = "regency",
  data       = data,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Warning: Area column 'regency' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
summary(model2)
#> Error: object 'model2' not found

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
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Warning: Area column 'regency' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')

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
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
# }
```
