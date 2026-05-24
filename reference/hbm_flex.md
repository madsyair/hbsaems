# Fit a Flexible HBSAE Model with Any Registered Family

Flexible factory that fits an HBSAE model using any distribution
currently registered in the family registry, together with the full set
of cross-cutting features: spatial random effects (CAR/SAR), shrinkage
priors (horseshoe, R2D2), smooth terms (penalised splines, Gaussian
processes), auxiliary-parameter hyperpriors, and missing-data
strategies. Distribution-specific wrappers
([`hbm_lnln`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md),
[`hbm_binlogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md),
[`hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md))
are thin signature shims around this function with preset `family_key`
values. Advanced users can also call it directly once a custom family
has been registered via
[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md).

## Usage

``` r
hbm_flex(
  family_key,
  response,
  auxiliary = NULL,
  data,
  addition_var = NULL,
  area_var = NULL,
  area_re_structure = c("nested", "crossed"),
  spatial_var = NULL,
  spatial_model = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  prior = NULL,
  fixed_params = NULL,
  sampling_variance = NULL,
  prior_type = "default",
  hs_df = 1,
  hs_df_global = 1,
  hs_df_slab = 4,
  hs_scale_global = NULL,
  hs_scale_slab = 2,
  hs_par_ratio = NULL,
  hs_autoscale = TRUE,
  r2d2_mean_R2 = 0.5,
  r2d2_prec_R2 = 2,
  r2d2_cons_D2 = NULL,
  r2d2_autoscale = TRUE,
  nonlinear = NULL,
  nonlinear_type = "spline",
  spline_k = -1L,
  spline_bs = "tp",
  gp_k = NA_integer_,
  gp_cov = "exp_quad",
  gp_c = NULL,
  gp_scale = NULL,
  handle_missing = NULL,
  m = 5L,
  mice_args = list(),
  control = list(),
  chains = 4L,
  iter = 4000L,
  warmup = floor(iter/2),
  cores = 1L,
  sample_prior = "no",
  link = NULL,
  aux_args = NULL,
  stanvars = NULL,
  predictors = NULL,
  group = NULL,
  sre = NULL,
  sre_type = NULL,
  ...
)
```

## Arguments

- family_key:

  Character. The registry key of the desired family (e.g.\\
  `"lognormal"`, `"binomial"`, `"gamma_log"`).

- response:

  Character. Name of the response variable column.

- auxiliary:

  Character vector. Names of auxiliary (fixed-effect) variables;
  corresponds to area-level covariates in SAE literature.

- data:

  A `data.frame`.

- addition_var:

  Character or `NULL`. Name of the addition term variable (e.g.\\ trials
  for binomial). Required when the family spec has
  `has_addition_term = TRUE`.

- area_var:

  Character vector or `NULL`. Name(s) of the column(s) in `data`
  identifying the areas. Three usage modes:

  - Length 1 (default behaviour): a single area-level random intercept
    `(1 | area_var)`.

  - Length \\\geq\\ 2 with `area_re_structure = "nested"` (default): a
    hierarchy of areas given from the *highest* to the *lowest* level,
    e.g. `c("province", "regency")` yields `(1 | province / regency)`
    which brms expands to `(1 | province) + (1 | province:regency)`.
    This is the canonical multi-stage SAE setup.

  - Length \\\geq\\ 2 with `area_re_structure = "crossed"`: non-nested
    levels, e.g.\\ adding separate effects for
    `(1 | province) + (1 | urbanrural)`. Use only when the levels are
    truly crossed rather than hierarchically nested.

- area_re_structure:

  Either `"nested"` (default) or `"crossed"`. Only consulted when
  `area_var` has length \\\geq\\ 2. See above.

- spatial_var, spatial_model, car_type, sar_type, M:

  Spatial random-effect arguments forwarded to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md). See
  [`?hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for a
  full description.

- prior, prior_type, hs_df, hs_df_global, hs_df_slab, hs_scale_global,
  hs_scale_slab, hs_par_ratio, hs_autoscale, r2d2_mean_R2, r2d2_prec_R2,
  r2d2_cons_D2, r2d2_autoscale:

  Shrinkage prior arguments forwarded to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md). When the
  formula contains
  [`s()`](https://paulbuerkner.com/brms/reference/s.html) or
  [`gp()`](https://paulbuerkner.com/brms/reference/gp.html) terms, the
  prior is automatically cascaded to the corresponding parameter classes
  (`"sds"`, `"sdgp"`) via the brms `main = TRUE` pattern.

- fixed_params:

  Optional named list pinning distributional parameters to known values.
  See [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  the spec format. Allows power-user access to the generic
  fixed-parameter machinery (works with custom families too).

- sampling_variance:

  Optional character. Name of a column in `data` containing the
  **known** sampling variance \\D_i\\ for each area (the Fay-Herriot
  sugar). When supplied, \\\sigma_i = \sqrt{D_i}\\ is pinned via offset.
  Forwarded to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md), where a
  family-compatibility check ensures the family exposes a residual SD
  parameter named `sigma` (gaussian, lognormal, student, skew_normal,
  exgaussian, asym_laplace). Incompatible families (beta, binomial,
  poisson, etc.) raise an explicit error pointing at the family-specific
  alternative. See
  [`?hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  details.

- nonlinear:

  Character vector of variable names to include as smooth/nonlinear
  terms (rather than linear). Variables listed here that also appear in
  `auxiliary` are modelled nonlinearly only.

- nonlinear_type:

  Character. `"spline"` (penalised regression spline via mgcv, default)
  or `"gp"` (Gaussian process via brms).

- spline_k:

  Integer. Spline basis dimension passed to `mgcv::s(..., k = ...)`.
  `-1` (default) lets mgcv choose automatically. For SAE typically
  `k = 8` to `15`.

- spline_bs:

  Character. Spline basis type passed to `mgcv::s(..., bs = ...)`.
  Defaults to `"tp"` (thin-plate regression spline). Set `"cr"` (cubic
  regression) for better numerical stability with correlated auxiliary
  variables. Other choices: `"cs"` (cubic with shrinkage), `"ps"`
  (P-splines).

- gp_k:

  Integer or `NA`. Number of basis functions for the Hilbert-space
  approximate GP (Riutort-Mayol et al.\\ 2020). `NA` (default) = exact
  GP, scales \\O(n^3)\\ and is **not recommended for** \\n \> 100\\
  areas. Integer `gp_k = 10`–`25` is typical for SAE applications and
  dramatically improves convergence and runtime.

- gp_cov:

  Character. Covariance function: `"exp_quad"` (squared exponential /
  RBF, default), `"matern15"` (Matern 3/2), `"matern25"` (Matern 5/2,
  often more numerically stable than RBF), `"exponential"`.

- gp_c:

  Numeric. Hilbert-space GP boundary-scale factor passed to
  `brms::gp(c = ...)`. Default brms value is 5/4 (= 1.25); increase if
  the GP appears truncated at domain boundaries. Only relevant when
  `gp_k` is set.

- gp_scale:

  **Deprecated.** Use `gp_c` instead. The old name suggested a
  length-scale interpretation but actually mapped to the boundary-scale
  factor.

- handle_missing, m, mice_args:

  Missing-data arguments. When `handle_missing = NULL` the wrapper
  auto-selects a strategy based on the family registry's `supports_mi`
  flag.

- control, chains, iter, warmup, cores, sample_prior, link:

  Sampler and model-spec arguments forwarded to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

- aux_args:

  Optional named list of family-specific auxiliary arguments (e.g.\\
  `list(n = "n", deff = "deff")` for the Beta family's phi hyperprior).
  Forwarded to the family's `aux_param_hyperprior` callback if it has
  one.

- stanvars:

  Optional
  [`stanvar`](https://paulbuerkner.com/brms/reference/stanvar.html)
  object passed through to brms. When the family has an
  `aux_param_hyperprior` callback that returns its own `stanvars`, the
  two are concatenated.

- predictors:

  **Deprecated.** Use `auxiliary` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- group:

  **Deprecated.** Use `area_var` instead.

- sre:

  **Deprecated.** Use `spatial_var` instead.

- sre_type:

  **Deprecated.** Use `spatial_model` instead.

- ...:

  Additional arguments forwarded to
  [`brm`](https://paulbuerkner.com/brms/reference/brm.html).

## Value

An object of class `hbmfit`.

## Details

The factory performs five duties that were previously duplicated across
wrappers:

1.  Validate that `response`, `auxiliary`, and optional variables exist
    in `data`.

2.  Run the family's `response_check` on the response and report a
    human-readable error on failure.

3.  Auto-select a missing-data strategy that respects the family's
    `supports_mi` flag (e.g.\\ binomial cannot use `"model"`).

4.  Build the brms formula with optional addition terms and apply
    spline/GP transformations.

5.  Invoke the family's `aux_param_hyperprior` callback (if defined) so
    distributions with a hyperprior on auxiliary parameters – e.g.\\ phi
    for the Beta family, shape for Gamma, nu for Student-t – can inject
    Stan code without writing a thick wrapper file.

## See also

[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md),
[`list_hbsae_models`](https://madsyair.github.io/hbsaems/reference/list_hbsae_models.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# \donttest{
library(hbsaems)
data("data_lnln")
# Equivalent to hbm_lnln(...)
fit <- hbm_flex(
  family_key = "lognormal",
  response   = "y_obs",
  auxiliary  = c("x1", "x2", "x3"),
  area_var   = "district",         # area-level random effect: (1 | district)
  data       = data_lnln,
  chains = 2, iter = 1000, refresh = 0
)
#> Warning: Area column 'district' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
# }
```
