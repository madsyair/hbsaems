# hbm: Hierarchical Bayesian Small Area Models

Fits a hierarchical Bayesian model for Small Area Estimation (SAE) using
the `brms` package (Stan back-end). The function supports fixed effects,
random effects, spatial random effects (CAR/SAR), user-defined priors,
and three strategies for handling missing data in auxiliary (predictor)
variables.

## Usage

``` r
hbm(
  formula,
  hb_sampling = "gaussian",
  hb_link = "identity",
  link_phi = "log",
  re = NULL,
  sre = NULL,
  sre_type = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  data,
  prior = NULL,
  fixed_params = NULL,
  prior_type = "default",
  hs_df = 1,
  hs_df_global = 1,
  hs_df_slab = 4,
  hs_scale_global = NULL,
  hs_scale_slab = 2,
  hs_par_ratio = NULL,
  r2d2_mean_R2 = 0.5,
  r2d2_prec_R2 = 2,
  r2d2_cons_D2 = NULL,
  nonlinear = NULL,
  nonlinear_type = "spline",
  spline_k = -1L,
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
  stanvars = NULL,
  ...
)
```

## Arguments

- formula:

  A
  [`brmsformula`](https://paulbuerkner.com/brms/reference/brmsformula.html)
  or standard `formula` object specifying the model structure. For
  multi-response or imputation sub-models use
  [`bf`](https://paulbuerkner.com/brms/reference/brmsformula.html).
  Examples: `formula(y ~ x1 + x2)`, `bf(y ~ x1 + x2)`, or
  `bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ x2)`.

- hb_sampling:

  Character string naming the distribution family of the response
  variable (default: `"gaussian"`). Any family supported by
  [`brmsfamily`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  is accepted.

- hb_link:

  Character string specifying the link function (default: `"identity"`).

- link_phi:

  Character string specifying the link function for the precision/phi
  parameter (default: `"log"`). Only used for Beta and related families.

- re:

  An optional one-sided `formula` specifying group-level (random)
  effects, e.g. `~(1|area)`. Must follow standard `lme4`-style syntax:
  `~ (1|group1) + (1|group2)`. If `NULL` (default) *and* `sre_type` is
  also `NULL`, the function emits a warning recommending an area-level
  random effect, since a Fay-Herriot SAE model with neither IID nor
  spatial area effects degenerates to fixed-effects regression and does
  not borrow strength across areas. The warning can be silenced with
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) if a
  fixed-effects-only baseline is intentional.

- sre:

  Character. Name of the grouping variable used for spatial random
  effects. Must be supplied *together with* `sre_type` and `M`;
  providing only one of them is an error.

- sre_type:

  Character. Type of spatial model: `"car"` (Conditional Autoregressive)
  or `"sar"` (Simultaneous Autoregressive). Must be supplied *together
  with* `sre` and `M`; providing only one of them is an error.

- car_type:

  Character. CAR subtype passed to `brms`: one of `"escar"` (exact
  sparse CAR), `"esicar"` (exact sparse intrinsic CAR), `"icar"`
  (intrinsic CAR), or `"bym2"`. Defaults to `"icar"` when
  `sre_type = "car"`.

- sar_type:

  Character. SAR subtype: `"lag"` (SAR of the response values) or
  `"error"` (SAR of the residuals). Defaults to `"lag"` when
  `sre_type = "sar"`.

- M:

  Spatial matrix supplied as `data2` to `brms`. For CAR this must be a
  binary adjacency matrix; for SAR a spatial weight matrix. Row names
  must match the levels of the spatial grouping variable.

- data:

  A data frame containing all variables referenced in `formula`.

- prior:

  Priors specified via
  [`prior`](https://paulbuerkner.com/brms/reference/set_prior.html) or a
  list thereof. If `NULL` (default), `brms` default priors are used.

- fixed_params:

  Named list pinning distributional parameters to known values instead
  of sampling them. Each entry maps a parameter name to one of:

  A column name (character)

  :   The named column in `data` is used as the fixed values.

  A scalar (numeric, length 1)

  :   Broadcast to all rows.

  A vector (numeric, length `nrow(data)`)

  :   Used directly.

  A one-sided formula (e.g. `~ I(n / deff - 1)`)

  :   Evaluated against `data` to produce the vector of fixed values.

  Internally, each pinned parameter `<par>` is attached to the data as a
  column `.hbsaems_<par>_fixed` and added to the brms formula as
  `<par> ~ 0 + offset(.hbsaems_<par>_fixed)`. Using `fixed_params` on a
  parameter for which the user also supplies an explicit prior is an
  error. Typical use cases include pinning `phi` for Beta regression
  from survey design effect (`phi = ~ I(n / deff - 1)`) and pinning
  `sigma` for Fay–Herriot-style models with known sampling variance.

- prior_type:

  Character. Global-local shrinkage prior applied to all regression
  coefficients (`class = "b"`). One of:

  `"default"`

  :   No shrinkage prior is added; the `prior` argument governs
      everything (default).

  `"horseshoe"`

  :   Regularised horseshoe prior (Piironen & Vehtari 2017). Encourages
      exact sparsity while allowing large signals through. Controlled by
      `hs_df`, `hs_df_global`, `hs_df_slab`, `hs_scale_global`,
      `hs_scale_slab`, and `hs_par_ratio`.

  `"r2d2"`

  :   R2D2 prior (Zhang et al. 2022). Places a prior directly on the
      model \\R^2\\ and distributes explained variance across predictors
      via a Dirichlet decomposition. Controlled by `r2d2_mean_R2`,
      `r2d2_prec_R2`, and `r2d2_cons_D2`.

  If `prior` already contains a global `class = "b"` entry, `prior_type`
  is ignored and a warning is issued.

- hs_df:

  Numeric \\\> 0\\. Local half-\\t\\ degrees of freedom for the
  horseshoe prior (default `1` = half-Cauchy).

- hs_df_global:

  Numeric \\\> 0\\. Global half-\\t\\ degrees of freedom (default `1`).

- hs_df_slab:

  Numeric \\\> 0\\. Slab half-\\t\\ degrees of freedom (default `4`).

- hs_scale_global:

  Numeric \\\> 0\\ or `NULL`. Scale for the global half-\\t\\ prior.
  `NULL` (default) lets `brms` compute it automatically from the number
  of predictors.

- hs_scale_slab:

  Numeric \\\> 0\\. Scale for the slab component (default `2`).

- hs_par_ratio:

  Numeric \\\> 0\\ or `NULL`. Expected ratio of non-zero to total
  coefficients. `NULL` (default) treats all coefficients as potentially
  non-zero.

- r2d2_mean_R2:

  Numeric in \\(0, 1)\\. Prior mean of the model \\R^2\\ (default
  `0.5`).

- r2d2_prec_R2:

  Numeric \\\> 0\\. Prior precision of \\R^2\\ (default `2`). Higher
  values concentrate mass around `r2d2_mean_R2`.

- r2d2_cons_D2:

  Numeric \\\> 0\\ or `NULL`. Dirichlet concentration for the D2
  component. `NULL` (default) corresponds to `0.5`, yielding a uniform
  distribution over the simplex.

- nonlinear:

  Character vector or `NULL`. Names of predictor variables to model with
  a smooth nonlinear term. Each listed variable is replaced in the
  formula RHS with `s(var)` (spline) or `gp(var)` (Gaussian process).
  Variables not listed remain linear. Do *not* also write `s(x)` in the
  formula when using this argument – the modification is applied
  automatically. Default `NULL` (all predictors remain linear).

- nonlinear_type:

  Character. Smooth term family to use. One of `"spline"` (default,
  thin-plate regression spline via
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html)) or `"gp"`
  (Gaussian process via
  [`brms::gp()`](https://paulbuerkner.com/brms/reference/gp.html)).

- spline_k:

  Integer. Spline basis dimension (number of knots). `-1L` (default)
  lets mgcv choose automatically. Values \\\geq 3\\ are also accepted.
  Ignored when `nonlinear_type = "gp"`.

- gp_scale:

  Numeric \\\> 0\\ or `NULL`. Length-scale hyperparameter (`c` in
  [`brms::gp()`](https://paulbuerkner.com/brms/reference/gp.html)).
  `NULL` (default) uses the `brms` default. Ignored when
  `nonlinear_type = "spline"`.

- handle_missing:

  Character or `NULL`. Strategy for missing data. One of `"deleted"`,
  `"multiple"`, or `"model"` (see **Details**). If `NULL` (default) and
  missing values exist in the data, an informative error is raised.

- m:

  Integer. Number of imputations when `handle_missing = "multiple"`
  (default: `5`). Ignored for other strategies.

- mice_args:

  A named list of additional arguments forwarded to
  [`mice`](https://amices.org/mice/reference/mice.html), for example
  `list(method = "pmm", seed = 42)`. Only used when
  `handle_missing = "multiple"`.

- control:

  A named list of sampler control parameters (default:
  [`list()`](https://rdrr.io/r/base/list.html)). Passed directly to
  [`brm`](https://paulbuerkner.com/brms/reference/brm.html).

- chains:

  Integer. Number of MCMC chains (default: `4`).

- iter:

  Integer. Total iterations per chain (default: `4000`).

- warmup:

  Integer. Warm-up iterations per chain (default: `floor(iter / 2)`).

- cores:

  Integer. Number of CPU cores for parallel sampling (default: `1`).

- sample_prior:

  Character. Whether to draw from the prior distribution. One of `"no"`
  (default), `"yes"`, or `"only"`.

- stanvars:

  An optional
  [`stanvar`](https://paulbuerkner.com/brms/reference/stanvar.html)
  object (or a list of such objects) supplying additional Stan code,
  data, or parameters. Passed directly to
  [`brm`](https://paulbuerkner.com/brms/reference/brm.html) and
  [`brm_multiple`](https://paulbuerkner.com/brms/reference/brm_multiple.html).
  Intended for use by wrapper functions such as
  [`hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  that require custom Stan blocks; end users typically do not need to
  set this argument. Default: `NULL`.

- ...:

  Additional arguments forwarded to
  [`brm`](https://paulbuerkner.com/brms/reference/brm.html) or
  [`brm_multiple`](https://paulbuerkner.com/brms/reference/brm_multiple.html).

## Value

An object of class `hbmfit`, which is a named list containing:

- model:

  The fitted `brmsfit` object (or `brmsfit_multiple` when
  `handle_missing = "multiple"` with missing predictors).

- handle_missing:

  The missing-data strategy used (`NULL` if the data were complete).

- data:

  The **original** data frame passed to `hbm()` before any row deletion
  or imputation. This is intentional:
  [`hbsae`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  needs all rows – including those with missing \\Y\\ – to generate
  predictions for every small area.

## Details

Hierarchical Bayesian Model for Small Area Estimation

\*\*Spatial Small Area Estimation Models\*\*

For spatially correlated areas, hbsaems extends the standard area-level
SAE model (Fay-Herriot 1979) by adding a spatially structured random
effect: \$\$y_i = x_i^\top \boldsymbol{\beta} + u_i + e_i,\$\$ where
\\u_i\\ is the spatial random effect for area \\i\\ and \\e_i\\ the
sampling error. Two families of spatial structures are supported.

- **CAR (Conditional Autoregressive; Besag 1974)**:

  Specified by `sre_type = "car"`. The joint distribution of the spatial
  effects is \$\$u \sim \mathcal{N}\bigl(0,\\ \sigma_u^2 (D - \rho
  W)^{-1}\bigr),\$\$ where \\W\\ is a binary adjacency matrix (1 if
  neighbour, 0 otherwise) and \\D = \mathrm{diag}(W \mathbf{1})\\.
  Sub-types via `car_type`: `"icar"` (intrinsic, \\\rho = 1\\; Besag
  1991); `"escar"`, `"esicar"` (exact sparse formulations of Morris et
  al.\\ 2019); `"bym2"` (BYM2 reparameterisation of Riebler et al.\\
  2016, recommended for disconnected graphs).

- **SAR (Simultaneous Autoregressive; Whittle 1954, Anselin 1988)**:

  Specified by `sre_type = "sar"`. The model is \$\$u = \rho W u +
  \varepsilon, \quad \varepsilon \sim \mathcal{N}(0,
  \sigma\_\varepsilon^2 I),\$\$ where \\W\\ is row-standardised so that
  \\\rho \in (-1, 1)\\ carries an interpretable correlation meaning.
  Sub-types via `sar_type`: `"lag"` (spatial lag of the response, \\y =
  \rho W y + X\boldsymbol{\beta} + \varepsilon\\); `"error"` (spatial
  error model).

Use
[`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md)
to verify that \\M\\ satisfies the theoretical requirements (square,
zero diagonal, symmetry for CAR, style-appropriate for the model class).
Use
[`build_spatial_weight`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
to construct \\M\\ from a shapefile.

\*\*Missing Data Strategies\*\*

The three strategies differ in scope and statistical assumptions:

- `"deleted"`:

  Complete-case analysis. Only rows where **all** response variable(s)
  are observed are used for model fitting. Auxiliary variables must be
  complete; otherwise an informative error is raised. Appropriate under
  MCAR (Missing Completely At Random).

- `"multiple"`:

  Multiple imputation via `mice` **for auxiliary (predictor) variables
  only**. The response variable \\Y\\ is **never** imputed. In a
  Bayesian model, missing outcomes are naturally marginalised through
  the posterior predictive distribution: \$\$p(\theta \mid
  Y\_{\text{obs}}, X) = \int p(\theta \mid Y\_{\text{obs}},
  Y\_{\text{mis}}, X)\\ p(Y\_{\text{mis}} \mid Y\_{\text{obs}}, X,
  \theta)\\ \mathrm{d}Y\_{\text{mis}}.\$\$ Imputing \\Y\\ before fitting
  would replace this integral with a single point substitute, deflate
  posterior uncertainty, and potentially bias the estimates if the
  imputation model is misspecified. If \\Y\\ has missing values, those
  rows are excluded from model fitting (a `warning` is issued) but are
  retained in the returned object for subsequent prediction via
  [`hbsae`](https://madsyair.github.io/hbsaems/reference/deprecated.md).
  Appropriate under MAR (Missing At Random).

- `"model"`:

  Model-based imputation using
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html).
  Missing values in auxiliary variables are jointly estimated with the
  model parameters. The user must specify imputation sub-models
  explicitly in the `formula` argument, e.g.:
  `bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ x2)`. Only applicable to
  **continuous** distributions. Appropriate under MAR.

If data are Missing Not At Random (MNAR), none of the above strategies
applies directly; sensitivity analyses and explicit missingness models
are recommended.

## References

Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*. John Wiley
& Sons.

Burkner, P. C. (2017). brms: An R package for Bayesian multilevel models
using Stan. *Journal of Statistical Software*, 80(1), 1–28.

van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
imputation by chained equations in R. *Journal of Statistical Software*,
45(3), 1–67.

## Author

Achmad Syahrul Choir, Saniyyah Sri Nurhayati, and Sofi Zamzanah

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
data <- data_fhnorm

# Standard lightweight MCMC settings used throughout these examples.
# Use chains = 4, iter = 4000 for production runs.
FAST <- list(chains = 2, iter = 2000, warmup = 1000, cores = 1,
             seed = 123, refresh = 0)

# -- Basic model --------------------------------------------------------------
model <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       hb_sampling = "gaussian",
       hb_link     = "identity",
       re          = ~(1 | group),
       data        = data),
  FAST
))
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model)
#> Error: object 'model' not found

# -- Horseshoe prior (sparse coefficients) ------------------------------------
model_hs <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       re          = ~(1 | group),
       data        = data,
       prior_type  = "horseshoe",
       hs_df       = 1),
  FAST
))
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_hs)
#> Error: object 'model_hs' not found

# -- R2D2 prior (prior on model R-squared) -------------------------------------
model_r2 <- do.call(hbm, c(
  list(formula      = bf(y ~ x1 + x2 + x3),
       re           = ~(1 | group),
       data         = data,
       prior_type   = "r2d2",
       r2d2_mean_R2 = 0.5,
       r2d2_prec_R2 = 2),
  FAST
))
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_r2)
#> Error: object 'model_r2' not found

# -- Spline smooth for x1 (nonlinear) -----------------------------------------
# x1 is modelled with s(x1); x2 and x3 remain linear.
model_spline <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | group),
       data           = data,
       nonlinear      = "x1",
       nonlinear_type = "spline"),
  FAST
))
#> Warning: Variable(s) x1 appear in both 'predictors' (linear) and 'nonlinear'. They will be modelled nonlinearly ONLY. Remove them from 'predictors' to suppress this warning.
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_spline)
#> Error: object 'model_spline' not found

# -- Gaussian process for x2 (nonlinear) --------------------------------------
model_gp <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | group),
       data           = data,
       nonlinear      = "x2",
       nonlinear_type = "gp"),
  FAST
))
#> Warning: Variable(s) x2 appear in both 'predictors' (linear) and 'nonlinear'. They will be modelled nonlinearly ONLY. Remove them from 'predictors' to suppress this warning.
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_gp)
#> Error: object 'model_gp' not found

# -- Missing data: deletion (Y missing, X complete) ---------------------------
data_miss_y        <- data
data_miss_y$y[3:5] <- NA

model_deleted <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | group),
       data           = data_miss_y,
       handle_missing = "deleted"),
  FAST
))
#> handle_missing = 'deleted': 3 row(s) with missing response variable removed from model fitting.
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_deleted)
#> Error: object 'model_deleted' not found

# -- Missing data: multiple imputation (X only -- Y is never imputed) ----------
data_miss_x          <- data
data_miss_x$x1[6:8]  <- NA

model_multiple <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | group),
       data           = data_miss_x,
       handle_missing = "multiple",
       m              = 5),
  FAST
))
#> Missing predictor variable(s): x1. Applying multiple imputation (mice) with m = 5 imputations.
#> Compiling the C++ model
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_multiple)
#> Error: object 'model_multiple' not found

# -- Missing data: model-based imputation with mi() ---------------------------
data_miss_x2         <- data
data_miss_x2$x1[6:7] <- NA

model_model <- do.call(hbm, c(
  list(formula        = bf(y | mi() ~ mi(x1) + x2 + x3) +
                        bf(x1 | mi() ~ x2 + x3),
       re             = ~(1 | group),
       data           = data_miss_x2,
       handle_missing = "model"),
  FAST
))
#> handle_missing = 'model': using mi() specification for joint model-based imputation.
#> Setting 'rescor' to FALSE by default for this model
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_model)
#> Error: object 'model_model' not found

# -- Spatial: CAR (Conditional Autoregressive) --------------------------------
data("adjacency_matrix_car")
model_car <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       data        = data,
       sre         = "sre",
       sre_type    = "car",
       M           = adjacency_matrix_car),
  FAST
))
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_car)
#> Error: object 'model_car' not found

# -- Spatial: SAR (Simultaneous Autoregressive) -------------------------------
data("spatial_weight_sar")
model_sar <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       data        = data,
       sre         = "sre",
       sre_type    = "sar",
       M           = spatial_weight_sar),
  FAST
))
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model_sar)
#> Error: object 'model_sar' not found
# }
```
