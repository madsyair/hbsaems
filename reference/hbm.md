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
  spatial_var = NULL,
  spatial_model = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  data,
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
  measurement_error = NULL,
  control = list(),
  chains = 4L,
  iter = 4000L,
  warmup = floor(iter/2),
  cores = 1L,
  sample_prior = "no",
  sre = NULL,
  sre_type = NULL,
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
  `~ (1|group1) + (1|group2)`. If `NULL` (default) *and* `spatial_model`
  is also `NULL`, the function emits a warning recommending an
  area-level random effect, since a Fay-Herriot SAE model with neither
  IID nor spatial area effects degenerates to fixed-effects regression
  and does not borrow strength across areas. The warning can be silenced
  with [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) if a
  fixed-effects-only baseline is intentional.

- spatial_var:

  Character. Name of the column in `data` that identifies the spatial
  areas (e.g. `"regency"` or `"province"`). Must be supplied *together
  with* `spatial_model` and `M`; providing only one of them is an error.
  Distinct from `re`: `re` is a formula for IID random effects, whereas
  `spatial_var` is a column name (string) for the spatially-structured
  random effect.

- spatial_model:

  Character. Type of spatial model: `"car"` (Conditional Autoregressive)
  or `"sar"` (Simultaneous Autoregressive). Must be supplied *together
  with* `spatial_var` and `M`; providing only one of them is an error.

- car_type:

  Character. CAR subtype passed to `brms`: one of `"escar"` (exact
  sparse CAR), `"esicar"` (exact sparse intrinsic CAR), `"icar"`
  (intrinsic CAR), or `"bym2"`. Defaults to `"icar"` when
  `spatial_model = "car"`.

- sar_type:

  Character. SAR subtype: `"lag"` (SAR of the response values) or
  `"error"` (SAR of the residuals). Defaults to `"lag"` when
  `spatial_model = "sar"`.

- M:

  Spatial matrix supplied as `data2` to `brms`. For CAR this must be a
  binary adjacency matrix; for SAR a spatial weight matrix. Row names
  must match the levels of `spatial_var`.

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

- sampling_variance:

  Optional character. Name of a column in `data` containing the
  **known** sampling variance \\D_i\\ for each area (the Fay-Herriot
  sugar). When supplied, \\\sigma_i = \sqrt{D_i}\\ is pinned via offset.
  This is the canonical way to fit a Gaussian Fay-Herriot model: without
  it, the residual \\\sigma\\ and the area-RE \\\sigma_u\\ compete to
  explain the same variance and the model is unidentified, typically
  producing divergent transitions. Equivalent to
  `fixed_params = list(sigma = sqrt(data[[<col>]]))`.

  **Family compatibility.** `sampling_variance` requires a continuous
  family whose response distribution has a residual scale parameter
  named `sigma`. Supported: `gaussian` (the canonical Fay-Herriot case),
  `lognormal` (D must be on the log scale; see also
  [`hbm_lnln`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md)),
  `student`, `skew_normal`, `exgaussian`, `asym_laplace`. A helpful
  error is thrown when an incompatible family is supplied.

  **For non-Gaussian SAE families** the analogous pinning mechanism is
  family-specific:

  - **Beta**: pin the precision `phi` via the survey design effect,
    e.g.\\ `fixed_params = list(phi = ~ I(n/deff - 1))` (Liu 2009). See
    [`hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md).

  - **Binomial**: sampling variability enters through the `trials`
    addition term, not through a separate `sigma`. See
    [`hbm_binlogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md).

  - **Poisson, Gamma, Weibull**: variance is tied algebraically to the
    mean – no separate scale parameter to pin.

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
      `hs_scale_slab`, `hs_par_ratio`, and `hs_autoscale`.

  `"r2d2"`

  :   R2D2 prior (Zhang et al. 2022). Places a prior directly on the
      model \\R^2\\ and distributes explained variance across predictors
      via a Dirichlet decomposition. Controlled by `r2d2_mean_R2`,
      `r2d2_prec_R2`, `r2d2_cons_D2`, and `r2d2_autoscale`.

  If `prior` already contains a global `class = "b"` entry, `prior_type`
  is ignored and a warning is issued.

  **Cascading to smooth and GP terms.** When the formula contains
  [`s()`](https://paulbuerkner.com/brms/reference/s.html) or
  [`gp()`](https://paulbuerkner.com/brms/reference/gp.html) terms, the
  shrinkage prior is automatically extended to the corresponding
  parameter classes `"sds"` (spline SDs) and/or `"sdgp"` (GP SDs) using
  the brms-canonical `main = TRUE` pattern. The resulting prior
  regularises ALL components jointly – linear coefficients, nonlinear
  smooth wiggliness, and GP marginal variance – which is the principled
  approach to global-local shrinkage in models that combine parametric
  and nonparametric components.

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
  of predictors via `hs_autoscale`.

- hs_scale_slab:

  Numeric \\\> 0\\. Scale for the slab component (default `2`).

- hs_par_ratio:

  Numeric \\\> 0\\ or `NULL`. Expected ratio of non-zero to total
  coefficients. `NULL` (default) treats all coefficients as potentially
  non-zero.

- hs_autoscale:

  Logical. Whether `brms` should auto-scale the horseshoe prior using
  the residual SD \\\sigma\\. Default `TRUE`; set to `FALSE` for
  non-continuous responses (binomial, Poisson, ...) where \\\sigma\\ is
  not defined.

- r2d2_mean_R2:

  Numeric in \\(0, 1)\\. Prior mean of the model \\R^2\\ (default
  `0.5`).

- r2d2_prec_R2:

  Numeric \\\> 0\\. Prior precision of \\R^2\\ (default `2`). Higher
  values concentrate mass around `r2d2_mean_R2`.

- r2d2_cons_D2:

  Numeric \\\> 0\\ or `NULL`. Dirichlet concentration for the D2
  component. `NULL` (default) uses the `brms` default `0.5`.

- r2d2_autoscale:

  Logical. Whether `brms` should auto-scale the R2D2 prior using
  \\\sigma\\. Default `TRUE`; set to `FALSE` for non-continuous
  responses.

- nonlinear:

  Character vector or `NULL`. Names of predictor variables to model with
  a smooth nonlinear term. Each listed variable is replaced in the
  formula RHS with `s(var)` (spline) or `gp(var)` (Gaussian process).
  Variables not listed remain linear. Do *not* also write `s(x)` in the
  formula when using this argument – the modification is applied
  automatically. Default `NULL` (all predictors remain linear).

- nonlinear_type:

  Character. Smooth term family to use. One of `"spline"` (default,
  penalised regression spline via
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html)) or `"gp"`
  (Gaussian process via
  [`brms::gp()`](https://paulbuerkner.com/brms/reference/gp.html)).

- spline_k:

  Integer. Spline basis dimension (number of knots) passed to
  `mgcv::s(..., k = ...)`. `-1L` (default) lets mgcv choose
  automatically. For SAE typically `k = 8` to `15`. Ignored when
  `nonlinear_type = "gp"`.

- spline_bs:

  Character. Spline basis type passed to `mgcv::s(..., bs = ...)`.
  Default `"tp"` (thin-plate regression spline, the mgcv default).
  Common alternatives: `"cr"` (cubic regression spline; often more
  stable for SAE with correlated auxiliary variables), `"cs"` (cubic
  with shrinkage; allows variable selection), `"ps"` (P-splines).
  Ignored when `nonlinear_type = "gp"`.

- gp_k:

  Integer or `NA`. Number of basis functions for the Hilbert-space
  approximate GP (Riutort-Mayol et al.\\ 2020), passed to
  `brms::gp(..., k = ...)`. `NA` (default) = exact GP which scales
  \\O(n^3)\\ and is **not recommended for** \\n \> 100\\ areas; an
  immediate warning is emitted in that case pointing to this argument.
  Integer values `10`–`25` are typical for SAE and dramatically improve
  convergence and runtime. Ignored when `nonlinear_type = "spline"`.

- gp_cov:

  Character. GP covariance function passed to
  `brms::gp(..., cov = ...)`: `"exp_quad"` (squared exponential / RBF,
  default), `"matern15"` (Matern 3/2), `"matern25"` (Matern 5/2; often
  more numerically stable for SAE than RBF), or `"exponential"`.

- gp_c:

  Numeric \\\> 0\\ or `NULL`. Hilbert-space GP boundary-scale factor
  passed to `brms::gp(..., c = ...)`. Default brms value is \\5/4\\ (=
  1.25); increase if the GP appears truncated at the domain boundaries.
  Only relevant when `gp_k` is supplied.

- gp_scale:

  **Deprecated.** Use `gp_c` instead. The old name suggested a
  length-scale interpretation but actually mapped to the HSGP
  boundary-scale factor. Will be removed in v2.0.0.

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

- measurement_error:

  Optional named list specifying which auxiliary variables are measured
  with error and where to find their standard errors. The list maps
  variable names to columns in `data` containing the SE, e.g.
  `measurement_error = list(x1 = "se_x1", x2 = "se_x2")`. Listed
  variables are wrapped on-the-fly with `mi(var, se_col)` in the
  brmsformula so that brms treats them as latent variables with a
  Gaussian measurement-error structure, following Ybarra and Lohr
  (2008). Standard errors must be non-negative and have no missing
  values; `measurement_error` variables must be a subset of the model's
  auxiliary (linear) predictors. When the user has already written
  `mi(...)` explicitly in the formula, the corresponding entries in
  `measurement_error` are detected and not duplicated. Note that ME
  inflates the parameter space (each *x_i* becomes latent), which slows
  sampling and increases the risk of divergent transitions, especially
  when combined with smooth terms (`nonlinear`). See the "Measurement
  error" section of the SAE vignette.

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

- sre:

  **Deprecated.** Use `spatial_var` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- sre_type:

  **Deprecated.** Use `spatial_model` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

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

  Specified by `spatial_model = "car"`. The joint distribution of the
  spatial effects is \$\$u \sim \mathcal{N}\bigl(0,\\ \sigma_u^2 (D -
  \rho W)^{-1}\bigr),\$\$ where \\W\\ is a binary adjacency matrix (1 if
  neighbour, 0 otherwise) and \\D = \mathrm{diag}(W \mathbf{1})\\.
  Sub-types via `car_type`: `"icar"` (intrinsic, \\\rho = 1\\; Besag
  1991); `"escar"`, `"esicar"` (exact sparse formulations of Morris et
  al.\\ 2019); `"bym2"` (BYM2 reparameterisation of Riebler et al.\\
  2016, recommended for disconnected graphs).

- **SAR (Simultaneous Autoregressive; Whittle 1954, Anselin 1988)**:

  Specified by `spatial_model = "sar"`. The model is \$\$u = \rho W u +
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

## How to pin distributional parameters per family

hbsaems exposes three layers for pinning distributional parameters to
known values, in increasing order of generality:

1.  **Family-specific sugar** (least typing, most readable). Each
    wrapper exposes a convenience argument that maps to a well-defined
    Fay-Herriot-style transformation of survey design quantities:

    |  |  |  |
    |----|----|----|
    | **Wrapper** | **Sugar argument** | **Pinned dpar** |
    | `hbm(..., hb_sampling = "gaussian")` | `sampling_variance = "D"` | \\\sigma_i = \sqrt{D_i}\\ |
    | [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md) | `sampling_variance = "psi"` | \\\sigma_i = \sqrt{\psi_i}\\ (on log scale) |
    | [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md) | `n = "n", deff = "deff"` | \\\phi_i = n_i / \mathrm{deff}\_i - 1\\ (Liu 2009) |
    | [`hbm_binlogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md) | `trials = "n"` | (sampling variance built into the family) |

2.  **Universal `fixed_params`** (works everywhere). A named list
    pinning any distributional parameter – accepts a column name
    (character), a scalar (numeric of length 1), a vector of length
    `nrow(data)`, or a one-sided formula (evaluated in `data`'s
    environment). Examples:

    - `fixed_params = list(sigma = "D")` – pin sigma to a column.

    - `fixed_params = list(phi = ~ I(n / deff - 1))` – pin phi via
      formula.

    - `fixed_params = list(nu = 4)` – pin Student-t df to a scalar.

3.  **Raw `stanvars`** (for power users authoring custom Stan code).
    Direct injection of Stan code blocks – see
    [`stanvar`](https://paulbuerkner.com/brms/reference/stanvar.html).

Sugar arguments are simply thin wrappers around layer 2: they validate
the survey-design inputs (no NAs, strictly positive) and translate to
`fixed_params` before delegating to the universal machinery. Hence the
conflict policy below applies uniformly to both sugar and explicit
`fixed_params`.

## Conflict resolution between prior, prior_type, fixed_params, and stanvars

`hbm()` provides four orthogonal mechanisms to influence the prior /
parameter specification of the underlying brms model:

1.  `prior` – explicit `brmsprior` object(s).

2.  `prior_type` – global-local shrinkage prior on the regression
    coefficients (cascades to `"sds"` / `"sdgp"` when splines or GPs are
    present).

3.  `fixed_params` – pin distributional parameters to known values via
    the offset trick.

4.  `stanvars` – inject custom Stan code blocks.

Plus two family-specific *sugar* arguments that translate to
`fixed_params` internally:

- `sampling_variance` (continuous families): pins \\\sigma_i =
  \sqrt{D_i}\\, equivalent to `fixed_params$sigma = sqrt(data$D)`.

- `n + deff` (`hbm_betalogitnorm`): pins \\\phi_i = n_i /
  \mathrm{deff}\_i - 1\\, equivalent to
  `fixed_params$phi = n / deff - 1`.

Combining these without rules in mind can produce unidentified models or
compile-time errors. `hbm()` therefore enforces the following **conflict
matrix**, where each cell describes what happens when the row and column
inputs both target the same distributional parameter (e.g.\\ both pin
`sigma`):

|  |  |  |  |  |
|----|----|----|----|----|
|  | **fixed_params** | **prior** | **prior_type** | **stanvars** |
| **sampling_variance** | error | error (transitive) | no overlap | error (transitive) |
| **n + deff** | error | error (transitive) | no overlap | error (transitive) |
| **fixed_params** | – | error (10b.i) | no overlap | error (10b.ii) |
| **prior** | error (10b.i) | – | warning, user wins | no check needed |
| **prior_type** | no overlap | warning, user wins | – | no check needed |
| **stanvars** | error (10b.ii) | no check needed | no check needed | – |

Resolution semantics in detail:

- **`prior` vs `prior_type`.** If the user supplies a *global* (no
  `coef =`) prior on `class = "b"`, `"sds"`, or `"sdgp"`, `prior_type`
  is silently dropped for that class and a warning is emitted.
  Coefficient-specific user priors (`coef = "x1"`) are kept alongside
  the shrinkage prior without warning.

- **`fixed_params` vs `prior`.** A pinned parameter is removed from the
  sampler; supplying a prior on that same parameter therefore has no
  effect and is treated as a user error – an informative
  [`stop()`](https://rdrr.io/r/base/stop.html) is issued.

- **`fixed_params` vs `stanvars`.** Same logic as above: a sampling
  statement in `stanvars` that targets a pinned parameter would fail at
  Stan compile time; `hbm()` catches this and stops with a clear
  message.

- **Sugar vs `fixed_params` on the same parameter.** The sugar
  translators (`.translate_sampling_variance()`,
  `.translate_n_deff_to_phi()`) error out if the user has also
  pre-populated `fixed_params` for the target parameter – there should
  never be two pin sources for the same dpar.

- **Sugar vs `prior` / `stanvars` transitively.** After the sugar -\>
  `fixed_params` translation, the downstream `fixed_params`-vs-prior /
  -stanvars checks fire automatically. E.g.\\ `sampling_variance = "D"`
  plus `prior = set_prior("normal(0, 1)", class = "sigma")` errors via
  the `fixed_params` vs `prior` rule.

The intent is to fail fast and explicitly rather than silently producing
an unidentified or mis-specified model.

## Convergence advice for SAE practitioners

Common convergence pathologies in hierarchical Bayesian SAE models and
how to address them. Run
[`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
after fitting to inspect \\\hat R\\, effective sample size (ESS), and
divergent transitions.

**1. Default sampler settings (recommended starting point).**

- `chains = 4`, `iter = 4000`, `warmup = 2000`

- `control = list(adapt_delta = 0.95, max_treedepth = 12)`

- `cores = parallel::detectCores() - 1`

**2. Divergent transitions.** Most common cause is the *funnel* geometry
of hierarchical variance parameters.

- First-line: increase `adapt_delta` to `0.99`.

- If still diverging, increase `warmup` and consider a tighter prior on
  the area-level standard deviation (`sd` class), e.g.\\
  `set_prior("normal(0, 0.5)", class = "sd")`.

- For Beta/Binomial logit-normal models, prior on the random intercept
  SD should also be on the logit scale.

- **Gaussian (Fay-Herriot) only.** Always supply the known sampling
  variance via `sampling_variance = "<col>"`. Without this, the residual
  \\\sigma\\ and the area-RE \\\sigma_u\\ compete to explain the same
  variance component, producing weak identifiability and divergent
  transitions almost regardless of `adapt_delta`. This is the single
  most common cause of divergences in Fay-Herriot fits and should be
  checked *first* before any sampler-tuning.

**3. Low effective sample size (ESS \< 1000).**

- Increase `iter` (e.g.\\ to `6000`); this is the single most reliable
  fix.

- Centre and scale the auxiliary variables before fitting.

- Check for prior–data conflict via priorsense; see
  [`?prior_check`](https://madsyair.github.io/hbsaems/reference/prior_check.md).

**4. Gaussian processes.**

- **Exact GP scales \\O(n^3)\\.** For \\n \> 100\\ areas, set `gp_k` to
  use the Hilbert-space approximate GP (Riutort-Mayol et al.\\ 2020). A
  heuristic is `gp_k = ceiling(min(n / 5, 25))`.

- Try `gp_cov = "matern25"` (Matern 5/2) if the default
  squared-exponential covariance is numerically unstable.

- The boundary-scale factor `gp_c` (brms default 1.25) may need
  increasing if the posterior GP is truncated at the domain edges.

**5. Splines.**

- Start with `spline_k = -1` (auto). Increase only if the residual
  diagnostics suggest under-smoothing.

- For strongly correlated auxiliary variables, try `spline_bs = "cr"`
  (cubic regression spline) for better numerical stability than the
  default thin-plate.

- For variable selection, use `spline_bs = "cs"` (cubic with shrinkage);
  coefficients on irrelevant smooths shrink toward zero.

**6. Spatial models.**

- For CAR, `car_type = "bym2"` (Riebler et al.\\ 2016) is the modern
  recommendation; it stabilises the spatial/IID decomposition via a
  single mixing parameter.

- Verify the weight matrix with
  [`check_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md);
  isolated areas or multiple disconnected components cause
  non-identifiability.

**7. Prior predictive check first.** Always call
[`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md)
(`sample_prior = "only"`) before the full posterior run. Implausible
prior predictives are the single most common cause of slow / divergent
sampling.

## References

Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*. John Wiley
& Sons.

Burkner, P. C. (2017). brms: An R package for Bayesian multilevel models
using Stan. *Journal of Statistical Software*, 80(1), 1–28.

Riutort-Mayol, G., Burkner, P.-C., Andersen, M. R., Solin, A., &
Vehtari, A. (2023). Practical Hilbert space approximate Bayesian
Gaussian processes for probabilistic programming. *Statistics and
Computing*, 33, 17.
[doi:10.1007/s11222-022-10167-2](https://doi.org/10.1007/s11222-022-10167-2)

Riebler, A., Sorbye, S. H., Simpson, D., & Rue, H. (2016). An intuitive
Bayesian spatial model for disease mapping that accounts for scaling.
*Statistical Methods in Medical Research*, 25(4), 1145–1165.

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
       re          = ~(1 | regency),
       data        = data),
  FAST
))
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 50 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.68, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + (1 | regency), pforms = list(),      pfix = list(), resp = "y", family = structure(list(family = "gaussian",          link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#> Warning: Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors.
#> Warning: There were 50 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.96      0.45     0.04     1.50 1.32        5      122
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.11      0.13     9.83    10.33 1.05       39     1018
#> x1            1.00      0.14     0.73     1.30 1.07      302      355
#> x2           -0.37      0.13    -0.64    -0.12 1.10      496      512
#> x3            0.34      0.14     0.04     0.59 1.07       23      651
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.78      0.53     0.02     1.54 1.68        3       23
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Horseshoe prior (sparse coefficients) ------------------------------------
model_hs <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       re          = ~(1 | regency),
       data        = data,
       prior_type  = "horseshoe",
       hs_df       = 1),
  FAST
))
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 4 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model_hs)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + (1 | regency), pforms = list(),      pfix = list(), resp = "y", family = structure(list(family = "gaussian",          link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#> Warning: There were 4 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.68      0.40     0.02     1.39 1.03       74       73
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.08      0.14     9.80    10.36 1.00      816      619
#> x1            0.97      0.15     0.68     1.24 1.00     1498     1638
#> x2           -0.33      0.15    -0.62    -0.04 1.00     1181      736
#> x3            0.27      0.15    -0.01     0.56 1.00     1130      601
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.17      0.27     0.56     1.58 1.04       76       65
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- R2D2 prior (prior on model R-squared) -------------------------------------
model_r2 <- do.call(hbm, c(
  list(formula      = bf(y ~ x1 + x2 + x3),
       re           = ~(1 | regency),
       data         = data,
       prior_type   = "r2d2",
       r2d2_mean_R2 = 0.5,
       r2d2_prec_R2 = 2),
  FAST
))
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 37 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.07, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model_r2)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + (1 | regency), pforms = list(),      pfix = list(), resp = "y", family = structure(list(family = "gaussian",          link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#> Warning: Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors.
#> Warning: There were 37 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.67      0.39     0.02     1.38 1.07       45       32
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.08      0.15     9.78    10.37 1.00      906      785
#> x1            0.96      0.15     0.68     1.26 1.00     1441     1447
#> x2           -0.32      0.15    -0.62    -0.02 1.01      779      746
#> x3            0.26      0.14    -0.01     0.54 1.00     1174      977
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.18      0.28     0.52     1.60 1.07       26       17
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Spline smooth for x1 (nonlinear) -----------------------------------------
# x1 is modelled with s(x1); x2 and x3 remain linear.
model_spline <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | regency),
       data           = data,
       nonlinear      = "x1",
       nonlinear_type = "spline"),
  FAST
))
#> Warning: Variable(s) x1 appear in both 'auxiliary' (linear) and 'nonlinear'. They will be modelled nonlinearly ONLY. Remove them from 'auxiliary' to suppress this warning.
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 31 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.17, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model_spline)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ s(x1) + x2 + x3 + (1 | regency),      pforms = list(), pfix = list(), resp = "y", family = structure(list(         family = "gaussian", link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#> Warning: Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors.
#> Warning: There were 31 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ s(x1) + x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Smoothing Spline Hyperparameters:
#>            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(sx1_1)     1.02      0.98     0.02     3.67 1.02      284      161
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.86      0.47     0.05     1.52 1.11       13      113
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     9.94      0.14     9.64    10.21 1.00      384      349
#> x2           -0.36      0.15    -0.64    -0.08 1.00      340      980
#> x3            0.33      0.16     0.03     0.63 1.01      293      492
#> sx1_1         7.06      2.80     1.97    14.17 1.01      378      340
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.97      0.43     0.07     1.56 1.17        9       11
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Gaussian process for x2 (nonlinear) --------------------------------------
model_gp <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | regency),
       data           = data,
       nonlinear      = "x2",
       nonlinear_type = "gp"),
  FAST
))
#> Warning: Variable(s) x2 appear in both 'auxiliary' (linear) and 'nonlinear'. They will be modelled nonlinearly ONLY. Remove them from 'auxiliary' to suppress this warning.
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 40 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model_gp)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + gp(x2) + x3 + (1 | regency),      pforms = list(), pfix = list(), resp = "y", family = structure(list(         family = "gaussian", link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#> Warning: There were 40 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + gp(x2) + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Gaussian Process Hyperparameters:
#>              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sdgp(gpx2)       0.73      0.50     0.11     2.09 1.00      389      487
#> lscale(gpx2)     0.19      0.52     0.01     0.97 1.01      182      447
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.64      0.37     0.03     1.29 1.03       93      203
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.17      0.48     9.24    11.21 1.00      740      623
#> x1            1.02      0.14     0.76     1.30 1.00      973     1146
#> x3            0.33      0.14     0.06     0.60 1.00      540      802
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.17      0.25     0.63     1.55 1.03       82      151
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Missing data: deletion (Y missing, X complete) ---------------------------
data_miss_y        <- data
data_miss_y$y[3:5] <- NA

model_deleted <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | regency),
       data           = data_miss_y,
       handle_missing = "deleted"),
  FAST
))
#> handle_missing = 'deleted': 3 row(s) with missing response variable removed from model fitting.
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 28 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model_deleted)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + (1 | regency), pforms = list(),      pfix = list(), resp = "y", family = structure(list(family = "gaussian",          link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#>  Missing data : deleted 
#> 
#> ----- Parameter Estimates -----
#> Warning: There were 28 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 97) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 97) 
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     0.65      0.38     0.02     1.31 1.01       76      244
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.13      0.14     9.85    10.40 1.00      817      666
#> x1            0.99      0.14     0.72     1.26 1.00      813      556
#> x2           -0.41      0.15    -0.70    -0.12 1.00      429      343
#> x3            0.29      0.15     0.02     0.60 1.01      422      520
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.16      0.25     0.64     1.54 1.00       74      101
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Missing data: multiple imputation (X only -- Y is never imputed) ----------
data_miss_x          <- data
data_miss_x$x1[6:8]  <- NA

model_multiple <- do.call(hbm, c(
  list(formula        = bf(y ~ x1 + x2 + x3),
       re             = ~(1 | regency),
       data           = data_miss_x,
       handle_missing = "multiple",
       m              = 5),
  FAST
))
#> Missing predictor variable(s): x1. Applying multiple imputation (mice) with m = 5 imputations.
#> Warning: Number of logged events: 2
#> Compiling the C++ model
#> Error in getGlobalsAndPackages(expr, envir = envir, globals = globals): The total size of the 7 globals exported for future expression (‘FUN(chains = 2, iter = 2000, warmup = 1000, cores = 1, control = list(),; save_pars = structure(list(group = TRUE, latent = FALSE,; all = TRUE, manual = character(0)), class = "save_pars"),; refresh = 0)’) is 1.96 GiB. This exceeds the maximum allowed size 500.00 MiB per plan() argument 'maxSizeOfObjects'. This limit is set to protect against transfering too large objects to parallel workers by mistake, which may not be intended and could be costly. See help("future.globals.maxSize", package = "future") for how to adjust or remove the default threshold via an R option The three largest globals are ‘FUN’ (0.98 GiB of class ‘function’), ‘fit’ (0.98 GiB of class ‘list’) and ‘data’ (52.70 KiB of class ‘list’)
summary(model_multiple)
#> Error: object 'model_multiple' not found

# -- Missing data: model-based imputation with mi() ---------------------------
data_miss_x2         <- data
data_miss_x2$x1[6:7] <- NA

model_model <- do.call(hbm, c(
  list(formula        = bf(y | mi() ~ mi(x1) + x2 + x3) +
                        bf(x1 | mi() ~ x2 + x3),
       re             = ~(1 | regency),
       data           = data_miss_x2,
       handle_missing = "model"),
  FAST
))
#> handle_missing = 'model': using mi() specification for joint model-based imputation.
#> Setting 'rescor' to FALSE by default for this model
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 91 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.07, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
summary(model_model)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : (link: )
#>  Formula      : structure(list(forms = list(y = structure(list(formula = y |      mi() ~ mi(x1) + x2 + x3 + (1 | regency), pforms = list(),      pfix = list(), resp = "y", family = structure(list(family = "gaussian",          link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )), x1 = structure(list(formula = x1 | mi() ~ x2 + x3 + (1 |      regency), pforms = list(), pfix = list(), resp = "x1", family = structure(list(     family = "gaussian", link = "identity", linkfun = function (mu)      link(mu, link = slink), linkinv = function (eta)      inv_link(eta, link = slink), dpars = c("mu", "sigma"), type = "real",      ybounds = c(-Inf, Inf), closed = c(NA, NA), ad = c("weights",      "subset", "se", "cens", "trunc", "mi", "index"), normalized = c("_time_hom",      "_time_het", "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",      "rescor")), class = c("brmsfamily", "family")), mecor = TRUE), class = c("brmsformula",  "bform"))), responses = c("y", "x1"), rescor = FALSE, mecor = TRUE), class = c("mvbrmsformula",  "bform")) 
#>  Missing data : model 
#> 
#> ----- Parameter Estimates -----
#> Warning: Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors.
#> Warning: There were 91 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: MV(gaussian, gaussian) 
#>   Links: mu = identity
#>          mu = identity 
#> Formula: y | mi() ~ mi(x1) + x2 + x3 + (1 | regency) 
#>          x1 | mi() ~ x2 + x3 + (1 | regency) 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Multilevel Hyperparameters:
#> ~regency (Number of levels: 100) 
#>                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(y_Intercept)      0.87      0.45     0.05     1.52 1.04       23       97
#> sd(x1_Intercept)     0.52      0.28     0.03     0.99 1.03       65       79
#> 
#> Regression Coefficients:
#>              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> y_Intercept     10.10      0.14     9.83    10.38 1.01      959     1211
#> x1_Intercept    -0.16      0.10    -0.37     0.04 1.01     1430     1199
#> y_x2            -0.39      0.14    -0.66    -0.11 1.00      825      977
#> y_x3             0.31      0.14     0.05     0.60 1.00     1155     1235
#> x1_x2           -0.05      0.10    -0.26     0.15 1.03       54       46
#> x1_x3           -0.02      0.11    -0.21     0.19 1.02      213      683
#> y_mix1           0.97      0.15     0.70     1.26 1.02      145      775
#> 
#> Further Distributional Parameters:
#>          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma_y      0.97      0.41     0.13     1.53 1.05       19       13
#> sigma_x1     0.81      0.21     0.33     1.10 1.01       60       43
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Spatial: CAR (Conditional Autoregressive) --------------------------------
data("adjacency_matrix_car")
model_car <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       data        = data,
       spatial_var = "province",
       spatial_model    = "car",
       M           = adjacency_matrix_car),
  FAST
))
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 906 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: Examine the pairs() plot to diagnose sampling problems
summary(model_car)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + car(M, gr = province,      type = "icar"), pforms = list(), pfix = list(), resp = "y",      family = structure(list(family = "gaussian", link = "identity",          linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + x2 + x3 + car(M, gr = province, type = "icar") 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Correlation Structures:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sdcar     0.69      0.45     0.10     1.79 1.00      445      496
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.09      0.14     9.80    10.36 1.00     2045     1246
#> x1            0.94      0.14     0.68     1.21 1.00     1625     1158
#> x2           -0.38      0.14    -0.65    -0.10 1.00     1598     1281
#> x3            0.32      0.14     0.06     0.58 1.00     1776     1247
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.38      0.10     1.19     1.61 1.00     1297     1096
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# -- Spatial: SAR (Simultaneous Autoregressive) -------------------------------
# spatial_weight_sar is a 100x100 row-standardised matrix with row-
# names regency_001..regency_100, so it pairs with the fine-grained
# "regency" column (100 levels) -- NOT with "province" (5 levels).
data("spatial_weight_sar")
model_sar <- do.call(hbm, c(
  list(formula     = bf(y ~ x1 + x2 + x3),
       data        = data,
       spatial_var = "regency",
       spatial_model    = "sar",
       M           = spatial_weight_sar),
  FAST
))
#> Compiling Stan program...
#> Start sampling
summary(model_sar)
#> 
#> ===== Hierarchical Bayesian Model Summary =====
#> 
#>  Observations : 100 
#>  Family       : gaussian (link: identity )
#>  Formula      : structure(list(formula = y ~ x1 + x2 + x3 + sar(M, type = "lag"),      pforms = list(), pfix = list(), resp = "y", family = structure(list(         family = "gaussian", link = "identity", linkfun = function (mu)          link(mu, link = slink), linkinv = function (eta)          inv_link(eta, link = slink), dpars = c("mu", "sigma"),          type = "real", ybounds = c(-Inf, Inf), closed = c(NA,          NA), ad = c("weights", "subset", "se", "cens", "trunc",          "mi", "index"), normalized = c("_time_hom", "_time_het",          "_lagsar", "_errorsar", "_fcor"), specials = c("residuals",          "rescor"), link_sigma = "log"), class = c("brmsfamily",      "family")), mecor = TRUE), class = c("brmsformula", "bform" )) 
#> 
#> ----- Parameter Estimates -----
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: y ~ x1 + x2 + x3 + sar(M, type = "lag") 
#>    Data: structure(list(y = c(6.98224870350365, 9.005801436 (Number of observations: 100) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Correlation Structures:
#>        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> lagsar     0.05      0.08    -0.12     0.16 1.00     2052     1272
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    10.03      0.17     9.69    10.37 1.00     2463     1494
#> x1            0.99      0.14     0.72     1.26 1.00     3041     1583
#> x2           -0.38      0.14    -0.66    -0.10 1.00     2774     1415
#> x3            0.32      0.14     0.04     0.59 1.00     2697     1390
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.42      0.11     1.23     1.64 1.00     2573     1356
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
# }
```
