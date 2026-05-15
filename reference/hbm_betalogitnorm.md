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
  group = NULL,
  sre = NULL,
  sre_type = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  link_phi = "identity",
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
  pinned via offset. Default: `NULL` (treats `phi` as random with
  hyperprior).

- deff:

  Character or `NULL`. Name of the design-effect column. Required when
  `n` is supplied (and vice versa).

- group:

  Character or `NULL`. Name of the area-grouping variable; if supplied,
  adds `(1 | group)` as a random intercept.

- sre, sre_type, car_type, sar_type, M:

  Spatial random-effect arguments, forwarded to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

- link_phi:

  Character. Link function for `phi`; default `"identity"` when `phi` is
  pinned (offset must be on the identity scale).

- prior:

  Optional `brmsprior` object. If `NULL`, sensible defaults are filled
  in:

  - `Intercept ~ student_t(4, 0, 10)`

  - `b ~ student_t(4, 0, 2.5)`

  - `phi ~ gamma(alpha, beta)` (random mode only)

  The user may pass a partial prior: missing default classes are filled
  in automatically.

- stanvars:

  Optional `brmsstanvars` object specifying hyperpriors for `alpha` and
  `beta` when `phi` is random. If `NULL` (default),
  `alpha ~ gamma(1, 1)` and `beta ~ gamma(1, 1)` are used. The user may
  also supply custom
  [`stanvar()`](https://paulbuerkner.com/brms/reference/stanvar.html)
  expressions to override either or both hyperpriors, e.g.:


      stanvars = stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
                 stanvar(scode = "beta  ~ gamma(2, 3);", block = "model")

- handle_missing, m, control, chains, iter, warmup, cores, sample_prior,
  ...:

  Passed through to
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

- fixed_params:

  Optional named list pinning distributional parameters to known values.
  See [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  the spec format. Conflicts with `n` + `deff` on the `phi` parameter;
  use one or the other.

- predictors:

  **Deprecated.** Use `auxiliary` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

## Value

An object of class `hbmfit`.

## Details

The precision parameter \\\phi_i\\ can either be pinned to a survey
design effect (`n` + `deff`) or sampled with a hierarchical hyperprior
(\\\phi \sim \mathrm{Gamma}(\alpha, \beta)\\, \\\alpha \sim
\mathrm{Gamma}(1,1)\\, \\\beta \sim \mathrm{Gamma}(1,1)\\ by default).

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
  group      = "group",
  data       = data,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model1)
#> Error: object 'model1' not found

# -- 2. Fixed phi via survey design (n + deff) -------------------------------
model2 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  n          = "n",
  deff       = "deff",
  group      = "group",
  data       = data,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
summary(model2)
#> Error: object 'model2' not found

# -- 3. Custom hyperprior on alpha, beta -------------------------------------
model3 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  group      = "group",
  data       = data,
  stanvars   = stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
               stanvar(scode = "beta  ~ gamma(2, 3);", block = "model"),
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')

# -- 4. Spatial CAR model ----------------------------------------------------
data("adjacency_matrix_car")
model4 <- hbm_betalogitnorm(
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  n          = "n",
  deff       = "deff",
  sre        = "sre",
  sre_type   = "car",
  M          = adjacency_matrix_car,
  data       = data,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```
