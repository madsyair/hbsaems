# Small Area Estimation under a Binomial Logit-Normal Model

Convenience wrapper that fits a Hierarchical Bayesian Small Area
Estimation model with a **binomial** likelihood and logit link.
Internally delegates to
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
with `family_key = "binomial"`.

## Usage

``` r
hbm_binlogitnorm(
  response,
  trials,
  auxiliary = NULL,
  data,
  area_var = NULL,
  area_re_structure = c("nested", "crossed"),
  spatial_var = NULL,
  spatial_model = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
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

  Character. Name of the successes variable (non-negative integer, \\y
  \le n\\).

- trials:

  Character. Name of the trials variable (positive integer).

- auxiliary:

  Character vector of auxiliary (fixed-effect) variable names;
  corresponds to area-level covariates in SAE literature (see Rao &
  Molina 2015 Ch. 4).

- data:

  A `data.frame`.

- area_var:

  Optional character vector. Name(s) of column(s) in `data` identifying
  the small area / domain. Length 1 fits an IID area-level random
  intercept `(1 | area_var)`; length \\\geq\\ 2 supports hierarchical
  areas – see
  [`?hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  for the nested vs.\\ crossed structures. Default: `NULL`.

- area_re_structure:

  Either `"nested"` (default) or `"crossed"`; controls how multiple area
  columns combine.

- spatial_var:

  Optional character. Name of a column identifying the spatial cluster.
  Must be supplied together with `spatial_model` and `M`. Default:
  `NULL`.

- spatial_model:

  Optional character. Spatial dependence: `"car"` (default
  `car_type = "icar"`) or `"sar"` (default `sar_type = "lag"`). Default:
  `NULL`.

- car_type:

  Optional character. CAR sub-type passed to brms: `"escar"`,
  `"esicar"`, `"icar"`, or `"bym2"`.

- sar_type:

  Optional character. SAR sub-type: `"lag"` or `"error"`.

- M:

  Optional numeric matrix. Spatial weight matrix. Required when
  `spatial_model` is supplied.

- fixed_params:

  Optional named list pinning distributional parameters to known values.
  See [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  the spec format.

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
  [`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  (e.g.\\ `prior_type`, `handle_missing`, sampler controls).

## Value

An object of class `hbmfit`.

## Details

Let \\y_i\\ denote the number of successes in area \\i\\ out of \\n_i\\
trials. The model is \$\$y_i \mid p_i, n_i \sim \mathrm{Binomial}(n_i,
p_i),\$\$ \$\$\mathrm{logit}(p_i) = x_i^\top \boldsymbol{\beta} + u_i,
\quad u_i \sim \mathcal{N}(0, \sigma_v^2).\$\$

## Conflict policy

- `auxiliary` *and* the deprecated `predictors` in the same call are
  rejected with an informative error.

- `handle_missing = "model"` is not supported (binomial is a discrete
  family; see *Notes on missing data*).

## Notes on missing data

The binomial family does not support `handle_missing = "model"` (joint
Bayesian imputation via
[`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html)). When
NA values are detected and `handle_missing` is left `NULL`, the wrapper
auto-selects `"multiple"` (multiple imputation via mice on the
predictors only).

## See also

[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_binlogitnorm")

# -- 1. Standard binomial logit-normal SAE ---------------------------------
fit <- hbm_binlogitnorm(
  response   = "y",
  trials     = "n",
  auxiliary  = c("x1", "x2", "x3"),
  area_var   = "district",        # area-level random effect (1 | district)
  data       = data_binlogitnorm,
  chains = 4, iter = 2000, warmup = 1000, refresh = 0
)
#> Warning: Area column 'district' has 100 unique levels for 100 rows -- looks more like a continuous covariate than a grouping factor. Did you mean to put this in `auxiliary` instead?
#> Compiling Stan program...
#> Start sampling

# -- 2. With spatial CAR random effect -------------------------------------
data("adjacency_matrix_car_regency")
fit_car <- hbm_binlogitnorm(
  response      = "y",
  trials        = "n",
  auxiliary     = c("x1", "x2", "x3"),
  spatial_var   = "regency",
  spatial_model = "car",
  M             = adjacency_matrix_car_regency,
  data          = data_binlogitnorm,
  chains = 4, iter = 2000, warmup = 1000, refresh = 0
)
#> Compiling Stan program...
#> Start sampling
# }
```
