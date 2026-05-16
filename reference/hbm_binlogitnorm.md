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
  fixed_params = NULL,
  predictors = NULL,
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

- fixed_params:

  Optional named list pinning distributional parameters to known values.
  See [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  the spec format.

- predictors:

  **Deprecated.** Use `auxiliary` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

- ...:

  Additional arguments forwarded to
  [`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).
  See
  [`?hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).

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
  group      = "group",            # area-level random effect
  data       = data_binlogitnorm,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')

# -- 2. With spatial CAR random effect -------------------------------------
data("adjacency_matrix_car")
fit_car <- hbm_binlogitnorm(
  response   = "y",
  trials     = "n",
  auxiliary  = c("x1", "x2", "x3"),
  sre        = "sre",
  sre_type   = "car",
  M          = adjacency_matrix_car,
  data       = data_binlogitnorm,
  chains = 1, iter = 500, warmup = 250, refresh = 0
)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```
