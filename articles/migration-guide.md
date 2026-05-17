# Migration Guide: legacy v0.1.x names to v1.0.0

## What changed in v1.0.0

Function names have been modernised to follow the **brms** snake_case
convention. The original short-form names (`hbcc`, `hbmc`, `hbpc`,
`hbsae`) are now **deprecated** – they still work but produce a warning
once per session.

| Deprecated (old) | Replacement (new) |
|----|----|
| `hbcc(model)` | `convergence_check(model)` |
| `hbmc(model)` | `model_compare(model)` |
| `hbpc(model, data, response_var)` | `prior_check(model, data, response_var)` |
| `hbsae(model)` | `sae_predict(model)` |

The deprecated names will be **removed in v2.0.0**.

## Quick before/after

``` r

# Before (legacy v0.1.x names)
diag  <- hbcc(model)
comp  <- hbmc(model, model2)
pc    <- hbpc(model_prior, data, "y")
est   <- hbsae(model)

# After (v1.0.0 names)
diag  <- convergence_check(model)
comp  <- model_compare(model, model2)
pc    <- prior_check(model_prior, data = data, response_var = "y")
est   <- sae_predict(model)
```

## Bulk rename via sed

From a project root:

``` bash
sed -i "s/hbcc(/convergence_check(/g"  *.R
sed -i "s/hbmc(/model_compare(/g"      *.R
sed -i "s/hbpc(/prior_check(/g"        *.R
sed -i "s/hbsae(/sae_predict(/g"       *.R
```

## Argument renames in v1.0.0

In addition to function renames, several **argument names** have been
modernised in
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and the
wrapper functions
([`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md),
[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md),
[`hbm_binlogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md)).
The classical SAE literature describes the spatial random effect as the
“area” variable, not a generic “group”, so the new names spell that out:

| Old (deprecated) | New (canonical) | Type / scope |
|----|----|----|
| `sre = "area_col"` | `spatial_var = "area_col"` | Column name for spatial RE |
| `sre_type = "car"` | `spatial_model = "car"` | Type of spatial model |
| `group = "area_col"` | `area_var = "area_col"` | Column name for IID area RE (wrappers) |
| `predictors = c("x1")` | `auxiliary = c("x1")` | Auxiliary covariate names |
| `sampling_var = "psi_i"` | `sampling_variance = "psi_i"` | Column name of known sampling variance () |

The old names continue to work for one release cycle but emit a soft
deprecation warning. All are **scheduled for removal in v2.0.0**.

``` r

# Old (still works in v1.0.0, but emits a warning)
fit <- hbm(formula = bf(y ~ x1 + x2),
           sre     = "province",
           sre_type = "car",
           car_type = "bym2",
           M       = adjacency_matrix_car,
           data    = data_fhnorm)

# New (v1.0.0 canonical)
fit <- hbm(formula       = bf(y ~ x1 + x2),
           spatial_var   = "province",
           spatial_model = "car",
           car_type      = "bym2",
           M             = adjacency_matrix_car,
           data          = data_fhnorm)
```

For wrapper functions:

``` r

# Old (still works, emits warning)
fit <- hbm_lnln(response  = "y_obs",
                auxiliary = c("x1", "x2", "x3"),
                group     = "district",     # deprecated
                data      = data_lnln)

# New
fit <- hbm_lnln(response  = "y_obs",
                auxiliary = c("x1", "x2", "x3"),
                area_var  = "district",     # NEW
                data      = data_lnln)
```

## Dataset column renames in v1.0.0

The bundled toy datasets have been renamed to reflect concrete
Indonesian administrative levels, making it easier to recognise the SAE
roles of each column:

| Dataset              | Old columns    | New columns           |
|----------------------|----------------|-----------------------|
| `data_fhnorm`        | `group`, `sre` | `regency`, `province` |
| `data_betalogitnorm` | `group`, `sre` | `regency`, `province` |
| `data_binlogitnorm`  | `group`, `sre` | `district`, `regency` |
| `data_lnln`          | `group`, `sre` | `district`, `regency` |

The matrix `adjacency_matrix_car` was relabelled with row-/column-names
`province_01` .. `province_05`. A new companion matrix
`adjacency_matrix_car_regency` (with rownames `regency_01` ..
`regency_05`) ships alongside it for district-level datasets.

## New functionality in v1.0.0

``` r

library(hbsaems)
data("data_fhnorm")

model  <- hbm(brms::bf(y ~ x1 + x2 + x3), data = data_fhnorm,
              re = ~ (1 | regency),
              chains = 4, iter = 4000)
model2 <- hbm(brms::bf(y ~ x1 + x2),      data = data_fhnorm,
              re = ~ (1 | regency),
              chains = 4, iter = 4000)

# Quick convergence check
is_converged(model)
is_converged(model, threshold = 1.05)

# Full diagnostics
diag <- convergence_check(model)
summary(diag)
plot(diag, type = "trace")

# Multi-model ranking (analogous to loo_compare in brms)
tbl <- model_compare_all(full   = model,
                         medium = model2,
                         criterion = "both")
print(tbl)

# Bayesian model averaging
avg_est <- model_average(model, model2, weights = c(0.6, 0.4))

# Standard S3 methods now work on hbmfit
coef(model);  fixef(model);  nobs(model)
predict(model);  fitted(model)
update(model, iter = 8000)

# Posterior extraction
draws <- posterior_draws(model)
ci95  <- posterior_interval(model, prob = 0.95)

# Inspection helpers
hbm_info(model)
hbm_warnings(model)
```
