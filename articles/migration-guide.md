# Migration Guide: v0.2.x to v0.3.0

## What changed in v0.3.0

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

The deprecated names will be **removed in v1.0.0**.

## Quick before/after

``` r

# Before (v0.2.x)
diag  <- hbcc(model)
comp  <- hbmc(model, model2)
pc    <- hbpc(model_prior, data, "y")
est   <- hbsae(model)

# After (v0.3.0)
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

## New functionality in v0.3.0

``` r

library(hbsaems)
data("data_fhnorm")

model  <- hbm(brms::bf(y ~ x1 + x2 + x3), data = data_fhnorm,
              chains = 4, iter = 4000)
model2 <- hbm(brms::bf(y ~ x1 + x2),      data = data_fhnorm,
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
