# Migration Guide: legacy v0.1.x names to v1.0.0

## What changed in v1.1.0

**Behaviour change you must be aware of when upgrading from v1.0.x.**

- **[`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  now defaults to `predict_type = "epred"`.** It summarises the
  posterior of the area mean rather than the posterior predictive of a
  new observation . This is the correct small-area-estimation target, so
  the reported `SD` / `RSE_percent` are now **smaller** than in v1.0.x
  (they no longer include observation-level likelihood variance).
  **Every RSE number your existing scripts produce will change.** To
  reproduce the old values exactly:

``` r

# v1.0.x behaviour (posterior predictive of a new observation)
est <- sae_predict(model, predict_type = "response")
```

- **[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md)
  default `adapt_delta` is now `0.95`** (was brms’s `0.8`). Hierarchical
  SAE funnels need the higher value to avoid spurious divergences. Any
  `control` value you set yourself is still respected.
- **[`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
  /
  [`model_compare_all()`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md)**
  now check Pareto-k for PSIS-LOO reliability and the previously inert
  `reloo_args` argument is functional – supplying it triggers
  [`brms::reloo()`](https://paulbuerkner.com/brms/reference/reloo.brmsfit.html)
  on high-k folds.
- **[`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)**
  now reports divergent transitions, E-BFMI, and the max-treedepth hit
  rate as numbers in [`summary()`](https://rdrr.io/r/base/summary.html).

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
              re                = ~ (1 | regency),
              sampling_variance = "D",                 # Fay-Herriot identifiability
              control           = list(adapt_delta = 0.99),
              chains = 4, iter = 4000)
model2 <- hbm(brms::bf(y ~ x1 + x2),      data = data_fhnorm,
              re                = ~ (1 | regency),
              sampling_variance = "D",
              control           = list(adapt_delta = 0.99),
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

------------------------------------------------------------------------

## Additional v1.0.0 features (consolidated)

The following items round out the v1.0.0 release. They are listed
separately because they were introduced after the initial section above
had stabilised.

### 1. `sae_benchmark()`: explicit `target_type`

**Earlier development versions:** when `weights = NULL` the default was
`rep(1/n, n)`, which silently assumed `target` was a population *mean*.
If users passed a population total, the resulting benchmarked estimates
were scale-corrupt by a factor of $`n`$.

**Current v1.0.0 behaviour:** a new argument
`target_type = c("total", "mean")` chooses a safe default. The default
is `"total"` (the BPS convention), so `weights = NULL` now means
`rep(1, n)`.

``` r

# OLD code (silently used 1/n; corrupt if target is a total)
sae_benchmark(predictions, target = 1e6)

# NEW (explicit, correct)
sae_benchmark(predictions, target = 1e6, target_type = "total")
# or — recommended for production —
sae_benchmark(predictions, target = 1e6, weights = popsize_per_area)
```

If your v1.0.0 code passed a population *mean* without explicit weights,
add `target_type = "mean"` to retain the old default.

### 2. `hbm_betalogitnorm()`: `link_phi` resolves automatically

**Earlier development versions:** default was `link_phi = "identity"`,
which is the correct choice when `phi` is pinned via the survey design
(`n + deff` or `fixed_params$phi`), but unsafe when `phi` is estimated.

**Current v1.0.0 behaviour:** default is now `NULL` and resolves
automatically:

- `link_phi = "identity"` when `phi` is pinned (fixed mode).
- `link_phi = "log"` otherwise (random / hyperprior mode – the brms
  default).

User-supplied `"identity"` in random mode emits a clear warning about
the risk of divergent transitions.

### 2b. `hbm_betalogitnorm()`: phi prior simplified to brms default

**Earlier development versions:** in random mode, `phi` was given a
hierarchical hyperprior:

    phi   ~ gamma(alpha, beta)
    alpha ~ gamma(1, 1)              (Stan parameter, real<lower=1>)
    beta  ~ gamma(1, 1)              (Stan parameter, real<lower=0>)

The wrapper declared `alpha` and `beta` as Stan parameters and
auto-injected the hyperprior sampling statements via `stanvars`.

**Current v1.0.0 behaviour:** `phi` uses brms’s own default prior
$`\mathrm{Gamma}(0.01, 0.01)`$ (lower bound 0). The wrapper no longer
declares `alpha` or `beta`, and no longer injects any `stanvars`
sampling statements for them.

**Why:** the prior on `alpha` (declared as `real<lower=1>` in Stan) was
on the boundary of its support, routinely producing divergent
transitions on weakly-informative data. The simpler brms default also
means `prior = NULL` does exactly what users expect.

**Migration:** legacy code that still passes `stanvars` containing
sampling statements on `alpha` / `beta` to
[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
now raises an informative error pointing users at the new mechanism
(`prior = brms::set_prior("gamma(...)", class = "phi")`). If you need to
reproduce the exact pre-v1.0.0 hierarchical model (e.g. to verify
against an earlier analysis), drop down to the universal
[`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
interface, which doesn’t apply the
[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
guard and accepts arbitrary user-declared Stan parameters. See
`vignette("hbsaems-betalogitnorm-model")` for the side-by-side code
listing.

### 2c. CRITICAL bug fix: `sampling_variance` no longer silently corrupted

**Earlier development versions:**
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) with
`sampling_variance = "D"` stored $`\sqrt{D_i}`$ in a hidden offset
column and attached `sigma ~ 0 + offset(.hbsaems_sigma_fixed)` to the
formula. However, brms applies the dpar’s link function (default
`link_sigma = "log"` for Gaussian / Lognormal / Student / etc.) to the
linear predictor before plugging it into the likelihood – so the Stan
model actually computed $`\sigma_i = \exp(\sqrt{D_i})`$ instead of
$`\sqrt{D_i}`$. E.g.  
$`D_i = 4`$ should have given $`\sigma_i = 2.0`$ but produced
$`\sigma_i \approx 7.39`$ – a catastrophic miscalibration. Same bug
affected `fixed_params = list(sigma = ...)`.

**Current v1.0.0 behaviour:**
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) now
overrides `link_<par> = "identity"` on the family object for every
pinned distributional parameter, so offset values are passed through
verbatim. No user action required – existing code automatically benefits
from the fix. This is the single most important behavioural change in
v1.0.0; if you have been fitting Fay-Herriot models with
`sampling_variance` on an earlier hbsaems development version, your
estimates were biased and we strongly recommend refitting.

### 3. `sae_scale()`: zero-variance predictions no longer corrupt downstream data

**Earlier development versions:** when all area predictions were
identical (e.g. in a toy fit),
[`base::scale()`](https://rdrr.io/r/base/scale.html) produced `NaN`
throughout, silently corrupting `result_table`.

**Current v1.0.0 behaviour:** detects this case, emits a warning, and
returns centred-only (or raw) predictions instead of `NaN`.

### 4. NEW: `measurement_error` argument (Ybarra-Lohr 2008)

When auxiliary covariates are themselves estimated from a survey and
come with a known sampling SE, you can now declare measurement error
without writing the brms
[`mi()`](https://paulbuerkner.com/brms/reference/mi.html) syntax by
hand:

``` r

fit <- hbm(
  formula = brms::bf(y ~ x1 + x2),
  data    = mydata,
  measurement_error = list(x1 = "se_x1"),   # x1 has known SE in se_x1 column
  sampling_variance = "D",                  # Fay-Herriot
  re      = ~ (1 | regency)
)
```

Internally
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) rewrites
the RHS to `mi(x1, se_x1) + x2` before delegating to brms. Caveat:
measurement-error models inflate the parameter space; expect 2-5x longer
sampling time and consider `adapt_delta = 0.99` if combining with smooth
terms.

### 5. NEW: automatic `mi() / me()` detection

If you have always written `mi(...)` explicitly in your formula,
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) no longer
requires you to set `handle_missing` – the call silently routes to
model-based imputation:

``` r

# Previously: had to spell out handle_missing
hbm(bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ z),
    data = d, handle_missing = "model")

# v1.0.0: handle_missing inferred from formula
hbm(bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ z),
    data = d)
```
