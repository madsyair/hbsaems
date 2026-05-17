# Advanced Features: Spatial, Benchmarking, Custom Families, Smooth Terms

This vignette demonstrates the advanced features available in **hbsaems
v1.0.0**, complementing the `complete-workflow` vignette which focuses
on the core fitting pipeline.

Topics covered:

1.  **Pre-fit data quality checking** (`check_data`)
2.  **Spatial weight matrices** – building from shapefiles + theory
    checks
3.  **Multi-domain benchmarking** – kecamatan to kabupaten with raking
4.  **Fully Bayesian benchmarking** – correct uncertainty after
    adjustment
5.  **Smooth terms** – splines and Gaussian processes
6.  **Shrinkage priors** – horseshoe and R2D2 (with cascade)
7.  **Custom distributions** – the model registry
8.  **Model averaging with LOO weights** (`model_average`)
9.  **Prior sensitivity analysis** (`prior_sensitivity`)

``` r

library(hbsaems)
```

------------------------------------------------------------------------

## 1. Pre-fit data quality checking

Before fitting any model, run
[`check_data()`](https://madsyair.github.io/hbsaems/reference/check_data.md)
to verify the variables exist, identify missing patterns, and get a
recommendation for the `handle_missing` strategy.

``` r

data("data_fhnorm")

chk <- check_data(
  data       = data_fhnorm,
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  spatial_var = "province"
)

print(chk)
#>  HBSAE Data Check  [hbsaems_data_check]
#>  ---------------------------------------
#>   Observations    : 50
#>   Missing pattern : none (data complete)
#>   Issues          : none
#>
#>   - No missing values detected. handle_missing can stay NULL.
```

[`check_data()`](https://madsyair.github.io/hbsaems/reference/check_data.md)
returns an object of class `c("hbsaems_data_check", "hbsaems_check")`,
so you can test it with the generic predicate:

``` r

is.hbsaems_check(chk)   # TRUE
```

### When the response has missing values

Missing values in *Y* often correspond to **non-sample areas** – domains
for which you want a prediction but have no direct estimate.
[`check_data()`](https://madsyair.github.io/hbsaems/reference/check_data.md)
highlights this explicitly:

``` r

d2 <- data_fhnorm
d2$y[1:5] <- NA
chk2 <- check_data(d2, response = "y", auxiliary = c("x1", "x2", "x3"))
chk2$non_sample_warning
```

If those rows really are non-sample areas, **keep them**: fit the model
on the complete-Y subset and use `sae_predict(..., newdata = na_rows)`.

------------------------------------------------------------------------

## 2. Spatial weight matrices

### Building W from a shapefile

[`build_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
constructs a CAR-ready binary matrix or a SAR-ready row-standardised
matrix from a polygon shapefile:

``` r

# Queen contiguity + binary -> for CAR (Besag 1974)
M_car <- build_spatial_weight(
  shp       = "path/to/areas.shp",
  for_model = "car"         # implies type = "queen", style = "B"
)

# K-nearest + row-standardised -> for SAR (Anselin 1988)
M_sar <- build_spatial_weight(
  shp       = "path/to/areas.shp",
  for_model = "sar",        # implies type = "knn", style = "W"
  k         = 4
)
```

Distance-band and rook contiguity are also supported – see
[`?build_spatial_weight`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md).

### Theoretical compatibility check

[`check_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md)
runs five checks against the requirements of the chosen model class:

``` r

check_spatial_weight(M_car, spatial_model = "car")
#> Spatial Weight Matrix Diagnostic
#> ---------------------------------
#>   Square          : TRUE
#>   Zero diagonal   : TRUE
#>   Symmetric       : TRUE
#>   Detected style  : B
#>   Isolated areas  : 0
#>   Components      : 1
#>   Matrix is theoretically compatible.
```

This catches the common mistakes: non-symmetric matrices fed into CAR,
isolated areas under ICAR, etc. It also runs automatically inside
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) whenever
`spatial_model` is set.

------------------------------------------------------------------------

## 3. Multi-domain benchmarking: kecamatan to kabupaten

A common SAE scenario: you have estimates per kecamatan and need them to
roll up to a published kabupaten total from BPS.
[`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
with `method = "raking"` handles this:

``` r

# Suppose we have SAE estimates per kecamatan, with a kabupaten column
estimates <- sae_predict(fit_hbm)

# Official BPS totals
T_kab <- c(Bogor = 110, Sukabumi = 145, Cianjur = 145)

bm <- sae_benchmark(
  predictions = estimates,
  target      = T_kab,                  # one target per kabupaten
  weights     = data_fhnorm$populasi,   # population per kecamatan
  groups      = data_fhnorm$kabupaten,  # mapping kec -> kab
  method      = "raking"
)

bm$benchmark_info$converged    # raking with one group var: converges in 1 sweep
```

The adjustment factor is *constant within each kabupaten* but *different
across kabupaten* – exactly what multi-domain benchmarking requires.

### Unit consistency

The critical rule: `sum(weights * theta) == target`, with consistent
units. Two common pitfalls:

| Wrong                             | Right                                  |
|-----------------------------------|----------------------------------------|
| weights in raw count, target in % | both as totals OR both as means        |
| theta in proportion, weights as N | weights = N/total, then target as rate |

------------------------------------------------------------------------

## 4. Fully Bayesian benchmarking (v1.0.0)

The default
[`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
mode applies the adjustment to the posterior **mean** only – the SD
column is approximated as unchanged. **This understates uncertainty for
multiplicative ratios** and over- or understates it for additive shifts.

The fully Bayesian mode applies the benchmark to **every posterior
draw** and recomputes SD, RSE, and quantiles correctly:

``` r

# Pass posterior = TRUE if predictions object carries draws
bm_bayes <- sae_benchmark(
  predictions = estimates,
  target      = 1000,
  method      = "ratio",
  posterior   = TRUE,                # NEW in v1.0.0
  probs       = c(0.025, 0.5, 0.975)
)

bm_bayes$result_table
#>    Prediction      SD  RSE_percent      Q025      Q50      Q975
#> 1     ...           ...     ...          ...      ...      ...

# Or supply your own draws matrix (D x n)
draws <- posterior_predict(fit$model)
bm_bayes2 <- sae_benchmark(estimates, target = 1000,
                            method = "ratio",
                            posterior = draws)
```

### Why this matters

For multiplicative ratio benchmarking, the adjustment factor itself is a
random variable computed from the same draws:
``` math
  r_d = T \, / \, \sum_j w_j \, \theta_{d,j},
  \qquad
  \theta_{d,i}^{B} = \theta_{d,i} \cdot r_d.
```
Since $`\theta_{d,i}`$ and $`r_d`$ are negatively correlated (a draw
with larger area-$`i`$ value implies a smaller $`r_d`$), the
post-benchmark variance is **strictly smaller** than the naive
$`r^2 \, \mathrm{Var}(\theta)`$ approximation. Fully Bayesian
benchmarking captures this exactly.

------------------------------------------------------------------------

## 5. Smooth terms: splines and Gaussian processes

`hbsaems` exposes the **full brms canonical API** for smooth terms.
Replace any linear predictor with a smooth function:

### 5.1 Penalised regression splines (`mgcv` backend)

``` r

# Default: thin-plate regression spline, basis dimension chosen by mgcv
fit_spline <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3),
  data           = data_fhnorm,
  re             = ~ (1 | regency),       # area-level random effect (Fay-Herriot)
  nonlinear      = c("x2", "x3"),
  nonlinear_type = "spline",
  spline_k       = 7L,                    # basis dim (-1 = auto)
  spline_bs      = "tp",                  # "tp" (default), "cr", "cs", "ps"
  chains = 2, iter = 2000, refresh = 0
)
```

**Choosing the basis (`spline_bs`):**

| Basis | When to use |
|----|----|
| `"tp"` | Thin-plate (default). Good general choice; rotationally invariant. |
| `"cr"` | Cubic regression spline. More numerically stable when auxiliary variables are highly correlated. |
| `"cs"` | Cubic with shrinkage. Allows variable selection — smooths on irrelevant covariates shrink to zero. |
| `"ps"` | P-splines. Strong choice for monotonic relationships. |

### 5.2 Gaussian processes

For SAE applications with more than ~100 areas, **always** use the
Hilbert-space approximate GP (HSGP) of Riutort-Mayol et al. (2023):

``` r

# RECOMMENDED: HSGP with Matérn 5/2 covariance
fit_gp <- hbm(
  formula        = brms::bf(y ~ x1 + x2),
  data           = data_fhnorm,
  re             = ~ (1 | regency),
  nonlinear      = "x1",
  nonlinear_type = "gp",
  gp_k           = 20L,                  # basis dim — REQUIRED for n > 100
  gp_cov         = "matern25",           # more stable than "exp_quad"
  gp_c           = 1.25,                 # boundary scale (brms default)
  chains = 2, iter = 2000, refresh = 0
)
```

**GP argument reference:**

| Argument | Meaning | Recommendation |
|----|----|----|
| `gp_k` | Number of basis functions for HSGP | `ceiling(min(n/5, 25))` |
| `gp_cov` | Covariance function | `"matern25"` for SAE; `"exp_quad"` default |
| `gp_c` | HSGP boundary-scale factor | `1.25` (brms default); increase if GP truncates at edges |

**Note: exact GP warning.** When `gp_k = NA` (the default), an *exact*
GP is fitted — which scales $`O(n^3)`$. For $`n > 100`$, `hbsaems` emits
an immediate warning recommending an HSGP basis size. *Always* set
`gp_k` in production SAE work.

### 5.3 Same arguments in distribution-specific wrappers

All wrappers (`hbm_lnln`, `hbm_binlogitnorm`, `hbm_betalogitnorm`) and
the flexible factory
[`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
accept the same smooth-term arguments. Or use the
[`hbm_nonlinear()`](https://madsyair.github.io/hbsaems/reference/hbm_nonlinear.md)
config helper:

``` r

# Build the smooth-term spec once, reuse across models
nl <- hbm_nonlinear(c("x1"), type = "gp",
                     k = 20, gp_cov = "matern25")

fit <- hbm_flex(family_key = "lognormal",
                 response  = "y_obs",
                 auxiliary = c("x1", "x2"),
                 area_var  = "district",
                 data      = data_lnln,
                 nl)                       # spliced in automatically
```

------------------------------------------------------------------------

## 5.4 Convergence advice for smooth-term SAE models

Hierarchical Bayesian SAE models with smooth terms are prone to a few
recurring issues. Run
[`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
after every fit; the recommendations below address the most common
pathologies.

**1. Divergent transitions** (most common). Cause is usually the
“funnel” between the area-level standard deviation and the area
intercepts. Fix:

- Set `control = list(adapt_delta = 0.99, max_treedepth = 12)`.
- Tighten the prior on the area SD:
  `set_prior("normal(0, 0.5)", class = "sd")`.

**2. Low effective sample size** (ESS \< 1000):

- Increase `iter` to 6000 with `warmup = 3000`.
- Centre and scale the auxiliary variables before fitting.
- Run
  [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md)
  (sample_prior = “only”) to rule out prior–data conflict.

**3. Exact GP at $`n > 100`$** — *always* hits `max_treedepth` and/or
diverges. Set `gp_k`. See §5.2 above.

**4. Spline collinearity**. When auxiliary variables are correlated:

- Switch to `spline_bs = "cr"` for better numerical conditioning.
- Use `spline_bs = "cs"` if you want shrinkage-driven selection on the
  smooth terms.

**5. Spatial random effects**. See
[`vignette("hbsaems-spatial")`](https://madsyair.github.io/hbsaems/articles/hbsaems-spatial.md)
for the BYM2 recommendation.

------------------------------------------------------------------------

## 6. Shrinkage priors

`hbsaems` exposes brms-canonical global-local shrinkage priors. Both
**horseshoe** (Piironen & Vehtari 2017) and **R2D2** (Zhang et al. 2022)
are available, and both can be combined with splines and GPs.

### 6.1 Horseshoe (Piironen & Vehtari 2017)

``` r

fit_hs <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3 + x4 + x5),
  data           = my_data,
  re             = ~ (1 | regency),
  prior_type     = "horseshoe",
  hs_df          = 1,        # local half-t df  (1 = original HS)
  hs_df_global   = 1,        # global half-t df
  hs_df_slab     = 4,        # slab half-t df   (regularised HS+)
  hs_scale_slab  = 2,        # slab scale
  hs_autoscale   = TRUE,     # scale with residual sigma (Gaussian only)
  chains = 2, iter = 2000, refresh = 0
)
```

### 6.2 R2D2 (Zhang, Yang & Bondell 2022)

``` r

fit_r2d2 <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3 + x4 + x5),
  data           = my_data,
  re             = ~ (1 | regency),
  prior_type     = "r2d2",
  r2d2_mean_R2   = 0.5,      # prior mean of R^2
  r2d2_prec_R2   = 2,        # prior precision of R^2
  r2d2_autoscale = TRUE,
  chains = 2, iter = 2000, refresh = 0
)
```

### 6.3 Cascading shrinkage across smooth and GP terms

When a model combines parametric coefficients with non-parametric smooth
terms ([`s()`](https://paulbuerkner.com/brms/reference/s.html)) or
Gaussian processes
([`gp()`](https://paulbuerkner.com/brms/reference/gp.html)), the
principled approach is to **shrink all components jointly**. `hbsaems`
does this automatically using the brms-canonical `main = TRUE` pattern:

``` r

# Model with linear coefficients + spline + Gaussian process
fit_combined <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3),
  data           = my_data,
  re             = ~ (1 | regency),
  nonlinear      = c("x2", "x3"),
  nonlinear_type = "spline",       # spline for x2 AND x3
  prior_type     = "horseshoe",    # cascades automatically
  chains = 2, iter = 2000, refresh = 0
)
```

Internally `hbsaems` builds the following brms prior structure:

``` r

horseshoe(df = 1, df_global = 1, df_slab = 4, scale_slab = 2, main = TRUE)  # class "b"
horseshoe(df = 1, df_global = 1, df_slab = 4, scale_slab = 2)               # class "sds"
```

The `main = TRUE` flag on the `b` prior is what tells brms to apply the
horseshoe **jointly** to coefficients and to the smooth/GP standard
deviations. Without this cascade, the spline wiggliness and GP variance
would be regularised by their own (uninformative) default priors,
undermining the shrinkage from the parametric part.

The same cascade works for R2D2 and any combination of
[`s()`](https://paulbuerkner.com/brms/reference/s.html) +
[`gp()`](https://paulbuerkner.com/brms/reference/gp.html) terms:

| Formula contains           | Parameter classes regularised |
|----------------------------|-------------------------------|
| `y ~ x1 + x2`              | `b`                           |
| `y ~ x1 + s(x2)`           | `b`, `sds`                    |
| `y ~ x1 + gp(x2, k=10)`    | `b`, `sdgp`                   |
| `y ~ s(x1) + gp(x2, k=10)` | `b`, `sds`, `sdgp`            |

### 6.4 When to disable autoscale

The `hs_autoscale` / `r2d2_autoscale` flags control whether brms
re-scales the prior using the residual standard deviation $`\sigma`$.
This is the right thing to do for **continuous** responses (Gaussian,
lognormal) but $`\sigma`$ is undefined for discrete families. For Beta,
binomial, or Poisson responses, set `autoscale = FALSE`:

``` r

fit_bin_hs <- hbm_binlogitnorm(
  response       = "y",
  trials         = "n",
  auxiliary      = c("x1", "x2", "x3", "x4", "x5"),
  area_var       = "district",
  data           = data_binlogitnorm,
  prior_type     = "horseshoe",
  hs_autoscale   = FALSE,            # binomial: no residual SD to scale by
  chains = 2, iter = 2000, refresh = 0
)
```

Both priors work with **any family** in the registry (Gaussian, Beta,
binomial, lognormal, loglogistic, custom…) and combine with splines /
GPs without extra setup.

------------------------------------------------------------------------

## 7. Custom distributions: the model registry

The model registry (`R/models-registry.R`) catalogues every model spec
hbsaems knows about. Each spec bundles a likelihood family, a default
link, optional auxiliary-parameter hyperpriors, missing-data support
flags, and response-domain checks. Registered models are usable via
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md), the
convenience wrappers, and the flexible factory
[`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).

### Inspecting the registry

``` r

list_hbsae_models()
#> [1] "Beta" "beta-binomial" "binomial" "categorical" ...

get_hbsae_model("beta")
#> $family             : "Beta"
#> $link               : "logit"
#> $discrete           : FALSE
#> $supports_mi        : TRUE
#> $aux_param_hyperprior: function(args, data) ...   # phi ~ gamma(alpha, beta)
```

### Adding a custom model (Tier-1: pure metadata)

For distributions that just need a name, link, and domain check:

``` r

register_hbsae_model(
  key                = "gamma_log",
  family             = "Gamma",
  link               = "log",
  discrete           = FALSE,
  supports_mi        = TRUE,
  response_check     = function(y) all(y > 0, na.rm = TRUE),
  response_check_msg = "Gamma response must be strictly positive."
)

# Use immediately
fit_gamma <- hbm_flex(
  family_key = "gamma_log",
  response   = "expenditure",
  auxiliary  = c("age", "income", "education"),
  area_var   = "kecamatan",                    # area-level random effect
  data       = my_household_data
)
```

### Adding a custom model (Tier-2: auxiliary parameter hyperprior)

For distributions with auxiliary parameters (e.g. $`\phi`$ for Beta,
shape for Gamma):

``` r

register_hbsae_model(
  key            = "gamma_with_hyperprior",
  family         = "Gamma",
  link           = "log",
  response_check = function(y) all(y > 0, na.rm = TRUE),

  # Inject Stan code via stanvars + a prior on the shape parameter
  aux_param_hyperprior = function(args, data) {
    if (!isTRUE(args$add_shape_prior)) return(NULL)
    list(
      stanvars = brms::stanvar(
                  scode = "real<lower=0> shape_a;\n  real<lower=0> shape_b;",
                  block = "parameters") +
                brms::stanvar(
                  scode = "shape_a ~ gamma(1, 1);\n  shape_b ~ gamma(1, 1);",
                  block = "model"),
      prior    = brms::set_prior("gamma(shape_a, shape_b)", class = "shape")
    )
  }
)

# Trigger the hyperprior via aux_args
fit <- hbm_flex(
  family_key = "gamma_with_hyperprior",
  response   = "y",
  auxiliary  = c("x1", "x2"),
  area_var   = "area_id",                      # area-level random effect
  data       = my_data,
  aux_args   = list(add_shape_prior = TRUE)
)
```

------------------------------------------------------------------------

## 8. Bayesian model averaging with LOO weights

When several candidate models pass convergence and posterior-predictive
checks, **model averaging** lets you hedge across the candidates rather
than committing to a single best model.
[`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
supports three weighting strategies:

| `method` | Source of weights | When to use |
|----|----|----|
| `"manual"` | User-supplied `weights` vector | When external constraints (e.g. official survey methodology) impose specific weights |
| `"stacking"` | `loo::loo_model_weights(method="stacking")` | Recommended default; optimises log-score over the simplex (Yao et al. 2018) |
| `"pseudobma"` | `loo::loo_model_weights(method="pseudobma")` | Robust when one model is clearly better than the others |

``` r

# Fit two competing models
fit_linear <- hbm(brms::bf(y ~ x1 + x2 + x3),
                   data = my_data, re = ~(1 | regency))
fit_smooth <- hbm(brms::bf(y ~ x1 + x2 + x3),
                   data           = my_data,
                   re             = ~(1 | regency),
                   nonlinear      = c("x2", "x3"),
                   nonlinear_type = "spline")

# Stacking (recommended Bayesian model averaging)
avg_stack <- model_average(fit_linear, fit_smooth,
                            method = "stacking")

# Inspect the computed weights
attr(avg_stack, "weights")
#> [1] 0.34 0.66
attr(avg_stack, "weight_method")
#> [1] "stacking"
```

The returned `hbsae_results` object can be passed to
[`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
exactly like any single-model estimate.

------------------------------------------------------------------------

## 9. Prior sensitivity analysis

After convergence has been verified, the next question is: .
[`prior_sensitivity()`](https://madsyair.github.io/hbsaems/reference/prior_sensitivity.md)
wraps the modern power-scaling diagnostics of Kallioinen et al.  
(2024) which detect:

- **Prior-data conflict** – the posterior moves non-negligibly when the
  prior is up- or down-weighted. Often indicates an overly informative
  or misspecified prior.
- **Weak likelihood** – the posterior is dominated by the prior. Common
  in SAE for areas with few sampled units.

``` r

fit <- hbm(brms::bf(y ~ x1 + x2 + x3),
           data           = my_data,
           re             = ~(1 | regency),
           prior_type     = "horseshoe",
           chains = 4, iter = 4000)

# Report power-scaling diagnostics for every monitored parameter
ps <- prior_sensitivity(fit)
print(ps)
#> # A tibble: 8 x 4
#>   variable                  prior  likelihood diagnosis
#>   <chr>                     <dbl>       <dbl> <chr>
#> 1 b_Intercept              0.012        0.821 -
#> 2 b_x1                     0.156        0.683 -
#> 3 b_x2                     0.834        0.412 prior-data conflict
#> ...
```

A non-“-” diagnosis flags a parameter that warrants closer inspection;
in this hypothetical output, the coefficient on `x2` shows a prior-data
conflict, suggesting the horseshoe prior is too strong for that
covariate.

[`prior_sensitivity()`](https://madsyair.github.io/hbsaems/reference/prior_sensitivity.md)
requires the package; install with `install.packages("priorsense")`.

------------------------------------------------------------------------

A realistic workflow combining everything:

``` r

# 1. Check data
chk <- check_data(my_data, response = "y",
                   auxiliary  = c("x1", "x2", "x3"),
                   spatial_var = "kecamatan")

# 2. Build CAR matrix from shapefile
M <- build_spatial_weight("kecamatan.shp",
                            for_model = "car",
                            id_col    = "kec_code")

# 3. Configure with helpers (v1.0.0+); pass bundles directly to hbm()
priors <- hbm_priors(prior_type = "horseshoe", hs_df_slab = 4)
nl     <- hbm_nonlinear(c("x1"), type = "spline", k = 5)
ctrl   <- hbm_control(chains = 4, iter = 4000, cores = 4,
                       adapt_delta = 0.95)

# 4. Fit
fit <- hbm(
  formula = brms::bf(y ~ x1 + x2 + x3),
  data    = my_data,
  spatial_var = "kecamatan", spatial_model = "car", M = M,
  handle_missing = chk$recommended_method,
  priors, nl, ctrl
)

# 5. Predict at all areas (including non-sample)
est <- sae_predict(fit)

# 6. Benchmark to kabupaten totals (fully Bayesian)
T_kab <- c(Bogor = 110, Sukabumi = 145, Cianjur = 145)
bm <- sae_benchmark(est,
                     target  = T_kab,
                     weights = my_data$populasi,
                     groups  = my_data$kabupaten,
                     method  = "raking",
                     posterior = TRUE)

# 7. Use the corrected uncertainty
bm$result_table
```

------------------------------------------------------------------------

## References

- **Besag, J.** (1974). Spatial interaction and the statistical analysis
  of lattice systems. *JRSS-B*, 36(2), 192-225.
- **Anselin, L.** (1988). *Spatial Econometrics: Methods and Models*.
  Kluwer.
- **Pfeffermann, D.** (2013). New important developments in small area
  estimation. *Statistical Science*, 28(1), 40-68.
- **Piironen, J. & Vehtari, A.** (2017). Sparsity information and
  regularization in the horseshoe and other shrinkage priors.
  *Electronic J. of Statistics*, 11(2), 5018-5051.
- **Zhang, Y. D., Naughton, B. P., Bondell, H. D., & Reich, B. J.**
  (2022). Bayesian Regression Using a Prior on the Model Fit: The R2-D2
  Shrinkage Prior. *JASA*, 117(538), 862-874.
- **Riutort-Mayol, G., Burkner, P.-C., Andersen, M. R., Solin, A., &
  Vehtari, A.** (2023). Practical Hilbert space approximate Bayesian
  Gaussian processes for probabilistic programming. *Statistics and
  Computing*, 33, 17.
- **Yao, Y., Vehtari, A., Simpson, D., & Gelman, A.** (2018). Using
  stacking to average Bayesian predictive distributions. *Bayesian
  Analysis*, 13(3), 917-1007.
- **Kallioinen, N., Paananen, T., Burkner, P.-C., & Vehtari, A.**
  (2024). Detecting and diagnosing prior and likelihood sensitivity with
  power-scaling. *Statistics and Computing*, 34, 57.
- **Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Burkner,
  P.-C.** (2021). Rank-normalization, folding, and localization: An
  improved R-hat for assessing convergence of MCMC. *Bayesian Analysis*,
  16(2), 667-718.
