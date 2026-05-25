# Complete Workflow with hbsaems 1.0.0

``` r

library(hbsaems)
```

A typical Bayesian SAE workflow with `hbsaems` v1.0.0 follows seven
steps, each backed by a single primary function. This vignette walks
through the canonical pipeline.

## See also

For deeper coverage of topics beyond this introductory workflow, see the
**articles on the package website**:

<https://madsyair.github.io/hbsaems/articles/>

The articles cover:

- Distribution-specific walkthroughs (Beta logit-normal, binomial
  logit-normal, lognormal-lognormal)
- Spatial models (CAR / SAR / BYM2)
- Handling missing data (deletion, multiple imputation, joint Bayesian
  imputation)
- The interactive Shiny dashboard
- Advanced features (custom families, benchmarking, smooth terms)
- AST-based formula manipulation (internals)
- Migration guide for users coming from earlier versions

## 0. Inspect the data

``` r

data("data_fhnorm")
str(data_fhnorm, max.level = 1)
#> 'data.frame':    100 obs. of  9 variables:
#>  $ y         : num  6.98 9.01 6.22 10.47 10.62 ...
#>  $ D         : num  0.921 2.643 1.704 0.659 1.017 ...
#>  $ x1        : num  -0.9026 -0.7527 -0.2606 0.0759 0.2118 ...
#>  $ x2        : num  0.712 -0.878 -0.607 -1.077 -0.502 ...
#>  $ x3        : num  1.431 -0.318 -1.029 -1.071 -0.496 ...
#>  $ theta_true: num  8.5 9.73 8.54 9.68 11.86 ...
#>  $ u         : num  -0.8531 -0.0145 -1.2505 -0.5989 1.5845 ...
#>  $ regency   : chr  "regency_001" "regency_002" "regency_003" "regency_004" ...
#>  $ province  : chr  "province_01" "province_01" "province_01" "province_01" ...
#>  - attr(*, "datalabel")= chr "data_fhnorm"
#>  - attr(*, "var.labels")= chr [1:9] "" "" "" "" ...
head(data_fhnorm[, c("regency", "province", "y", "D", "x1", "x2", "x3")], 4)
#>       regency    province         y         D         x1         x2         x3
#> 1 regency_001 province_01  6.982249 0.9213655 -0.9026345  0.7116302  1.4305563
#> 2 regency_002 province_01  9.005801 2.6432183 -0.7526745 -0.8779797 -0.3182451
#> 3 regency_003 province_01  6.216509 1.7035092 -0.2605613 -0.6066989 -1.0293040
#> 4 regency_004 province_01 10.469687 0.6586857  0.0759456 -1.0766939 -1.0711613
```

`data_fhnorm` is a simulated Fay-Herriot dataset: 100 regencies nested
within 5 provinces, with a known sampling variance `D` per area and
three covariates.

## 1. Prior predictive check

``` r

# This is the production call.  Replace `chains`, `iter` with your
# usual values; the lighter settings below are a quick demonstration.
model_prior <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  data              = data_fhnorm,
  re                = ~ (1 | regency),     # area-level random effect (Fay-Herriot)
  sampling_variance = "D",                 # KNOWN sampling variance D_i
  sample_prior      = "only",
  prior             = c(
    brms::prior(normal(0, 1),  class = "b"),
    brms::prior(normal(10, 5), class = "Intercept"),
    brms::prior(normal(0, 2),  class = "sd")
  ),
  chains = 2, iter = 1000
)
pc <- prior_check(model_prior,
                  data         = data_fhnorm,
                  response_var = "y")
plot(pc)
```

The `sampling_variance = "D"` argument is the **defining feature** of a
Fay-Herriot model: the sampling variance is treated as **known** from
the design (it is not estimated), so brms pins the observation-level to
. Omitting this argument makes the model unidentified because the
residual and the area random-effect would compete to explain the same
variance, typically producing divergent transitions and very wide
credible intervals.

## 2. Fit the model

``` r

model <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  data              = data_fhnorm,
  re                = ~ (1 | regency),
  sampling_variance = "D",
  control           = list(adapt_delta = 0.99),
  chains            = 4, iter = 4000, warmup = 2000, cores = 4
)
summary(model)
```

Two settings deserve attention:

- **`sampling_variance = "D"`** – the column `D` in `data_fhnorm` holds
  the known sampling variance for each area; `hbsaems` translates this
  into an offset on the observation-level standard deviation so brms /
  Stan never tries to estimate it.
- **`adapt_delta = 0.99`** – Fay-Herriot likelihoods can produce mild
  funnel geometry between and the area random effects when many areas
  have small . Raising `adapt_delta` from the brms default 0.95 to 0.99
  buys a more conservative leapfrog step and eliminates the occasional
  divergent transition without re-parameterising the model.

### A small fitted model to demonstrate the rest of the workflow

For the remainder of the vignette we use a tiny model (very few
iterations) so that the diagnostic and prediction chunks below have a
real object to operate on. In your own analysis, **use the full
production settings shown above**, not the toy settings here.

``` r

# Mini fit -- iter = 200, chains = 1 -- for vignette demonstration only.
# Do NOT use these settings for inference: the chains have not
# converged at this length and the posterior will be biased.
fit_demo <- suppressWarnings(
  hbm(
    formula           = brms::bf(y ~ x1 + x2 + x3),
    data              = data_fhnorm,
    re                = ~ (1 | regency),
    sampling_variance = "D",
    chains  = 1,
    iter    = 200,
    warmup  = 100,
    refresh = 0,
    seed    = 1
  )
)
```

## 3. Convergence diagnostics

``` r

# Operate on the mini fit_demo above (NOT a substitute for production
# diagnostics on full chains).
diag <- convergence_check(fit_demo)
is_converged(fit_demo)
summary(diag)
hbm_warnings(fit_demo)
```

The full set of diagnostics on your production fit would also include
trace plots, R-hat / ESS tables, and pp-checks. These all follow the
same calling convention:

``` r

plot(diag, type = "trace")
plot(diag, type = "rhat")
plot(diag, type = "ess")
```

## 4. Goodness-of-fit

``` r

gof <- model_compare(model)    # single-model: LOO, WAIC, pp_check
summary(gof)
plot(gof, type = "pp_check")
```

## 5. Compare alternatives

``` r

model2 <- hbm(brms::bf(y ~ x1 + x2), data = data_fhnorm,
              re = ~ (1 | regency),
              sampling_variance = "D",
              control = list(adapt_delta = 0.99),
              chains = 4, iter = 4000)
model3 <- hbm(brms::bf(y ~ x1),      data = data_fhnorm,
              re = ~ (1 | regency),
              sampling_variance = "D",
              control = list(adapt_delta = 0.99),
              chains = 4, iter = 4000)

model_compare(model, model2)
model_compare_all(full = model, medium = model2, simple = model3)
```

## 6. Small-area estimates

The simplest call uses the data the model was fit on – no `newdata`
argument needed:

``` r

# In-sample prediction (default): operates on the data stored
# inside the fitted object.
est <- sae_predict(fit_demo)
summary(est)
head(est$result_table, 5)
```

To predict at fresh data points, pass `newdata`. When you do, any
internal offset columns the original fit needed
(e.g. `.hbsaems_sigma_fixed = sqrt(D)` for the Fay-Herriot sugar) are
repopulated automatically when the new data frame has the same number of
rows as the training data – the standard “predict at the same areas with
updated covariates” use case.

``` r

# Out-of-sample prediction at new areas:
est_new <- sae_predict(fit_demo, newdata = data_new)
```

### Bayesian model averaging

For model averaging, fit several candidate models, then either let
[`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
weight them by stacking weights (LOO-based) or supply your own weights:

``` r

# Stacking weights (default, derived from LOO)
avg_stacked <- model_average(model, model2, model3)

# User-specified weights -- must sum to 1
avg_manual  <- model_average(model, model2, weights = c(0.7, 0.3))

# The returned object is an `hbsae_results`, identical in shape to
# what sae_predict() returns, so all post-processing helpers
# (sae_transform, sae_scale, sae_filter, sae_aggregate) work on it.
plot(avg_stacked, type = "predictions")
```

### Visualisations

``` r

# Visualisations (require a graphics device):
plot(est, type = "predictions")
plot(est, type = "uncertainty")
```

## 7. Post-processing predictions

All `sae_*` post-processing helpers operate on the `hbsae_results`
object returned by either
[`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
or
[`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md):

``` r

log_est  <- sae_transform(est, log)
sc_est   <- sae_scale(est)
hi_est   <- sae_filter(est, est$pred > stats::median(est$pred))
agg_est  <- sae_aggregate(sae_predict(model),
                          sae_predict(model2),
                          method = "mean")
```

## Session info

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] hbsaems_1.0.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_1.2.1      dplyr_1.2.1           farver_2.1.2         
#>  [4] loo_2.9.0             S7_0.2.2              fastmap_1.2.0        
#>  [7] tensorA_0.36.2.1      rpart_4.1.27          digest_0.6.39        
#> [10] lifecycle_1.0.5       StanHeaders_2.32.10   survival_3.8-6       
#> [13] magrittr_2.0.5        posterior_1.7.0       compiler_4.6.0       
#> [16] rlang_1.2.0           sass_0.4.10           tools_4.6.0          
#> [19] yaml_2.3.12           knitr_1.51            bridgesampling_1.2-1 
#> [22] htmlwidgets_1.6.4     pkgbuild_1.4.8        RColorBrewer_1.1-3   
#> [25] abind_1.4-8           purrr_1.2.2           desc_1.4.3           
#> [28] nnet_7.3-20           grid_4.6.0            stats4_4.6.0         
#> [31] jomo_2.7-6            mice_3.19.0           inline_0.3.21        
#> [34] ggplot2_4.0.3         scales_1.4.0          iterators_1.0.14     
#> [37] MASS_7.3-65           cli_3.6.6             mvtnorm_1.3-7        
#> [40] rmarkdown_2.31        reformulas_0.4.4      ragg_1.5.2           
#> [43] generics_0.1.4        otel_0.2.0            RcppParallel_5.1.11-2
#> [46] minqa_1.2.8           cachem_1.1.0          rstan_2.32.7         
#> [49] stringr_1.6.0         splines_4.6.0         bayesplot_1.15.0     
#> [52] parallel_4.6.0        matrixStats_1.5.0     brms_2.23.0          
#> [55] vctrs_0.7.3           boot_1.3-32           glmnet_5.0           
#> [58] Matrix_1.7-5          jsonlite_2.0.0        mitml_0.4-5          
#> [61] systemfonts_1.3.2     foreach_1.5.2         jquerylib_0.1.4      
#> [64] tidyr_1.3.2           glue_1.8.1            pan_1.9              
#> [67] nloptr_2.2.1          pkgdown_2.2.0         codetools_0.2-20     
#> [70] distributional_0.7.0  stringi_1.8.7         gtable_0.3.6         
#> [73] shape_1.4.6.1         QuickJSR_1.10.0       lme4_2.0-1           
#> [76] tibble_3.3.1          pillar_1.11.1         htmltools_0.5.9      
#> [79] Brobdingnag_1.2-9     R6_2.6.1              textshaping_1.0.5    
#> [82] Rdpack_2.6.6          evaluate_1.0.5        lattice_0.22-9       
#> [85] rbibutils_2.4.1       backports_1.5.1       broom_1.0.13         
#> [88] bslib_0.11.0          rstantools_2.6.0      Rcpp_1.1.1-1.1       
#> [91] coda_0.19-4.1         gridExtra_2.3         nlme_3.1-169         
#> [94] checkmate_2.3.4       xfun_0.57             fs_2.1.0             
#> [97] pkgconfig_2.0.3
```
