# Handling Missing Data in hbsaems

> **Note on the printed output blocks.** Stan / `brms` model fits take
> several minutes to run and are therefore **not executed at vignette
> build time**. The numerical outputs shown after each `eval = FALSE`
> chunk in this vignette are **realistic illustrations** –
> representative of what you would see on a real run, but not produced
> by the chunks above. To reproduce the exact numbers, copy the code
> into an interactive R session.

## Missing data in SAE

Missingness is the rule rather than the exception in small area work.
Three patterns are most common:

1.  **Missing response, complete covariates** – some areas were not
    sampled at all, but you have administrative auxiliaries for them.
2.  **Complete response, missing covariates** – the survey reached every
    area, but a census variable is missing for some.
3.  **Missing in both** – a mix of the above.

`hbsaems` exposes three strategies through a single `handle_missing`
argument:

| Strategy | Trigger | What it does |
|----|----|----|
| `"deleted"` | Listwise deletion | Drops rows with any missing variable |
| `"multiple"` | `mice` multiple imputation | Imputes covariates $`m`$ times, fits each, pools |
| `"model"` | Joint Bayesian imputation via [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html) | Treats missing values as parameters |

When `handle_missing = NULL` (default), the package **auto-selects** a
strategy based on the pattern detected and the family’s capabilities.

## Strategy 1: deletion (listwise)

The simplest approach: drop any row with `NA` in `response` or
`auxiliary`. Appropriate when the proportion missing is small and
missingness is plausibly MCAR (missing completely at random):

``` r

library(hbsaems)
data("data_fhnorm")
data_miss_y      <- data_fhnorm
data_miss_y$y[c(3, 14, 27)] <- NA

fit_deleted <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3),
  re             = ~ (1 | group),
  data           = data_miss_y,
  handle_missing = "deleted",
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

    handle_missing = 'deleted': 3 row(s) with missing response variable removed
    from model fitting.

The dropped rows are listed in `fit_deleted$missing_info`. Predictions
for these areas can still be obtained via
[`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
with their auxiliary values.

## Strategy 2: multiple imputation (mice)

For covariates that are missing in some areas but observable in
principle from a census or register, multiple imputation with `mice` is
the workhorse:

``` r

data_miss_x         <- data_fhnorm
data_miss_x$x1[6:8] <- NA

fit_mi <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3),
  re             = ~ (1 | group),
  data           = data_miss_x,
  handle_missing = "multiple",
  m              = 5,                       # 5 imputations
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

    Missing predictor variable(s): x1. Applying multiple imputation (mice)
    with m = 5 imputations.

    Compiling the C++ model
    Fitting imputed model 1
    Start sampling
    ...
    Fitting imputed model 2
    Start sampling
    ...
    Fitting imputed model 5
    Start sampling
    ...

    (Posterior draws pooled via Rubin's rules.)

The pooled posterior is returned in the usual `hbmfit` interface.
Diagnostics are *per-imputation*; the global Rhat / ESS view is not
informative for pooled chains – see
[`?brm_multiple`](https://paulbuerkner.com/brms/reference/brm_multiple.html)
for details.

You can control mice via `mice_args`:

``` r

fit_mi2 <- hbm(
  formula        = brms::bf(y ~ x1 + x2 + x3),
  re             = ~ (1 | group),
  data           = data_miss_x,
  handle_missing = "multiple",
  m              = 10,
  mice_args      = list(method = "pmm", maxit = 20),
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

## Strategy 3: model-based (joint Bayesian) imputation

When the missingness is suspected MAR or MNAR and you have a good model
for the missing variable, joint Bayesian imputation via
[`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html) is more
principled:

``` r

fit_model <- hbm(
  formula        = brms::bf(y  | mi() ~ mi(x1) + x2 + x3) +
                   brms::bf(x1 | mi() ~ x2 + x3),
  re             = ~ (1 | group),
  data           = data_miss_x,
  handle_missing = "model",
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

    handle_missing = 'model': using mi() specification for joint model-based
    imputation.

    Setting 'rescor' to FALSE by default for this model.

    Compiling Stan program...
    Start sampling...

This fits a **multivariate** brms model where missing values of `y` and
`x1` are sampled alongside the regression coefficients. Posterior
uncertainty in the imputations is automatically propagated.

Not every family supports
[`mi()`](https://paulbuerkner.com/brms/reference/mi.html) – check
`spec$supports_mi` in the model registry
([`?list_hbsae_models`](https://madsyair.github.io/hbsaems/reference/list_hbsae_models.md)).

## Auto-selection

If you omit `handle_missing`, the wrappers will look at the missing
pattern and choose:

``` r

fit_auto <- hbm_lnln(
  response  = "y_obs",
  auxiliary = c("x1", "x2", "x3"),
  group     = "group",
  data      = data_with_some_na,
  # handle_missing not given
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

    Missing values detected. Auto-selecting handle_missing = 'multiple'
    (family 'lognormal', supports_mi = FALSE).

The decision tree is:

    Missing response only           ->  "deleted"
    Missing covariates only,
       family supports mi()          ->  "model"
    Missing covariates only,
       family does NOT support mi()  ->  "multiple"
    Missing in both                  ->  "model" if supported, else "multiple"

You can always override this with an explicit `handle_missing = "..."`.

## Comparing strategies

When time permits, compare strategies’ predictive performance:

``` r

loo_compare <- loo::loo_compare(
  loo::loo(fit_deleted$brmsfit),
  loo::loo(fit_mi$brmsfit),
  loo::loo(fit_model$brmsfit)
)
loo_compare
```

                      elpd_diff se_diff
    fit_model         0.0       0.0
    fit_mi           -2.1       2.4
    fit_deleted     -10.7       3.8

Deletion’s poor performance here is typical: throwing away 3 of 100
informative rows costs predictive accuracy.

## Practical guidance

- **Small fraction missing (\< 5%) and likely MCAR** – deletion is fine.
- **Larger fraction or non-MCAR** – prefer multiple imputation or
  model-based.
- **Missing in response only and you have administrative auxiliaries for
  those areas** – prefer deletion + post-fit prediction via
  [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  (this is more transparent than imputing the response).
- **Missing in covariates only** – model-based imputation is most
  principled if the family supports it; mice is a robust fallback.
- **Reproducibility** – set `seed` for both the wrapper call and (if
  using mice) within `mice_args` to make imputations reproducible.

## References

- Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
  Wiley.
- van Buuren, S. (2018). *Flexible Imputation of Missing Data*. CRC
  Press.
- Buerkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel
  Models Using Stan. *Journal of Statistical Software*, 80(1), 1-28.
  Section on [`mi()`](https://paulbuerkner.com/brms/reference/mi.html).
