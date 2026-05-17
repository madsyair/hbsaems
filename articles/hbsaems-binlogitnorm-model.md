# Binomial Logit-Normal SAE Model

> **Note on the printed output blocks.** Stan / `brms` model fits take
> several minutes to run and are therefore **not executed at vignette
> build time**. The numerical outputs shown after each `eval = FALSE`
> chunk in this vignette are **realistic illustrations** –
> representative of what you would see on a real run, but not produced
> by the chunks above.

## When to use the binomial logit-normal model

When the response is a **count of successes out of a known number of
trials** – say $`y_i`$ vaccinated children out of $`n_i`$ surveyed in
area $`i`$ – the binomial likelihood is the natural choice:

``` math
\begin{aligned}
y_i \mid p_i, n_i &\sim \mathrm{Binomial}(n_i, p_i), \\
\mathrm{logit}(p_i)
   &= x_i^\top \boldsymbol{\beta} + u_i, \quad
     u_i \sim \mathcal{N}(0, \sigma_u^2).
\end{aligned}
```

This differs from the Beta model in an important way: the **trials**
$`n_i`$ are *exactly* known (you counted them) rather than just being
used to set a precision parameter. Use this model when you have raw
counts; use the Beta model (`hbm_betalogitnorm`) when you have direct
proportion estimates with a known design effect.

## The data

``` r

data("data_binlogitnorm")
str(data_binlogitnorm[, c("district", "y", "n", "p", "x1", "x2", "x3")])
#> 'data.frame':    100 obs. of  7 variables:
#>  $ district: chr  "district_001" "district_002" "district_003" "district_004" ...
#>  $ y       : int  25 27 69 27 16 47 4 27 59 42 ...
#>  $ n       : int  136 171 183 120 53 130 57 116 157 177 ...
#>  $ p       : num  0.184 0.158 0.377 0.225 0.302 ...
#>  $ x1      : num  -0.5605 -0.2302 1.5587 0.0705 0.1293 ...
#>  $ x2      : num  0.2896 1.2569 0.7533 0.6525 0.0484 ...
#>  $ x3      : num  1.199 0.312 -1.265 -0.457 -1.414 ...
```

    'data.frame':   100 obs. of  7 variables:
     $ district: chr  "district_001" "district_002" "district_003" "district_004" ...
     $ y    : int  62 41 78 53 39 84 67 49 41 76 ...
     $ n    : int  120 95 142 108 87 153 124 102 91 138 ...
     $ p    : num  0.517 0.432 0.549 0.491 0.448 ...
     $ x1   : num  0.82 -0.41 1.04 0.15 -0.92 ...
     $ x2   : num  -0.31 0.74 -0.55 0.21 1.04 ...
     $ x3   : num  0.18 0.42 0.07 -0.31 0.65 ...

Here `y` is the raw success count, `n` the trials, `p = y / n` is the
direct estimate, and `x1`, `x2`, `x3` are auxiliary variables.

## Standard fit

``` r

library(hbsaems)
library(brms)

fit <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  area_var   = "district",
  data      = data_binlogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit)
```

    ===== Hierarchical Bayesian Model Summary =====

     Observations : 100
     Family       : binomial (link: logit )
     Formula      : y | trials(n) ~ x1 + x2 + x3 + (1 | district)

    ----- Parameter Estimates -----
     Family: binomial
      Links: mu = logit
    Formula: y | trials(n) ~ x1 + x2 + x3 + (1 | district)
       Data: data_binlogitnorm (Number of observations: 100)
      Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
             total post-warmup draws = 8000

    Multilevel Hyperparameters:
    ~group (Number of levels: 100)
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.24      0.05     0.14     0.34 1.00     1942     2734

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     0.08      0.04     0.00     0.16 1.00     5418     6107
    x1            0.28      0.03     0.22     0.34 1.00     6802     6917
    x2           -0.15      0.03    -0.21    -0.09 1.00     6517     6248
    x3            0.07      0.03     0.01     0.13 1.00     6604     6321

    Draws were sampled using sampling(NUTS).

The `Family: binomial` line and the `y | trials(n)` formula confirm that
the wrapper has correctly set up the binomial response and passed the
trials column.

## With CAR spatial random effect

For spatially autocorrelated areas (e.g. neighbouring districts sharing
characteristics), replace the IID `(1 | district)` random effect with a
CAR spatial random effect:

``` r

data("adjacency_matrix_car")
fit_car <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  spatial_var = "regency",                       # spatial-RE column
  spatial_model  = "car",                       # CAR structure
  M         = adjacency_matrix_car,        # neighbour weight matrix
  data      = data_binlogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit_car)
```

    Correlation Structures:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sdcar     0.19      0.07     0.06     0.34 1.00     1843     2516

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     0.07      0.04     0.00     0.15 1.00     6018     6342
    x1            0.26      0.03     0.20     0.32 1.00     7218     6917
    x2           -0.14      0.03    -0.20    -0.08 1.00     6814     6539
    x3            0.07      0.03     0.01     0.13 1.00     7104     6428

The new `Correlation Structures:` block reports the spatial standard
deviation `sdcar`. See
[`vignette("hbsaems-spatial")`](https://madsyair.github.io/hbsaems/articles/hbsaems-spatial.md)
for SAR, BYM2, and weight-matrix construction.

## Custom prior on a coefficient

To impose a tighter prior on a particular coefficient – for example,
constraining the effect of an obviously non-informative auxiliary to
near-zero – supply a
[`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
object:

``` r

fit_prior <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  area_var   = "district",
  data      = data_binlogitnorm,
  prior     = brms::set_prior("normal(0, 0.1)", class = "b", coef = "x3"),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
```

Default priors (`Intercept ~ student_t(4, 0, 10)`,
`b ~ student_t(4, 0, 2.5)`) are filled in for any class you don’t
override.

## Missing data

If `y`, `n`, or auxiliary columns contain `NA`, the wrapper
**auto-selects** a strategy unless you supply `handle_missing`:

``` r

data_with_na <- data_binlogitnorm
data_with_na$x1[c(3, 14, 27)] <- NA          # 3 areas with missing x1

fit_miss <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  area_var   = "district",
  data      = data_with_na,
  # handle_missing not given -> auto-select "multiple" (mice)
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
```

    Missing values detected. Auto-selecting handle_missing = 'multiple'
    (family 'binomial', supports_mi = FALSE).

    Missing predictor variable(s): x1. Applying multiple imputation
    (mice) with m = 5 imputations.
    Fitting imputed model 1...
    Fitting imputed model 2...
    ...
    Fitting imputed model 5...

Posterior samples from the five imputed fits are pooled via Rubin’s
rules in the returned `hbmfit` object. See
[`vignette("hbsaems-handle-missing")`](https://madsyair.github.io/hbsaems/articles/hbsaems-handle-missing.md)
for the full discussion.

## Convergence and prediction

The diagnostic and prediction steps are the same as for any wrapper:

``` r

convergence_check(fit)

new_areas <- data.frame(
  group = 101:105,
  n     = c(150, 120, 180, 110, 140),
  x1    = rnorm(5),
  x2    = rnorm(5),
  x3    = rnorm(5)
)
preds <- sae_predict(fit, newdata = new_areas, allow_new_levels = TRUE)
preds
```

    $verdict
    [1] "All chains converged: Rhat < 1.01, ESS > 400, no divergent transitions."

      group estimate  lower  upper
    1   101   0.524  0.461  0.589
    2   102   0.461  0.395  0.531
    3   103   0.587  0.523  0.648
    4   104   0.448  0.378  0.519
    5   105   0.495  0.432  0.560

Predictions for unseen areas use the marginal posterior of the random
intercept.

## Summary

- Use
  **[`hbm_binlogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md)**
  when you have *raw success counts* and *known trial counts*.
- Use
  **[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)**
  instead when you have *direct proportion estimates with a design
  effect*.
- Add spatial structure via `spatial_var`, `spatial_model`, and `M` –
  see the spatial vignette.
- Missing data is auto-handled with multiple imputation (mice).

## References

- Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*, 2nd ed.
  Wiley. Chapter 4 covers area-level binary SAE.
- Buerkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel
  Models Using Stan. *Journal of Statistical Software*, 80(1), 1-28.
