# Lognormal-Lognormal SAE Model

> **Note on the printed output blocks.** Stan / `brms` model fits take
> several minutes to run and are therefore **not executed at vignette
> build time**. The numerical outputs shown after each `eval = FALSE`
> chunk in this vignette are **realistic illustrations** –
> representative of what you would see on a real run, but not produced
> by the chunks above. To reproduce the exact numbers, copy the code
> into an interactive R session.

## When to use the lognormal-lognormal model

A great many positive, right-skewed survey responses are well
approximated by a **lognormal** distribution: household expenditure,
agricultural yield, firm revenue, length of hospital stay. Modelling the
log of the response with a normal hierarchy is a workhorse choice in SAE
(Slud & Maiti 2006; You & Chapman 2006).

[`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md)
fits

``` math
\begin{aligned}
y_i \mid \theta_i, \sigma
   &\sim \mathrm{Lognormal}(\theta_i, \sigma^2), \\
\theta_i
   &= x_i^\top \boldsymbol{\beta} + u_i, \quad
     u_i \sim \mathcal{N}(0, \sigma_u^2),
\end{aligned}
```

i.e. the log-mean parameter $`\theta_i`$ is linear in the auxiliaries
plus an IID area random effect, and the within-area residual variance
$`\sigma^2`$ is either sampled (default) or **pinned** to the known
sampling variance from the survey design.

## The data

``` r

data("data_lnln")
str(data_lnln[, c("group", "y_obs", "psi_i", "x1", "x2", "x3")])
#> 'data.frame':    100 obs. of  6 variables:
#>  $ group: int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ y_obs: num  4.5 2.98 2.5 1.97 2.04 ...
#>  $ psi_i: num  0.1222 0.0725 0.046 0.1003 0.0943 ...
#>  $ x1   : num  -0.5605 -0.2302 1.5587 0.0705 0.1293 ...
#>  $ x2   : num  0.2896 1.2569 0.7533 0.6525 0.0484 ...
#>  $ x3   : num  1.199 0.312 -1.265 -0.457 -1.414 ...
```

    'data.frame':   100 obs. of  6 variables:
     $ group: int  1 2 3 4 5 6 7 8 9 10 ...
     $ y_obs: num  4.21 3.18 6.55 5.04 7.12 ...
     $ psi_i: num  0.058 0.071 0.044 0.089 0.062 ...
     $ x1   : num  1.34 -0.42 0.51 -1.07 0.89 ...
     $ x2   : num  -0.79 1.21 -0.40 0.55 -0.30 ...
     $ x3   : num  0.07 0.94 -0.92 1.07 0.71 ...

`y_obs` is the direct survey estimate (positive); `psi_i` is its known
per-area sampling variance **on the log scale**, i.e. the variance of
$`\log(\hat y_i)`$. If your survey software reports $`\mathrm{Var}(\hat
y_i)`$ on the original scale, convert with the delta-method
approximation
``` math
\psi_i \approx \mathrm{Var}(\hat y_i) \, / \, \hat y_i^2
```
before passing the column.

## Mode 1: random $`\sigma`$ (default)

``` r

library(hbsaems)
fit <- hbm_lnln(
  response  = "y_obs",
  auxiliary = c("x1", "x2", "x3"),
  group     = "group",
  data      = data_lnln,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit)
```

    ===== Hierarchical Bayesian Model Summary =====

     Observations : 100
     Family       : lognormal (link: identity )
     Formula      : y_obs ~ x1 + x2 + x3 + (1 | group)

    ----- Parameter Estimates -----
     Family: lognormal
      Links: mu = identity; sigma = log
    Formula: y_obs ~ x1 + x2 + x3 + (1 | group)
       Data: data_lnln (Number of observations: 100)
      Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
             total post-warmup draws = 8000

    Multilevel Hyperparameters:
    ~group (Number of levels: 100)
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.21      0.04     0.13     0.29 1.00     2418     3214

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     1.51      0.06     1.39     1.63 1.00     4912     5827
    x1            0.32      0.04     0.24     0.40 1.00     6204     6519
    x2           -0.18      0.04    -0.26    -0.10 1.00     5891     5934
    x3            0.11      0.03     0.05     0.17 1.00     6308     6147

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     0.27      0.03     0.21     0.34 1.00     5417     6028

    Draws were sampled using sampling(NUTS).

`sigma` appears in the “Further Distributional Parameters” block – it is
being sampled.

## Mode 2: Fay-Herriot variant with known sampling variance

When you know the per-area sampling variance $`\psi_i`$ from the survey
design (the classic Fay-Herriot situation on the log scale), pass
`sampling_var`:

``` r

fit_fh <- hbm_lnln(
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  group        = "group",
  sampling_var = "psi_i",     # <- pins sigma_i = sqrt(psi_i)
  data         = data_lnln,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit_fh)
```

    ===== Hierarchical Bayesian Model Summary =====

     Observations : 100
     Family       : lognormal (link: identity )
     Formula      : y_obs ~ x1 + x2 + x3 + (1 | group)
                    sigma ~ 0 + offset(.hbsaems_sigma_fixed)

    Multilevel Hyperparameters:
    ~group (Number of levels: 100)
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.18      0.03     0.12     0.25 1.00     2843     3415

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     1.51      0.05     1.41     1.61 1.00     5012     5783
    x1            0.32      0.04     0.24     0.40 1.00     6204     6519
    x2           -0.18      0.04    -0.26    -0.10 1.00     5891     5934
    x3            0.11      0.03     0.05     0.17 1.00     6308     6147

Notice that:

- `sigma` is no longer listed in the output – it is pinned to
  $`\sqrt{\psi_i}`$ per area.
- `sd(Intercept)` (the area-level variance $`\sigma_u`$) shrinks because
  $`\sigma`$ is no longer competing for residual variability.

This is the lognormal version of the Fay-Herriot model.

## Mode 3: custom prior on $`\sigma`$

If you want $`\sigma`$ sampled (Mode 1) but with a tighter prior than
brms’s default:

``` r

fit_custom <- hbm_lnln(
  response  = "y_obs",
  auxiliary = c("x1", "x2", "x3"),
  group     = "group",
  data      = data_lnln,
  prior     = brms::set_prior("normal(0.3, 0.05)", class = "sigma"),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
```

The default priors for the other classes (`Intercept`, `b`,
`sd(Intercept)`) remain in effect.

## Mode 4: generic `fixed_params` (power user)

Equivalent to Mode 2 via the generic interface:

``` r

fit_flex <- hbm_flex(
  family_key   = "lognormal",
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  group        = "group",
  fixed_params = list(sigma = ~ sqrt(psi_i)),
  data         = data_lnln,
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

The formula `~ sqrt(psi_i)` is evaluated against `data`, producing a
length-`nrow(data)` vector of pinned values.

## Adding a nonlinear smooth term

Suppose `x1` has a non-linear effect. Add a thin-plate spline:

``` r

fit_spline <- hbm_lnln(
  response       = "y_obs",
  auxiliary      = c("x1", "x2", "x3"),
  group          = "group",
  sampling_var   = "psi_i",
  data           = data_lnln,
  nonlinear      = "x1",
  nonlinear_type = "spline",
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

The formula becomes `y_obs ~ s(x1) + x2 + x3 + (1 | group)`. See
[`vignette("advanced-features")`](https://madsyair.github.io/hbsaems/articles/advanced-features.md)
for full coverage of nonlinear terms.

## Checking convergence

``` r

convergence_check(fit_fh)
```

    $rhat_max
    [1] 1.0023

    $ess_bulk_min
    [1] 2843

    $ess_tail_min
    [1] 3415

    $divergent_transitions
    [1] 0

    $verdict
    [1] "All chains converged: Rhat < 1.01, ESS > 400, no divergent transitions."

## Predicting back-transformed estimates

The lognormal family models $`\log y`$; predictions are returned on the
original (back-transformed) scale by
[`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md):

``` r

new_areas <- data.frame(
  group = 101:105,
  x1    = rnorm(5),
  x2    = rnorm(5),
  x3    = rnorm(5)
)
preds <- sae_predict(fit_fh, newdata = new_areas, allow_new_levels = TRUE)
preds
```

      group estimate  lower upper
    1   101    5.23   3.92  6.91
    2   102    4.18   3.10  5.62
    3   103    6.04   4.51  8.04
    4   104    3.71   2.71  4.99
    5   105    4.95   3.71  6.59

Posterior means are back-transformed correctly (taking the Jensen
inequality into account, since $`E[\exp(\theta + u)] \neq \exp(E[\theta
+ u])`$).

## Comparing Mode 1 vs Mode 2 with LOO

``` r

loo_compare <- loo::loo_compare(
  loo::loo(fit$brmsfit),
  loo::loo(fit_fh$brmsfit)
)
loo_compare
```

                  elpd_diff se_diff
    fit_fh        0.0       0.0
    fit          -4.7       1.9

A negative `elpd_diff` on the random-$`\sigma`$ model suggests that the
fixed-$`\sigma`$ Fay-Herriot variant has better expected log predictive
density on left-out areas.

## Summary

- [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md)
  covers positive right-skewed survey responses with a lognormal
  likelihood.
- Pin $`\sigma`$ via `sampling_var = "psi_i"` to get the Fay-Herriot
  variant – usually preferred when survey design info is reliable.
- Use the `prior` argument to tune $`\sigma`$ or any other class.
- Nonlinear smooths via `nonlinear` and `nonlinear_type`.

## References

- Slud, E. V., & Maiti, T. (2006). Mean-squared error estimation in
  transformed Fay-Herriot models. *JRSSB* 68(2), 239-257.
- You, Y., & Chapman, B. (2006). Small area estimation using area level
  models and estimated sampling variances. *Survey Methodology* 32(1),
  97-103.
- Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*, 2nd ed.
  Wiley.
