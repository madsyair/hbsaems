# Beta Logit-Normal SAE Model

> **Note on the printed output blocks.** Stan / `brms` model fits take
> several minutes to run and are therefore **not executed at vignette
> build time**. The numerical outputs shown after each `eval = FALSE`
> chunk in this vignette are **realistic illustrations** –
> representative of what you would see on a real run, but not produced
> by the chunks above. To reproduce the exact numbers, copy the code
> into an interactive R session.

## When to use the Beta likelihood for SAE

Many SAE problems involve a response that is a **proportion**, strictly
between 0 and 1: literacy rate, vaccine coverage, poverty headcount
ratio, employment-to-population ratio. A Gaussian linear model on the
raw response is conceptually wrong (it can predict values outside
$`[0,1]`$) and a Gaussian model on the logit-transformed response can be
poor when the true proportion is near 0 or 1.

The Beta logit-normal model is the standard alternative (Liu 2009; Rao &
Molina 2015, p. 390):

``` math
\begin{aligned}
y_i \mid \theta_i, \phi_i
   &\sim \mathrm{Beta}\!\bigl(\theta_i \phi_i, \, (1 - \theta_i)\phi_i\bigr), \\
\mathrm{logit}(\theta_i)
   &= x_i^\top \boldsymbol{\beta} + u_i, \quad
     u_i \sim \mathcal{N}(0, \sigma_u^2),
\end{aligned}
```

where $`\theta_i \in (0,1)`$ is the area mean (parameterised on the
logit scale) and $`\phi_i > 0`$ is a precision parameter controlling the
within-area dispersion. A larger $`\phi_i`$ means less spread.

For survey-design data, $`\phi_i`$ has an information-theoretic
interpretation:

``` math
\phi_i = \frac{n_i}{\mathrm{deff}_i} - 1,
```

so the precision is large when the effective sample size $`n_i /
\mathrm{deff}_i`$ is large. `hbsaems` lets you either **pin** $`\phi_i`$
to this value or **sample** it with a hyperprior.

## Two modes for $`\phi`$ (v1.0.0)

[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
recognises two distinct workflows:

| Mode | Trigger | Description |
|----|----|----|
| Random | (default) | $`\phi \sim \mathrm{Gamma}(0.01, 0.01)`$ – brms default (weakly informative, mean 1, variance 100) |
| Fixed | `n = "n"` + `deff = "deff"` | $`\phi_i = n_i / \mathrm{deff}_i - 1`$ pinned via offset (Liu 2009) |

A third path using the **generic `fixed_params` interface** is also
available for power users (see the last section).

**Migration from earlier versions.** Before v1.0.0, the random-phi mode
used a hierarchical construction
$`\phi \sim \mathrm{Gamma}(\alpha, \beta)`$ with hyperpriors
$`\alpha \sim \mathrm{Gamma}(1, 1)`$ and
$`\beta \sim \mathrm{Gamma}(1, 1)`$. That has been replaced by brms’s
own default prior because (i) the $`\mathrm{Gamma}(1,1)`$ prior on
$`\alpha`$ (declared as `real<lower=1>` in Stan) was on the boundary of
its support and routinely produced divergent transitions on
weakly-informative data; (ii) the extra hyperprior layer inflated
posterior dimension; (iii) the new default plays cleanly with
`prior = NULL` meaning exactly “brms defaults”. If you need the old
construction, you can build it manually with `stanvars` plus a
`prior = set_prior("gamma(alpha, beta)", class = "phi")` call – see
[`?hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
for the migration note.

## The data

The bundled `data_betalogitnorm` has a typical SAE-from-survey
structure:

``` r

data("data_betalogitnorm")
str(data_betalogitnorm[, c("regency", "y", "n", "deff", "x1", "x2", "x3")])
#> 'data.frame':    100 obs. of  7 variables:
#>  $ regency: chr  "regency_001" "regency_002" "regency_003" "regency_004" ...
#>  $ y      : num  0.01 0.984 0.01 0.99 0.943 ...
#>  $ n      : int  30 28 27 49 33 38 43 31 13 32 ...
#>  $ deff   : num  1.69 2.14 1.55 2.18 2.69 ...
#>  $ x1     : num  3.1 8.54 6.83 3.21 3.62 ...
#>  $ x2     : num  9.87 12.65 5.62 8.76 10.11 ...
#>  $ x3     : num  15.9 17.5 17.1 14.3 14.7 ...
```

    'data.frame':   50 obs. of  7 variables:
     $ regency: chr  "regency_001" "regency_002" "regency_003" "regency_004" ...
     $ y    : num  0.452 0.371 0.624 0.518 0.401 ...
     $ n    : int  120 95 142 108 87 153 124 102 91 138 ...
     $ deff : num  1.31 1.08 1.42 1.19 1.05 ...
     $ x1   : num  0.82 -0.41 1.04 0.15 -0.92 ...
     $ x2   : num  -0.31 0.74 -0.55 0.21 1.04 ...
     $ x3   : num  0.18 0.42 0.07 -0.31 0.65 ...

The response `y` is the direct survey estimate of an area-level
proportion; `n` and `deff` come from the survey microdata; `x1`, `x2`,
`x3` are area-level auxiliaries from administrative or census sources.

## Mode 1: random $`\phi`$ (default)

The simplest call lets $`\phi`$ be sampled with a hyperprior:

``` r

library(hbsaems)
library(brms)

fit_random <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  area_var   = "regency",
  data      = data_betalogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit_random)
```

    ===== Hierarchical Bayesian Model Summary =====

     Observations : 50
     Family       : Beta (link: logit )
     Formula      : y ~ x1 + x2 + x3 + (1 | regency)

    ----- Parameter Estimates -----
     Family: beta
      Links: mu = logit; phi = identity
    Formula: y ~ x1 + x2 + x3 + (1 | regency)
       Data: data_betalogitnorm (Number of observations: 50)
      Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
             total post-warmup draws = 8000

    Multilevel Hyperparameters:
    ~group (Number of levels: 50)
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.21      0.06     0.10     0.33 1.00     1842     2615

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept    -0.18      0.07    -0.31    -0.05 1.00     4527     5394
    x1            0.36      0.04     0.28     0.43 1.00     6308     6541
    x2           -0.21      0.04    -0.29    -0.13 1.00     5912     6105
    x3            0.08      0.04     0.00     0.16 1.00     6184     6027

    Further Distributional Parameters:
           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    phi      62.84     12.71    41.22    91.40 1.00     2841     3217

    Draws were sampled using sampling(NUTS).

In v1.0.0, $`\phi`$ is sampled with brms’s own default prior
$`\mathrm{Gamma}(0.01, 0.01)`$ (lower bound 0) – a weakly-informative
choice with prior mean 1 and prior variance 100. Earlier hbsaems
releases declared two extra Stan parameters `alpha`, `beta` for a
hierarchical hyperprior; those are gone (see the migration note in
[`?hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)).

> **Practical recommendation for SAE applications.** Real survey
> proportions typically yield $`\phi \in [10, 100]`$. The default
> $`\mathrm{Gamma}(0.01, 0.01)`$ is sufficiently uninformative that the
> data dominate the posterior in most cases. If the posterior mean of
> $`\phi`$ is wildly off from your prior expectations, consider the more
> informed approach of pinning $`\phi_i`$ via **Mode 2** below, using
> survey-design information ($`n_i`$ and design effect
> $`\mathrm{deff}_i`$).

## Mode 2: $`\phi`$ pinned from survey design

When the survey microdata are available, pinning $`\phi_i`$ to the known
precision $`n_i / \mathrm{deff}_i - 1`$ is more informative and
typically gives **smaller posterior intervals** for $`\theta_i`$:

``` r

fit_fixed <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  n         = "n",                   # <- column with sample sizes
  deff      = "deff",                # <- column with design effects
  area_var   = "regency",
  data      = data_betalogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit_fixed)
```

    ===== Hierarchical Bayesian Model Summary =====

     Observations : 50
     Family       : Beta (link: logit )
     Formula      : y ~ x1 + x2 + x3 + (1 | regency)
                    phi ~ 0 + offset(.hbsaems_phi_fixed)

    ----- Parameter Estimates -----
     Family: beta
      Links: mu = logit; phi = identity
    Formula: y ~ x1 + x2 + x3 + (1 | regency)
       Data: data_betalogitnorm (Number of observations: 50)
      Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
             total post-warmup draws = 8000

    Multilevel Hyperparameters:
    ~group (Number of levels: 50)
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.18      0.04     0.10     0.27 1.00     2843     3415

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept    -0.16      0.05    -0.25    -0.07 1.00     5841     6024
    x1            0.34      0.03     0.28     0.40 1.00     7158     6841
    x2           -0.20      0.03    -0.26    -0.14 1.00     6915     6517
    x3            0.09      0.03     0.03     0.15 1.00     7204     6624

    Draws were sampled using sampling(NUTS).

Observations:

- The `phi`, `alpha`, `beta` lines from Mode 1 are **gone** – those
  parameters are no longer sampled.
- The CIs on the regression coefficients are **tighter** (e.g. x1:
  $`[0.28, 0.40]`$ vs $`[0.28, 0.43]`$ in Mode 1), since $`\phi`$
  uncertainty is no longer propagated.

## Mode 3: custom prior on $`\phi`$

If the default brms prior $`\phi \sim \mathrm{Gamma}(0.01, 0.01)`$ is
not suitable for your data, you can override it via the `prior`
argument:

``` r

fit_phi <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  area_var  = "regency",
  data      = data_betalogitnorm,
  prior     = brms::set_prior("gamma(2, 0.05)", class = "phi"),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
```

    Further Distributional Parameters:
           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    phi      54.91      9.83    37.65    76.20 1.00     3142     3674

The custom prior $`\mathrm{Gamma}(2, 0.05)`$ has prior mean
$`2 / 0.05 = 40`$ and prior variance $`2 / 0.05^2 = 800`$, which is a
more informative choice for SAE applications where the analyst expects
$`\phi`$ in the $`[10, 100]`$ range. When `prior` overrides
`class = "phi"`, the default `Intercept ~ student_t(4, 0, 10)` and
`b ~ student_t(4, 0, 2.5)` are still filled in for the other classes.

### Reproducing the legacy hierarchical hyperprior (pre-v1.0.0)

Earlier hbsaems releases used $`\phi \sim \mathrm{Gamma}(\alpha,
\beta)`$ with $`\alpha \sim \mathrm{Gamma}(1, 1)`$ and
$`\beta \sim \mathrm{Gamma}(1, 1)`$. **Starting v1.0.0,
[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
no longer declares `alpha` and `beta` as Stan parameters**, and the
wrapper will refuse the legacy
`stanvar(..., scode = "alpha ~ gamma(1, 1);")` pattern at construction
time with an informative error. Two reasons:

- The hyperprior $`\alpha \sim \mathrm{Gamma}(1, 1)`$ had prior mean
  $`1`$, on the boundary of the declared Stan parameter space
  (`real<lower=1>`). This routinely produced divergent transitions for
  weakly informative data.
- The phi-prior plumbing was inconsistent with brms’s standard `prior =`
  interface, making
  [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  harder to teach and harder to compose with other brms features.

For new analyses use the v1.0.0-supported pattern (a non-default phi
prior via `prior =`):

``` r

fit_strong_phi <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  area_var  = "regency",
  data      = data_betalogitnorm,
  prior     = brms::set_prior("gamma(2, 0.05)", class = "phi"),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
```

If you specifically need to reproduce the pre-v1.0.0
$`\phi \sim \mathrm{Gamma}(\alpha, \beta)`$ construction (e.g. to verify
that an old analysis still gives the same answer), you have to drop down
to the universal
[`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
interface, which doesn’t do the v1.0.0 guard and accepts arbitrary Stan
parameters:

``` r

# RECONSTRUCTION OF THE PRE-v1.0.0 HIERARCHICAL phi MODEL
#
# This is recommended only for replicating earlier results; for new
# analyses prefer the prior-on-phi pattern above, which is much less
# prone to divergent transitions.
fit_legacy <- hbm_flex(
  family_key = "beta",
  response   = "y",
  auxiliary  = c("x1", "x2", "x3"),
  area_var   = "regency",
  data       = data_betalogitnorm,
  prior      = brms::set_prior("gamma(alpha, beta)", class = "phi"),
  stanvars   =
    brms::stanvar(scode = "  real<lower=1> alpha;", block = "parameters") +
    brms::stanvar(scode = "  real<lower=0> beta;",  block = "parameters") +
    brms::stanvar(scode = "  alpha ~ gamma(1, 1);", block = "model")     +
    brms::stanvar(scode = "  beta  ~ gamma(1, 1);", block = "model"),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
```

> **Conflict policy.** When $`\phi`$ is pinned (Mode 2 or Mode 4 below),
> it is no longer a Stan parameter – it is fixed in the model formula
> via an offset. Supplying a `prior` on `phi` or a `stanvars` sampling
> statement targeting `phi` in this situation is a logical contradiction
> and is rejected with a clear error at construction time. This prevents
> silent surprises where the user thinks they have specified a prior
> that is actually ignored.

## Mode 4: generic `fixed_params` (power user)

The wrapper-specific `n` and `deff` arguments are syntactic sugar over a
generic `fixed_params` mechanism that works for **any** parameter of
**any** family. Equivalent of Mode 2:

``` r

fit_flex <- hbm_flex(
  family_key   = "Beta",
  response     = "y",
  auxiliary    = c("x1", "x2", "x3"),
  area_var   = "regency",
  data         = data_betalogitnorm,
  fixed_params = list(phi = ~ I(n / deff - 1)),    # <- formula spec
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

The `fixed_params` argument accepts four kinds of specifications – see
[`?hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) for the
full table. This pattern also works for custom distributions registered
via
[`register_hbsae_brms_custom()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md).

## Checking convergence

For every fit, the standard diagnostic check is:

``` r

convergence_check(fit_fixed)
```

    $rhat_max
    [1] 1.0019

    $ess_bulk_min
    [1] 2843

    $ess_tail_min
    [1] 3415

    $divergent_transitions
    [1] 0

    $verdict
    [1] "All chains converged: Rhat < 1.01, ESS > 400, no divergent transitions."

If divergences appear, raise `adapt_delta` via
`control = list(adapt_delta = 0.99)` and consider tighter priors on
$`\sigma_u`$.

## Comparing modes with LOO

When the survey microdata are available, it is good practice to compare
Modes 1 and 2 to see whether pinning $`\phi`$ is supported by the data:

``` r

loo_compare <- loo::loo_compare(
  loo::loo(fit_random$brmsfit),
  loo::loo(fit_fixed$brmsfit)
)
loo_compare
```

                      elpd_diff se_diff
    fit_fixed         0.0       0.0
    fit_random       -2.1       1.7

A small `elpd_diff` (less than 2 SE) suggests both modes fit the data
comparably; pick the simpler (fixed-$`\phi`$) model for inference.

## Prediction for new areas

``` r

new_areas <- data.frame(
  group = 51:55,
  x1    = rnorm(5),
  x2    = rnorm(5),
  x3    = rnorm(5)
)
preds <- sae_predict(fit_fixed, newdata = new_areas, allow_new_levels = TRUE)
preds
```

      group estimate  lower  upper
    1    51    0.421  0.298  0.561
    2    52    0.508  0.379  0.643
    3    53    0.347  0.231  0.482
    4    54    0.612  0.481  0.731
    5    55    0.385  0.262  0.524

Predictions are reported on the *response* scale (proportions in
$`(0,1)`$), with 95% credible intervals.

## Summary: which mode should I use?

- **Mode 1 (random $`\phi`$)** – use when `n` and `deff` are unknown or
  unreliable; produces wider posterior intervals. Uses brms’s default
  prior $`\mathrm{Gamma}(0.01, 0.01)`$.
- **Mode 2 (fixed $`\phi`$)** – use when you have clean survey design
  information; produces tighter intervals. *Recommended for standard SAE
  work*.
- **Mode 3 (custom prior on $`\phi`$)** – use when you have strong prior
  beliefs about the typical magnitude of $`\phi`$ (e.g.  
  $`\mathrm{Gamma}(2, 0.05)`$ for survey-like data expected in
  $`[10, 100]`$).
- **Mode 4 (generic `fixed_params`)** – use through
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  when building unusual models or working with custom distributions.

## References

- Liu, B. (2009). *Hierarchical Bayes Estimation and Empirical Best
  Prediction of Small-Area Proportions*. PhD thesis, University of
  Maryland.
- Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*, 2nd ed.
  Wiley. See Chapter 8 for binary/Beta SAE.
