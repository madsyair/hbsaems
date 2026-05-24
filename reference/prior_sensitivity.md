# Power-Scale Prior Sensitivity Diagnostics for Fitted HBMs

Computes prior and likelihood power-scaling sensitivity diagnostics for
a fitted `hbmfit` model using the priorsense package. Useful for
assessing whether posterior conclusions are driven by the prior or the
data – a critical step in any principled Bayesian SAE workflow.

## Usage

``` r
prior_sensitivity(model, ...)
```

## Arguments

- model:

  An `hbmfit` object returned by
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) (or one
  of its wrappers) or a `brmsfit` object directly.

- ...:

  Additional arguments forwarded to
  [`priorsense::powerscale_sensitivity()`](https://n-kall.github.io/priorsense/reference/powerscale-sensitivity.html),
  e.g.\\ `variable = c("b_x1", "sd_regency__Intercept")` to restrict the
  report to specific parameters.

## Value

A `powerscale_sensitivity_summary` object (data frame) with one row per
monitored parameter and columns `variable`, `prior`, `likelihood`,
`diagnosis`. `NULL` (with a message) when the priorsense package is not
installed.

## Details

Prior sensitivity analysis answers the question: “If I had used a
slightly different prior, would the substantive conclusions change?”.
The power-scaling approach of Kallioinen et al.\\ (2023) detects:

- **Prior–likelihood conflict**: the posterior moves non-negligibly when
  the prior is up- or down-weighted. Often indicates an overly
  informative or misspecified prior.

- **Weak likelihood**: the posterior is dominated by the prior. Common
  in SAE for areas with few sampled units.

Reported diagnostics include the Kullback–Leibler divergence between the
original posterior and the power-scaled posterior (`prior`,
`likelihood`) and a categorical flag (`prior-data conflict`,
`strong prior`, `-`).

**Computational cost.** No re-sampling is required: importance sampling
reuses the existing posterior draws. Hence a typical run costs only a
few seconds even for large hierarchical models.

## When to run prior sensitivity

Always. Specifically:

- After every model fit, before drawing substantive conclusions.

- Whenever convergence diagnostics from
  [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
  are clean but the posterior seems implausibly narrow or implausibly
  wide.

- When comparing models with shrinkage priors – horseshoe and R2D2 are
  both informative, and small differences in their hyperparameters can
  move estimates noticeably.

## References

Kallioinen, N., Paananen, T., Burkner, P.-C., & Vehtari, A.\\ (2024).
Detecting and diagnosing prior and likelihood sensitivity with
power-scaling. *Statistics and Computing*, 34, 57.
[doi:10.1007/s11222-023-10366-5](https://doi.org/10.1007/s11222-023-10366-5)

## See also

[`prior_check`](https://madsyair.github.io/hbsaems/reference/prior_check.md),
[`convergence_check`](https://madsyair.github.io/hbsaems/reference/convergence_check.md),
[`model_compare`](https://madsyair.github.io/hbsaems/reference/model_compare.md)

## Examples

``` r
# \donttest{
if (requireNamespace("priorsense", quietly = TRUE)) {
  data("data_fhnorm")
  fit <- hbm(brms::bf(y ~ x1 + x2),
             data = data_fhnorm, re = ~(1 | regency),
             chains = 2, iter = 1000, refresh = 0)
  ps  <- prior_sensitivity(fit)
  print(ps)
}
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')
# }
```
