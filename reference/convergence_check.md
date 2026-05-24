# MCMC Convergence Diagnostics for Fitted HBMs

Computes a battery of convergence tests and diagnostic plots for an
`hbmfit` object. This is the primary convergence diagnostic function in
hbsaems (supersedes the deprecated
[`hbcc`](https://madsyair.github.io/hbsaems/reference/deprecated.md)).

## Usage

``` r
convergence_check(
  model,
  diag_tests = c("rhat", "geweke", "heidel", "raftery"),
  plot_types = c("trace", "dens", "acf", "nuts_energy", "rhat", "neff"),
  ...
)
```

## Arguments

- model:

  An `hbmfit` or `brmsfit` object.

- diag_tests:

  Character vector of tests to run. Any subset of
  `c("rhat", "geweke", "heidel", "raftery")`.

- plot_types:

  Character vector of plot types to generate. Any subset of
  `c("trace", "dens", "acf", "nuts_energy", "rhat", "neff")`.

- ...:

  Additional arguments passed to the underlying brms or coda routines.

## Value

An `hbcc_results` object containing:

- `rhat_ess`:

  Matrix with columns `Rhat`, `Bulk_ESS`, `Tail_ESS`.

- `geweke,raftery,heidel`:

  Outputs from the corresponding coda routines, or `NULL` when the test
  fails.

- `plots`:

  Named list of `ggplot`/`bayesplot` objects.

## Details

For each parameter \\\theta_j\\, the Gelman-Rubin statistic is computed
as \$\$\widehat{R}\_j =
\sqrt{\frac{\widehat{\mathrm{var}}^+(\theta_j)}{W_j}}\$\$ where \\W_j\\
is the within-chain variance and \\\widehat{\mathrm{var}}^+\\ is the
marginal posterior variance estimate. Values close to 1 (typically below
1.1) indicate convergence.

## See also

[`is_converged`](https://madsyair.github.io/hbsaems/reference/is_converged.md),
[`diagnostic_summary`](https://madsyair.github.io/hbsaems/reference/diagnostic_summary.md),
[`hbm_warnings`](https://madsyair.github.io/hbsaems/reference/hbm_warnings.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1 + x2 + x3),
             data   = data_fhnorm,
             re     = ~ (1 | regency),    # area-level random effect
             chains = 2, iter = 2000, warmup = 1000,
             cores  = 1, seed = 123, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')

diag <- convergence_check(model)
#> Error: object 'model' not found
summary(diag)
#> Error in object[[i]]: object of type 'closure' is not subsettable
is_converged(model)
#> Error: object 'model' not found
is_converged(model, threshold = 1.05)
#> Error: object 'model' not found
# }
```
