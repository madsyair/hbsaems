# Changelog

All notable changes to **hbsaems** are documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

> **Note.** For the CRAN-formatted version of the changelog, see
> [`NEWS.md`](https://madsyair.github.io/hbsaems/NEWS.md). This file
> (`CHANGELOG.md`) is the GitHub release notes companion.

------------------------------------------------------------------------

## [1.0.0](https://github.com/madsyair/hbsaems/releases/tag/v1.0.0) — 2026-05-14

First stable release. The package now follows semantic versioning; the
1.0.0 line constitutes a stable user-facing function set whose
signatures will not change before v2.0.0.

`hbsaems` fits **area-level** Hierarchical Bayesian Small Area
Estimation models following the methodological tradition of Rao and
Molina (2015)
[doi:10.1002/9781118735855](https://doi.org/10.1002/9781118735855), with
computational implementation adapted to the parameterisation and
prior-specification conventions of the `brms` package (Bürkner, 2017)
[doi:10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01) and
Stan back-end. The package is designed to support the principled
Bayesian workflow advocated by Gelman et al. (2020)
[doi:10.48550/arXiv.2011.01808](https://doi.org/10.48550/arXiv.2011.01808).

This release consolidates the changelog for every development cycle
since the 0.1.2 maintenance release.

### Added

#### Generic `fixed_params` mechanism

- New `fixed_params` argument in
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  lets users pin any distributional parameter to a value derived from a
  column, scalar, vector, or formula evaluated against the data
- Centralises a pattern that previously had to be coded per-wrapper
- Each pinned parameter `<par>` is threaded into the brms formula as
  `<par> ~ 0 + offset(.hbsaems_<par>_fixed)`
- Works with custom brms families too

#### Refactored `hbm_betalogitnorm()` with four operational modes

| Mode | Trigger | Description |
|----|----|----|
| 1\. Random φ | (default) | $`\phi \sim \mathrm{Gamma}(\alpha, \beta)`$ with $`\alpha, \beta`$ given hyperpriors |
| 2\. Fixed φ | `n = "n"` + `deff = "deff"` | $`\phi_i = n_i / \text{deff}_i - 1`$ pinned via offset |
| 3\. Custom hyperprior | `stanvars = ...` | User overrides default $`\mathrm{Gamma}(1,1)`$ priors on $`\alpha, \beta`$ |
| 4\. Generic | `hbm_flex(fixed_params=...)` | Power-user access for any distributional parameter |

Default priors filled in automatically and follow Liu (2009).

#### `hbm_lnln(sampling_variance = ...)` for Fay-Herriot lognormal

- New `sampling_variance` argument pins $`\sigma_i = \sqrt{\psi_i}`$
  from a known per-area sampling variance
- $`\psi_i`$ assumed on the **log scale**; delta-method conversion
  documented for users with original-scale sampling variance

#### Custom brms response distributions

- Built-in support for **Loglogistic** and **Shifted Loglogistic**
  distributions
- Public extension framework:
  [`register_hbsae_brms_custom()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md),
  [`read_stan_function()`](https://madsyair.github.io/hbsaems/reference/read_stan_function.md),
  [`build_brms_custom_family()`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md)
- Stan code lives in `inst/stan/` as plain `.stan` files (one source of
  truth, syntax highlighting, no string-escaping noise)
- Each registered family ships log-likelihood, posterior-predict and
  posterior-epred hooks so `loo()`, `posterior_predict()`, and
  `posterior_epred()` work out of the box

#### Spatial random effects

- CAR (ICAR / proper / BYM2) and SAR (lag / error) via `spatial_var`,
  `spatial_model`, `car_type`, `sar_type`, and `M`
- Weight matrices can be constructed from a shapefile via
  [`build_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
- Validation via
  [`check_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md)

#### Missing-data handling

- Three strategies: `"deleted"` (listwise deletion), `"multiple"`
  (mice), `"model"` (joint Bayesian via
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html))
- Auto-selection when `handle_missing = NULL` based on missingness
  pattern and family capabilities

#### Bayesian workflow utilities

- [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md)
  — prior predictive checks (replaces
  [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md))
- [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
  — MCMC convergence diagnostics (replaces
  [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md))
- [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md),
  [`model_compare_all()`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md),
  [`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
  — Bayesian model comparison and averaging (replaces
  [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md))
- [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  — out-of-sample prediction with proper uncertainty (replaces
  [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md))
- [`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
  — Pfeffermann-style design-consistent benchmarking
- Prior sensitivity analysis via optional `priorsense` integration

#### Other features

- **Shrinkage priors**: Horseshoe (Piironen & Vehtari, 2017) and R2D2
  (Zhang et al., 2022) selectable via `prior_type`
- **Nonlinear smooth terms**: thin-plate splines and Gaussian processes
  via `nonlinear`/`nonlinear_type` arguments
- **Bilingual Shiny dashboard**
  ([`run_sae_app()`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)):
  English/Indonesian GUI with dedicated spatial setup tab, in-app code
  preview, CSV/RDS data upload

#### Documentation

- **Seven new vignettes**: `hbsaems-modelling` (overview),
  `hbsaems-lnln-model`, `hbsaems-betalogitnorm-model`,
  `hbsaems-binlogitnorm-model`, `hbsaems-spatial`,
  `hbsaems-handle-missing`, `hbsaems-run_sae_app`
- All follow a CRAN-safe pattern: heavy Stan fits not evaluated at build
  time, with representative outputs printed as illustrations and an
  explicit disclaimer
- **Three retained vignettes**: `complete-workflow`,
  `advanced-features`, `migration-guide`

### Changed

- **Argument rename: `predictors` → `auxiliary`** in all wrappers. The
  new name aligns with Small Area Estimation literature (Rao & Molina,
  2015 Ch. 4; Pfeffermann, 2013). Old usage continues to work with a
  one-time soft-deprecation warning. **Will be removed in v2.0.0.**
- **Title**: “Hierarchical Bayesian Small Area Estimation Models” →
  “Hierarchical Bayesian **Area-Level** Small Area Estimation Models” to
  make scope explicit
- Function `hbm_generic()` renamed to
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  (old name removed)
- All exported functions now use SAE-idiomatic `snake_case` names
- Comprehensive conflict checks: refusing prior/stanvars on pinned
  parameters with clear, actionable error messages
- Default priors centralised: `Intercept ~ student_t(4, 0, 10)`,
  `b ~ student_t(4, 0, 2.5)`, `phi ~ gamma(alpha, beta)` (when sampled)
- Test suite reorganised into CRAN-safe unit tests (`tests/testthat/`)
  and heavy integration tests (`tests/testthat/dev-tests/`, gated by
  `skip_on_cran()`)

### Deprecated (removal scheduled for v2.0.0)

The following short-form names continue to work but emit a
soft-deprecation warning:

- [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  → use
  [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
- [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  → use
  [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
- [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  → use
  [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md)
- [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  → use
  [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
- `predictors =` argument → use `auxiliary =` argument

### Fixed

- **[`posterior_interval()`](https://madsyair.github.io/hbsaems/reference/posterior_interval.md)
  and
  [`prior_draws()`](https://madsyair.github.io/hbsaems/reference/prior_draws.md)
  now re-export the upstream generics** from `rstantools` and `brms`
  respectively, rather than defining new generics with conflicting
  signatures. This fixes a name-collision crash that occurred when
  `brms` was attached together with `hbsaems` and the user called
  `posterior_interval(fit)` on an `hbmfit` object: the error message
  *“For the default method ‘object’ should be a matrix”* no longer
  occurs. The fix follows the standard R package design pattern used by
  `brms` itself.
- `merge_betalogitnorm_priors()` now correctly distinguishes between
  pinned and sampled `phi` (no longer fills in a default
  `phi ~ gamma(alpha, beta)` prior when phi is pinned)
- `.process_fixed_params()` validates that resolved values are finite
  and (where appropriate) strictly positive before Stan compilation
- [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  now correctly forwards `fixed_params` to
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md)
- Random-effect formula handling in
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) Section
  9b: warning when no area random effect is detected and no spatial RE
  is configured (helps catch common SAE specification mistakes)
- Spelling/typo cleanup across documentation; `inst/WORDLIST` added for
  [`spelling::spell_check_test()`](https://docs.ropensci.org/spelling//reference/spell_check_package.html)

### Internal

- All exported functions documented with full `roxygen2` blocks
  including `@param`, `@return`, `@examples`, and references where
  appropriate
- Custom-distribution Stan code stored as separate `.stan` files in
  `inst/stan/`
- 499 unit tests passing on CRAN-safe environment (13 new regression
  tests for the S3 generic dispatch fix)
- 0 spelling issues
- New GitHub Actions workflows: `R-CMD-check.yaml` (5-OS matrix),
  `vignettes.yaml` (targeted vignette CI), `pkgdown.yaml` (auto-deploy
  site)
- New `_pkgdown.yml` site configuration

------------------------------------------------------------------------

## [0.1.0](https://github.com/madsyair/hbsaems/releases/tag/v0.1.0) — 2025 (initial CRAN release)

### Added

- Initial public release on CRAN
- Core function
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) for
  Bayesian area-level SAE
- Legacy wrappers:
  [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
- Example datasets: `data_fhnorm`, `data_lnln`, `data_betalogitnorm`,
  `data_binlogitnorm`, `adjacency_matrix_car`, `spatial_weight_sar`
- Basic vignettes: `complete-workflow`, `advanced-features`,
  `migration-guide`

------------------------------------------------------------------------
