# Changelog

All notable changes to **hbsaems** are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

> **Note.** For the CRAN-formatted version of the changelog, see [`NEWS.md`](NEWS.md). This file (`CHANGELOG.md`) is the GitHub release notes companion.

---

## [1.1.0] — 2026-05-31

Minor release with Bayesian-workflow improvements. **Read the behaviour
change below before upgrading production scripts.**

### Changed (BEHAVIOUR CHANGE)

- `sae_predict()` now defaults to `predict_type = "epred"` (posterior of the
  area mean θ_i) instead of the posterior predictive of a new observation.
  This is the correct SAE target; reported `SD` / `RSE_percent` now measure
  uncertainty of the estimator θ_i and are **smaller** than in 1.0.x. Every
  RSE number changes. Pass `predict_type = "response"` to reproduce 1.0.x
  exactly; `"linpred"` returns the linear predictor on the response scale.
  For binomial families `"epred"` returns the area **proportion** p_i (not the
  expected count n_i·p_i, which is not comparable across areas with different
  trial counts) and warns; use `"proportion"` to request it explicitly or
  `"response"` to keep the count.

### Added

- `sae_predict()` gains `predict_type = c("epred", "response", "linpred",
  "proportion")` and a `sae_predict.default()` method that gives a clear error
  for unsupported inputs. `model_average()` now forwards `predict_type` to
  each model; `sae_benchmark()` operates on the predictions you pass in, so it
  honours whichever `predict_type` produced them.
- `hbm_flex()` makes `family` the primary argument: it accepts a string
  (e.g. `"gaussian"`, exactly like the old `family_key`), a brms family object
  (e.g. `gaussian()`, `Gamma(link = "log")`), or a registered custom family
  object. `family_key` is retained as a backward-compatible alias (not
  deprecated).
- `hbm()` gains a matching `family` argument (string or brms family object),
  with `hb_sampling` retained as a backward-compatible alias, so `family` is
  the uniform distribution selector across `hbm()` and `hbm_flex()`.
- Four SAE-oriented brms-native families registered: `gamma` (log link),
  `skew_normal`, `student` (robust Fay-Herriot), and `hurdle_lognormal`,
  each with response-domain validation.
- `convergence_check()` reports divergent transitions, E-BFMI per chain, and
  the max-treedepth hit rate as numbers in `summary()`, and warns on
  divergences or E-BFMI < 0.3.
- `model_compare()` / `model_compare_all()` check Pareto-k for PSIS-LOO
  reliability (`n_high_k` column + warning when any k > 0.7); the previously
  dead `reloo_args` argument now triggers `brms::reloo()`.
- `prior_check()` auto-detects `data` and `response_var` when omitted
  (resolved from the fit / model formula via `brms::brmsterms()`); the
  deprecated `hbpc()` wrapper inherits the same convenience.

### Changed (defaults / validation)

- `hbm()` now defaults to `control = list(adapt_delta = 0.95,
  max_treedepth = 12)` instead of brms's `adapt_delta = 0.8`; hierarchical
  SAE funnels need the higher value. User-supplied `control` is respected.
- `hbm_binlogitnorm()` now errors on missing values in `trials` (the
  binomial likelihood is undefined without a known number of trials).
  Missingness in the response remains supported.
- `convergence_check()` validates `diag_tests` / `plot_types` and errors on
  unknown values instead of silently ignoring them.
- For CAR spatial models, `hbm()` / `hbm_flex()` now validate that the row
  names of the spatial weight matrix `M` match the levels of the spatial
  grouping variable **before** Stan compilation. A mismatch previously
  surfaced as a late brms error during Stan-data preparation; it now fails
  fast with an informative message naming the offending levels (a permutation
  of the levels is still accepted, since brms reorders by name).

### Fixed

- `convergence_check()` no longer reports `Tail_ESS = Bulk_ESS` in the
  fallback path when the 'posterior' package is unavailable (now `NA`).
- `model_compare()` / `hbmc()` accept a `list` of fitted models again via a
  new `model_compare.list()` method (1–2 models → `hbmc_results`; 3+ →
  ranked `hbm_table`; empty/invalid list → informative error).

## [1.0.0] — 2026-05-14

First stable release. The package now follows semantic versioning; the 1.0.0 line constitutes a stable user-facing function set whose signatures will not change before v2.0.0.

`hbsaems` fits **area-level** Hierarchical Bayesian Small Area Estimation models following the methodological tradition of Rao and Molina (2015) [doi:10.1002/9781118735855](https://doi.org/10.1002/9781118735855), with computational implementation adapted to the parameterisation and prior-specification conventions of the `brms` package (Bürkner, 2017) [doi:10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01) and Stan back-end. The package is designed to support the principled Bayesian workflow advocated by Gelman et al. (2020) [doi:10.48550/arXiv.2011.01808](https://doi.org/10.48550/arXiv.2011.01808).

This release consolidates the changelog for every development cycle since the 0.1.2 maintenance release.

### Added

#### Generic `fixed_params` mechanism

- New `fixed_params` argument in `hbm()` and `hbm_flex()` lets users pin any distributional parameter to a value derived from a column, scalar, vector, or formula evaluated against the data
- Centralises a pattern that previously had to be coded per-wrapper
- Each pinned parameter `<par>` is threaded into the brms formula as `<par> ~ 0 + offset(.hbsaems_<par>_fixed)`
- Works with custom brms families too

#### Refactored `hbm_betalogitnorm()` with four operational modes

| Mode | Trigger | Description |
|------|---------|-------------|
| 1. Random φ | (default) | $\phi \sim \mathrm{Gamma}(\alpha, \beta)$ with $\alpha, \beta$ given hyperpriors |
| 2. Fixed φ | `n = "n"` + `deff = "deff"` | $\phi_i = n_i / \text{deff}_i - 1$ pinned via offset |
| 3. Custom hyperprior | `stanvars = ...` | User overrides default $\mathrm{Gamma}(1,1)$ priors on $\alpha, \beta$ |
| 4. Generic | `hbm_flex(fixed_params=...)` | Power-user access for any distributional parameter |

Default priors filled in automatically and follow Liu (2009).

#### `hbm_lnln(sampling_variance = ...)` for Fay-Herriot lognormal

- New `sampling_variance` argument pins $\sigma_i = \sqrt{\psi_i}$ from a known per-area sampling variance
- $\psi_i$ assumed on the **log scale**; delta-method conversion documented for users with original-scale sampling variance

#### Custom brms response distributions

- Built-in support for **Loglogistic** and **Shifted Loglogistic** distributions
- Public extension framework: `register_hbsae_brms_custom()`, `read_stan_function()`, `build_brms_custom_family()`
- Stan code lives in `inst/stan/` as plain `.stan` files (one source of truth, syntax highlighting, no string-escaping noise)
- Each registered family ships log-likelihood, posterior-predict and posterior-epred hooks so `loo()`, `posterior_predict()`, and `posterior_epred()` work out of the box

#### Spatial random effects

- CAR (ICAR / proper / BYM2) and SAR (lag / error) via `spatial_var`, `spatial_model`, `car_type`, `sar_type`, and `M`
- Weight matrices can be constructed from a shapefile via `build_spatial_weight()`
- Validation via `check_spatial_weight()`

#### Missing-data handling

- Three strategies: `"deleted"` (listwise deletion), `"multiple"` (mice), `"model"` (joint Bayesian via `brms::mi()`)
- Auto-selection when `handle_missing = NULL` based on missingness pattern and family capabilities

#### Bayesian workflow utilities

- `prior_check()` — prior predictive checks (replaces `hbpc()`)
- `convergence_check()` — MCMC convergence diagnostics (replaces `hbcc()`)
- `model_compare()`, `model_compare_all()`, `model_average()` — Bayesian model comparison and averaging (replaces `hbmc()`)
- `sae_predict()` — out-of-sample prediction with proper uncertainty (replaces `hbsae()`)
- `sae_benchmark()` — Pfeffermann-style design-consistent benchmarking
- Prior sensitivity analysis via optional `priorsense` integration

#### Other features

- **Shrinkage priors**: Horseshoe (Piironen & Vehtari, 2017) and R2D2 (Zhang et al., 2022) selectable via `prior_type`
- **Nonlinear smooth terms**: thin-plate splines and Gaussian processes via `nonlinear`/`nonlinear_type` arguments
- **Bilingual Shiny dashboard** (`run_sae_app()`): English/Indonesian GUI with dedicated spatial setup tab, in-app code preview, CSV/RDS data upload

#### Documentation

- **Seven new vignettes**: `hbsaems-modelling` (overview), `hbsaems-lnln-model`, `hbsaems-betalogitnorm-model`, `hbsaems-binlogitnorm-model`, `hbsaems-spatial`, `hbsaems-handle-missing`, `hbsaems-run_sae_app`
- All follow a CRAN-safe pattern: heavy Stan fits not evaluated at build time, with representative outputs printed as illustrations and an explicit disclaimer
- **Three retained vignettes**: `complete-workflow`, `advanced-features`, `migration-guide`

### Changed

- **Argument rename: `predictors` → `auxiliary`** in all wrappers. The new name aligns with Small Area Estimation literature (Rao & Molina, 2015 Ch. 4; Pfeffermann, 2013). Old usage continues to work with a one-time soft-deprecation warning. **Will be removed in v2.0.0.**
- **Title**: "Hierarchical Bayesian Small Area Estimation Models" → "Hierarchical Bayesian **Area-Level** Small Area Estimation Models" to make scope explicit
- Function `hbm_generic()` renamed to `hbm_flex()` (old name removed)
- All exported functions now use SAE-idiomatic `snake_case` names
- Comprehensive conflict checks: refusing prior/stanvars on pinned parameters with clear, actionable error messages
- Default priors centralised: `Intercept ~ student_t(4, 0, 10)`, `b ~ student_t(4, 0, 2.5)`, `phi ~ gamma(alpha, beta)` (when sampled)
- Test suite reorganised into CRAN-safe unit tests (`tests/testthat/`) and heavy integration tests (`tests/testthat/dev-tests/`, gated by `skip_on_cran()`)

### Deprecated (removal scheduled for v2.0.0)

The following short-form names continue to work but emit a soft-deprecation warning:

- `hbcc()` → use `convergence_check()`
- `hbmc()` → use `model_compare()`
- `hbpc()` → use `prior_check()`
- `hbsae()` → use `sae_predict()`
- `predictors =` argument → use `auxiliary =` argument

### Fixed

- **`posterior_interval()` and `prior_draws()` now re-export the upstream generics** from `rstantools` and `brms` respectively, rather than defining new generics with conflicting signatures. This fixes a name-collision crash that occurred when `brms` was attached together with `hbsaems` and the user called `posterior_interval(fit)` on an `hbmfit` object: the error message *"For the default method 'object' should be a matrix"* no longer occurs. The fix follows the standard R package design pattern used by `brms` itself.
- `merge_betalogitnorm_priors()` now correctly distinguishes between pinned and sampled `phi` (no longer fills in a default `phi ~ gamma(alpha, beta)` prior when phi is pinned)
- `.process_fixed_params()` validates that resolved values are finite and (where appropriate) strictly positive before Stan compilation
- `hbm_flex()` now correctly forwards `fixed_params` to `hbm()`
- Random-effect formula handling in `hbm()` Section 9b: warning when no area random effect is detected and no spatial RE is configured (helps catch common SAE specification mistakes)
- Spelling/typo cleanup across documentation; `inst/WORDLIST` added for `spelling::spell_check_test()`

### Internal

- All exported functions documented with full `roxygen2` blocks including `@param`, `@return`, `@examples`, and references where appropriate
- Custom-distribution Stan code stored as separate `.stan` files in `inst/stan/`
- 499 unit tests passing on CRAN-safe environment (13 new regression tests for the S3 generic dispatch fix)
- 0 spelling issues
- New GitHub Actions workflows: `R-CMD-check.yaml` (5-OS matrix), `vignettes.yaml` (targeted vignette CI), `pkgdown.yaml` (auto-deploy site)
- New `_pkgdown.yml` site configuration

---

## [0.1.0] — 2025 (initial CRAN release)

### Added

- Initial public release on CRAN
- Core function `hbm()` for Bayesian area-level SAE
- Legacy wrappers: `hbcc()`, `hbmc()`, `hbpc()`, `hbsae()`
- Example datasets: `data_fhnorm`, `data_lnln`, `data_betalogitnorm`, `data_binlogitnorm`, `adjacency_matrix_car`, `spatial_weight_sar`
- Basic vignettes: `complete-workflow`, `advanced-features`, `migration-guide`

