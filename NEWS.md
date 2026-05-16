# hbsaems 1.0.0

First stable release.  `hbsaems` fits **area-level** Hierarchical
Bayesian Small Area Estimation models following the methodological
tradition of Rao and Molina (2015) <doi:10.1002/9781118735855>, with
computational implementation adapted to the parameterisation and
prior-specification conventions of the 'brms' package
(Buerkner 2017 <doi:10.18637/jss.v080.i01>) and Stan back-end.  The
package is designed to support the principled Bayesian workflow
advocated by Gelman et al.\ (2020)
<doi:10.48550/arXiv.2011.01808> -- prior predictive checks, MCMC
convergence diagnostics, posterior predictive checks, leave-one-out
cross-validation, Bayesian model comparison and averaging, prior
sensitivity analysis, and design-consistent benchmarking are all part
of the standard pipeline.

This entry consolidates the changelog for every development cycle
since the 0.1.2 maintenance release; the package now follows semantic
versioning and the 1.0.0 line constitutes a stable user-facing function set whose signatures will not change before v2.0.0.

## New features

* **Generic `fixed_params` mechanism** in `hbm()` and `hbm_flex()`.
  Lets the user pin any distributional parameter to a value derived
  from a column, scalar, vector, or formula evaluated against the
  data.  Centralises a pattern that previously had to be coded
  per-wrapper.
* **`hbm_betalogitnorm()` refactor.**  Four operational modes:
  random $\phi$ with hyperprior; fixed $\phi$ from survey
  $n$ and $\mathrm{deff}$; user-overridable hyperprior on
  $\alpha,\beta$ via `stanvars`; and generic `fixed_params` for
  power users.  Default priors filled in automatically and follow
  Liu (2009).
* **`hbm_lnln(sampling_var = ...)`** for the Fay--Herriot lognormal
  model.  Pins $\sigma_i = \sqrt{\psi_i}$ from a known per-area
  sampling variance.
* **Custom brms response distributions.**  Built-in support for
  Loglogistic and Shifted Loglogistic, plus a public extension
  framework (`register_hbsae_brms_custom()`,
  `read_stan_function()`, `build_brms_custom_family()`).  Stan code
  lives in `inst/stan/` as plain `.stan` files.  Each registered
  family ships log-likelihood, posterior-predict and posterior-epred
  hooks so `loo()`, `posterior_predict()`, and
  `posterior_epred()` work out of the box.
* **Spatial random effects.**  CAR (ICAR / proper / BYM2) and SAR
  (lag / error) via `sre`, `sre_type`, `car_type`, `sar_type`, and
  `M`.  Weight matrices can be constructed with the bundled
  `build_spatial_weight()` from a shapefile or coordinates.
* **Missing-data handling.**  Three strategies (`deleted`,
  `multiple` via mice, `model` via `brms::mi()`) with auto-selection
  when `handle_missing = NULL`.
* **Shrinkage priors.**  Horseshoe (regularised, Piironen & Vehtari
  2017) and R2D2 (Zhang et al.\ 2022) selectable via `prior_type`.
* **Nonlinear smooth terms.**  Thin-plate splines and Gaussian
  processes via the `nonlinear`/`nonlinear_type` arguments.
* **Bilingual Shiny dashboard** (`run_sae_app()`).  English /
  Indonesian, dedicated spatial setup tab, in-app code preview, and
  CSV / RDS data upload.  Source under
  `inst/shiny/sae_app/`.
* **Benchmarking helpers** (`sae_benchmark()`,
  `sae_predict()`).  Pfeffermann-style design-consistent benchmarking
  and out-of-sample prediction for unsampled areas.

## Naming and interface changes (breaking only at v2.0.0)

* **Argument rename: `predictors` -> `auxiliary`**.  The new name
  aligns with Small Area Estimation literature (Rao & Molina 2015,
  Pfeffermann 2013).  Old usage continues to work with a one-time
  soft-deprecation warning and is **scheduled for removal in
  v2.0.0**.
* **Argument rename: `hbm_generic()` -> `hbm_flex()`**.  Old name
  removed.
* **Deprecated wrappers (legacy v0.1.x):** `hbcc()`, `hbmc()`,
  `hbpc()`, `hbsae()`.  All four emit a soft-deprecation warning
  pointing at `hbm()`, `convergence_check()`, `prior_check()`,
  `sae_aggregate()`, or `sae_predict()` respectively.
  **Scheduled for removal in v2.0.0**.

## Documentation

* **Seven new vignettes** covering the full workflow:
  `hbsaems-modelling` (overview), `hbsaems-lnln-model`,
  `hbsaems-betalogitnorm-model`, `hbsaems-binlogitnorm-model`,
  `hbsaems-spatial`, `hbsaems-handle-missing`,
  `hbsaems-run_sae_app`.  All follow a CRAN-safe pattern: heavy
  Stan fits are not evaluated at build time; representative outputs
  are printed as illustrations with an explicit disclaimer.
* **Three retained vignettes**: `complete-workflow`,
  `advanced-features`, `migration-guide`.

## Bug fixes

* **`posterior_interval()` and `prior_draws()` now re-export the
  upstream generics** from \pkg{rstantools} and \pkg{brms}
  respectively, rather than defining new generics with conflicting
  signatures.  This fixes a name-collision crash that occurred when
  \pkg{brms} was attached together with \pkg{hbsaems} and the user
  called \code{posterior_interval(fit)} on an \code{hbmfit} object:
  the error message
  \emph{"For the default method 'object' should be a matrix"} no
  longer occurs.  The fix follows the standard R package design
  pattern used by \pkg{brms} itself.

## Internal

* All exported functions documented with full `roxygen2` blocks
  including `@param`, `@return`, `@examples`, and references where
  appropriate.
* Custom-distribution Stan code stored as separate `.stan` files in
  `inst/stan/` (one source of truth, syntax highlighting, no
  string-escaping noise).
* Test suite reorganised into CRAN-safe unit tests
  (`tests/testthat/`) and heavy integration tests
  (`tests/testthat/dev-tests/`, gated by `skip_on_cran()` and
  excluded via `.Rbuildignore`).

# hbsaems 0.1.0

* Initial public release on CRAN.  Provided `hbm()`, `hbcc()`,
  `hbmc()`, `hbpc()`, `hbsae()`, and three example datasets
  (`data_fhnorm`, `data_lnln`, `data_betalogitnorm`,
  `data_binlogitnorm`).
