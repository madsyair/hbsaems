# Changelog

## hbsaems 1.1.0

Minor release with Bayesian-workflow improvements. **Read the behaviour
change below before upgrading production scripts.**

### Behaviour change (IMPORTANT)

- **[`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  now defaults to `predict_type = "epred"`** (the posterior distribution
  of the area mean theta_i), replacing the previous
  posterior-predictive-of-a-new-observation behaviour. This is the
  theoretically correct target for small area estimation: the reported
  `SD` / `RSE_percent` now measure the uncertainty of the *estimator*
  theta_i and are therefore **smaller** than in 1.0.x (which included
  observation-level likelihood variance). **Every RSE number your
  scripts produce will change.** To reproduce the old numbers exactly,
  pass `predict_type = "response"`. A third option, `"linpred"`, returns
  the linear predictor on the response scale.

### New features

- Four SAE-oriented families added to the model registry, usable via
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  / [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and
  [`list_hbsae_models()`](https://madsyair.github.io/hbsaems/reference/list_hbsae_models.md):
  **`gamma`** (positive continuous, log link – incomes/expenditures),
  **`skew_normal`** (skewed continuous area means), **`student`**
  (heavy-tailed, robust Fay-Herriot), and **`hurdle_lognormal`**
  (positive continuous with excess zeros). Each carries the appropriate
  response-domain check (e.g. Gamma requires y \> 0; hurdle-lognormal
  allows y \>= 0).

- [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)’s
  `family` argument is now the primary way to choose the family. It
  accepts a string (e.g. `"gaussian"`, exactly like the old
  `family_key`), a brms family object
  (e.g. [`gaussian()`](https://rdrr.io/r/stats/family.html),
  [`binomial()`](https://rdrr.io/r/stats/family.html)), or a registered
  custom family object (e.g. `brms_custom_loglogistic()$custom_family`).
  The family name is matched to a registered spec, so custom
  distributions reuse the same Stan code (stanvars); an unregistered
  custom family raises an informative error. The `family_key` argument
  is retained as a backward-compatible alias (not deprecated). Supply
  either `family` or `family_key`, not both.

- [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) gains a
  `family` argument mirroring
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md):
  pass a string (e.g. `"gaussian"`) or a brms family object
  (e.g. [`gaussian()`](https://rdrr.io/r/stats/family.html),
  `Gamma(link = "log")`); a link carried on the object is honoured when
  `hb_link` is left at its default. `hb_sampling` is retained as a
  backward-compatible alias. Supply either `family` or `hb_sampling`,
  not both. `family` is thus the uniform distribution selector across
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).

- [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  gains the
  `predict_type = c("epred", "response", "linpred", "proportion")`
  argument (see above).
  [`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
  and
  [`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
  honour it. For a **binomial** family,
  [`posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
  returns the expected COUNT (n_i \* p_i), which is not comparable
  across areas with unequal trial counts; `predict_type = "proportion"`
  returns the area proportion p_i, and `predict_type = "epred"` on a
  binomial model now auto-converts to p_i (with a warning). Use
  `"response"` for the count.

- [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
  now reports **divergent transitions**, **E-BFMI** per chain, and the
  **max-treedepth hit rate** as numbers in
  [`summary()`](https://rdrr.io/r/base/summary.html) (not just the
  energy plot), and warns when divergences occur or E-BFMI \< 0.3.

- [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
  /
  [`model_compare_all()`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md)
  now check **Pareto-k** for PSIS-LOO reliability: a `n_high_k` column
  is added to the ranking table and a warning is issued when any k \>
  0.7. The previously dead `reloo_args` argument is now functional –
  supplying it triggers
  [`brms::reloo()`](https://paulbuerkner.com/brms/reference/reloo.brmsfit.html)
  to refit the high-k folds.

### Default change

- [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) now
  defaults to `control = list(adapt_delta = 0.95, max_treedepth = 12)`
  (the values long documented as the recommended starting point) instead
  of brms’s `adapt_delta = 0.8`. Hierarchical SAE funnels need the
  higher value to avoid spurious divergences. User-supplied `control`
  values are always respected. Seed-level results may differ slightly
  from 1.0.x.

### Bug fixes

- [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
  no longer reports `Tail_ESS = Bulk_ESS` in the fallback path when the
  ‘posterior’ package is unavailable; it reports `NA` rather than
  falsely implying the tails were checked.
- [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
  (and the deprecated
  [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md))
  accept a list of fitted models again. When
  [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
  became an S3 generic in 1.0.0 it dispatched only on
  `hbmfit`/`brmsfit`, so the pre-1.0.0 convention
  `hbmc(model = list(m1, m2))` failed with “no applicable method for
  ‘model_compare’ applied to an object of class ‘list’”. A
  `model_compare.list()` method now unwraps a 1-2 element list to the
  single/pairwise path (`hbmc_results`), routes a 3+ element list to
  [`model_compare_all()`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md)
  (ranked `hbm_table`), and rejects an empty or non-model list with an
  informative error.

## hbsaems 1.0.0

CRAN release: 2026-05-25

First stable release. `hbsaems` fits **area-level** Hierarchical
Bayesian Small Area Estimation models following the methodological
tradition of Rao and Molina (2015) <doi:10.1002/9781118735855>, with
computational implementation adapted to the parameterisation and
prior-specification conventions of the ‘brms’ package (Buerkner 2017
<doi:10.18637/jss.v080.i01>) and Stan back-end. The package is designed
to support the principled Bayesian workflow advocated by Gelman et
al. (2020) <doi:10.48550/arXiv.2011.01808> – prior predictive checks,
MCMC convergence diagnostics, posterior predictive checks, leave-one-out
cross-validation, Bayesian model comparison and averaging, prior
sensitivity analysis, and design-consistent benchmarking are all part of
the standard pipeline.

This entry consolidates the changelog for every development cycle since
the 0.1.2 maintenance release; the package now follows semantic
versioning and the 1.0.0 line constitutes a stable user-facing function
set whose signatures will not change before v2.0.0.

### New features

- **Shiny app: memory management for multi-model comparison.** The
  dashboard’s “snapshot library” lets users compare or average
  predictions across several fitted models in one session. Each snapshot
  is a full object (50-200 MB for a typical Fay-Herriot fit), so a
  session that snapshots 5+ models could easily blow past 1 GB of RAM.
  Three composable mitigations ship in v1.0.0: The multi-model panel now
  also shows a per-snapshot size-in-MB readout below the comparison
  buttons, so users can see exactly how much memory each snapshot is
  costing.

- **Shiny app now exposes the Fay-Herriot `sampling_variance`
  argument.** The interactive
  [`run_sae_app()`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)
  previously offered the survey-design route to fixing $`\phi_i`$ in the
  Beta-Logitnormal workflow (via `n + deff`) but did NOT expose the
  equivalent `sampling_variance = "psi_i"` slot for the
  Lognormal-Lognormal workflow (where it pins
  $`\sigma_i = \sqrt{\psi_i}`$) nor for the Custom (Gaussian / Lognormal
  / Student) workflow. Both gaps have been closed: a new “Sampling
  Variance (psi_i)” dropdown appears on the Lognormal-Lognormal panel
  and a “Sampling Variance (D_i)” dropdown appears under the Custom
  panel when the user picks a family that supports it. Selecting a
  column wires it straight through to
  [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md)
  / [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md),
  which then apply the link-override fix from this release
  transparently. This means the classical Fay-Herriot SAE workflow is
  now end-to-end usable from the GUI without dropping back to scripted
  calls.

- **Shiny app exposes `measurement_error` sugar (Ybarra-Lohr 2008).** A
  new collapsible “Measurement-error covariates” box on the Modeling tab
  lets the user mark one auxiliary covariate as noisily-measured and
  supply the column holding its standard error. hbsaems then rewrites
  into transparently.

- **Shiny app exposes the generic `fixed_params` slot.** A new
  collapsible “Fixed distributional parameters (advanced)” box on the
  Modeling tab accepts an R expression evaluating to a named list (e.g. 
  or ). Power users can now pin any distributional parameter (sigma,
  phi, shape, nu, …) from the GUI without writing scripts.

- **Shiny app exposes the `sae_*` post-processing helpers.** A new
  “Post-processing” sub-tab under Results lets the user apply (log /
  exp), (centre + std, or centre only), and (RSE-threshold cutoff) to
  the most recent prediction and download the transformed table.

- **Shiny app exposes multi-model workflows.** A new “Multi-model”
  sub-tab under Results provides an in-app library of named model
  snapshots. The user clicks “Snapshot current fit” to save a copy of
  under a custom name, then selects 2+ snapshots to feed into (LOO/WAIC)
  or (Bayesian model averaging). This closes the long-standing gap
  between the scripted multi-model API and the GUI workflow.

- **Generic `fixed_params` mechanism** in
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).
  Lets the user pin any distributional parameter to a value derived from
  a column, scalar, vector, or formula evaluated against the data.
  Centralises a pattern that previously had to be coded per-wrapper.

- **[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  refactor.** Four operational modes: random $`\phi`$ with hyperprior;
  fixed $`\phi`$ from survey $`n`$ and $`\mathrm{deff}`$;
  user-overridable hyperprior on $`\alpha,\beta`$ via `stanvars`; and
  generic `fixed_params` for power users. Default priors filled in
  automatically and follow Liu (2009).

- **`hbm_lnln(sampling_variance = ...)`** for the Fay–Herriot lognormal
  model. Pins $`\sigma_i = \sqrt{\psi_i}`$ from a known per-area
  sampling variance.

- **Custom brms response distributions.** Built-in support for
  Loglogistic and Shifted Loglogistic, plus a public extension framework
  ([`register_hbsae_brms_custom()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md),
  [`read_stan_function()`](https://madsyair.github.io/hbsaems/reference/read_stan_function.md),
  [`build_brms_custom_family()`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md)).
  Stan code lives in `inst/stan/` as plain `.stan` files. Each
  registered family ships log-likelihood, posterior-predict and
  posterior-epred hooks so
  [`loo()`](https://mc-stan.org/loo/reference/loo.html),
  [`posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html),
  and
  [`posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
  work out of the box.

- **Spatial random effects.** CAR (ICAR / proper / BYM2) and SAR (lag /
  error) via `spatial_var`, `spatial_model`, `car_type`, `sar_type`, and
  `M`. Weight matrices can be constructed with the bundled
  [`build_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
  from a shapefile or coordinates.

- **Missing-data handling.** Three strategies (`deleted`, `multiple` via
  mice, `model` via
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html)) with
  auto-selection when `handle_missing = NULL`.

- **Shrinkage priors.** Horseshoe (regularised, Piironen & Vehtari

  2017. and R2D2 (Zhang et al. 2022) selectable via `prior_type`.

- **Nonlinear smooth terms with full brms-canonical API.** Penalised
  regression splines via and Gaussian processes via :

  - Splines: `spline_k` (basis dim) and `spline_bs` (basis type: `"tp"`,
    `"cr"`, `"cs"`, `"ps"`).
  - GP: `gp_k` (Hilbert-space approximate GP basis dimension –
    Riutort-Mayol et al. 2023), `gp_cov` (covariance function:
    `"exp_quad"`, `"matern15"`, `"matern25"`, `"exponential"`), `gp_c`
    (boundary-scale factor).
  - Automatic warning when an exact GP (slow, $`O(n^3)`$) is requested
    for more than 100 areas, with the recommended `gp_k` value.
  - `gp_scale` deprecated in favour of `gp_c`; removal scheduled for
    v2.0.0.

- **Bilingual Shiny dashboard**
  ([`run_sae_app()`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)).
  English / Indonesian, dedicated spatial setup tab, in-app code
  preview, and CSV / RDS data upload. Source under
  `inst/shiny/sae_app/`.

- **Benchmarking helpers**
  ([`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md),
  [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)).
  Pfeffermann-style design-consistent benchmarking and out-of-sample
  prediction for unsampled areas.

- **Bayesian model averaging with LOO weights.**
  [`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
  now accepts a `method` argument: `"manual"` (default, user weights),
  `"stacking"`, or `"pseudobma"` (both via
  [`loo::loo_model_weights`](https://mc-stan.org/loo/reference/loo_model_weights.html),
  the canonical Bayesian stacking / pseudo-BMA+ of Yao et al. 2018).

- **Power-scale prior sensitivity diagnostics** via
  [`prior_sensitivity()`](https://madsyair.github.io/hbsaems/reference/prior_sensitivity.md):
  thin wrapper around
  [`priorsense::powerscale_sensitivity()`](https://n-kall.github.io/priorsense/reference/powerscale-sensitivity.html)
  (Kallioinen et al. 2024) for detecting prior-data conflict and weak
  likelihood in fitted models.

- **Hierarchical `area_var` for multi-stage SAE.**
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md),
  [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md),
  [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md),
  and
  [`hbm_binlogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md)
  now accept `area_var` as a character vector (highest level first, e.g.
  `c("province", "regency")`) and a companion `area_re_structure`
  argument that selects between `"nested"` (default; produces
  `(1 | province / regency)`) and `"crossed"` random intercepts.
  Length-1 input behaves exactly as in earlier releases, so existing
  code continues to work unchanged.

- **Custom Stan family name prefix `hbsae_`.** To avoid a symbol
  collision with Stan’s built-in `loglogistic_lpdf` (Stan \>= 2.29), the
  Stan function definitions for the loglogistic and shifted loglogistic
  families are now named `hbsae_loglogistic_lpdf` /
  `hbsae_shifted_loglogistic_lpdf` and live in
  `inst/stan/hbsae_loglogistic.stan` /
  `inst/stan/hbsae_shifted_loglogistic.stan`. The user-facing R helpers
  (`dloglogistic`, `brms_custom_loglogistic`, etc.) and the registry
  keys (`"loglogistic"`, `"shifted_loglogistic"`) are unchanged.

- **[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) now
  accepts `sampling_variance = "<col>"`** as the Fay-Herriot sugar
  (previously only available in
  [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md)).
  Pins via offset and is the canonical way to fit a Gaussian Fay-Herriot
  model. Without it the residual and the area-RE compete to explain the
  same variance, producing weak identifiability and divergent
  transitions almost regardless of `adapt_delta`. All vignettes using
  `data_fhnorm` have been updated accordingly.

  **Family compatibility check**: `sampling_variance` is only valid for
  continuous families that expose a residual SD parameter named `sigma`
  (gaussian, lognormal, student, skew_normal, exgaussian, asym_laplace).
  Passing it with Beta / Binomial / Poisson / Gamma / Weibull families
  now raises an explicit error pointing the user at the appropriate
  family-specific mechanism (e.g.  
  `fixed_params$phi` for Beta via design effect, `trials` for Binomial).

- **Centralised sugar-to-`fixed_params` translation.** The previously
  duplicated translation logic in
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md),
  [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md),
  and
  [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  is now consolidated in two internal helpers (, ) with consistent
  validation, conflict checks against `fixed_params`, and error
  messages. Behaviour is unchanged from the user’s point of view; the
  refactor eliminates ~60 lines of duplicated code.

- **`data_fhnorm` regenerated** with a deterministic, well-identified
  simulation (`set.seed(20260518L)`). Covariates are standardised to ,
  sigma_u = 1.0, and so that vignettes fit cleanly with default brms /
  Stan settings. See `data-raw/data_fhnorm.R` for the reproducible
  generator.

- **`measurement_error` sugar** (Ybarra and Lohr 2008).
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and the
  `hbm_*` wrappers accept which rewrites the brmsformula on the fly to
  wrap the listed auxiliary variables with . Validation enforces
  non-negative, NA-free standard errors and that the named variables are
  part of `auxiliary`.

- **Automatic `mi() / me()` detection.** When the user writes or
  explicitly in the formula,
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) no
  longer demands `handle_missing` be set and no longer drops rows with
  `NA`: brms’s joint-modelling / measurement-error framework handles the
  imputation internally. Internally `handle_missing` is silently set to
  `"model"` in that case.

- **Conditional `link_phi` resolution in
  [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md).**
  Default is now `NULL` and resolves automatically: `"identity"` when
  `phi` is pinned via `fixed_params$phi` (the survey-design mode),
  `"log"` (brms default) when `phi` is estimated via a hyperprior. This
  eliminates a class of divergent transitions caused by NUTS proposing
  negative on the identity scale. Manually setting `"identity"` in
  random mode emits a warning.

- **Simplified `phi` prior in
  [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  random mode.** The pre-existing hierarchical construction has been
  replaced by brms’s own default (lower bound 0). The wrapper no longer
  declares or as Stan parameters and no longer injects sampling
  statements for them. Rationale: the prior on (declared as ) was on the
  boundary of its support, producing divergent transitions on
  weakly-informative data; the extra layer also inflated the effective
  posterior dimension for what is essentially one scalar parameter per
  area model. users who relied on the old construction can build it
  manually via the + arguments; see for the migration note. Legacy code
  that still passes containing sampling statements on / now raises an
  informative error pointing the user at the new mechanism.

- **[`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
  defaults made semantically explicit.** New argument
  `target_type = c("total", "mean")` (default `"total"`) is consulted
  only when `weights = NULL`, choosing a safe default weighting
  (`rep(1, n)` for total, `rep(1 / n, n)` for mean). An informational
  message is emitted so the chosen weighting is always visible.
  Production users should still pass explicit `weights = N_i`
  (population size per area).

### Naming and interface changes (breaking only at v2.0.0)

- **Argument rename: `predictors` -\> `auxiliary`**. The new name aligns
  with Small Area Estimation literature (Rao & Molina 2015, Pfeffermann
  2013). Old usage continues to work with a one-time soft-deprecation
  warning and is **scheduled for removal in v2.0.0**.
- **Argument rename: `hbm_generic()` -\>
  [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)**.
  Old name removed.
- **Deprecated wrappers (legacy v0.1.x):**
  [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md).
  All four emit a soft-deprecation warning pointing at
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md),
  [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md),
  [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md),
  [`sae_aggregate()`](https://madsyair.github.io/hbsaems/reference/sae_aggregate.md),
  or
  [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  respectively. **Scheduled for removal in v2.0.0**.

### Documentation

- **Seven new vignettes** covering the full workflow:
  `hbsaems-modelling` (overview), `hbsaems-lnln-model`,
  `hbsaems-betalogitnorm-model`, `hbsaems-binlogitnorm-model`,
  `hbsaems-spatial`, `hbsaems-handle-missing`, `hbsaems-run_sae_app`.
  All follow a CRAN-safe pattern: heavy Stan fits are not evaluated at
  build time; representative outputs are printed as illustrations with
  an explicit disclaimer.
- **Three retained vignettes**: `complete-workflow`,
  `advanced-features`, `migration-guide`.
- **Vignettes updated to reflect v1.0.0 changes.** The
  `hbsaems-betalogitnorm-model` vignette now documents two modes for
  $`\phi`$ (Random and Fixed) instead of three, and includes a
  legacy-reproduction recipe for users who need the pre-v1.0.0
  hierarchical hyperprior. The `migration-guide` vignette gained two new
  sections covering the phi prior simplification and the critical link
  function override fix. The `hbsaems-lnln-model` vignette adds an
  implementation note explaining why the `sampling_variance` offset is
  now interpreted on the natural (untransformed) scale.

### New documentation

- **CRAN vignette now carries real output.** The `complete-workflow`
  vignette previously used `knitr::opts_chunk$set(eval = FALSE)`
  globally and showed all R code as display-only. This kept CRAN’s
  vignette build short but also meant readers could not see what any of
  the seven workflow steps actually produces.

  The revised vignette evaluates a curated subset of chunks:

  The production-grade `hbm(..., chains = 4, iter = 4000)` calls for the
  prior-predictive check, the headline model fit, the model-comparison,
  and model-averaging are kept as display-only blocks (lowercase, not
  evaluated by knitr) – those would push the CRAN vignette build well
  past its time budget. The text now explicitly tells readers to use
  those full settings in their own analyses, not the toy `iter = 200` of
  the demo.

  A Stan-toolchain probe in the setup chunk silently falls back to
  display-only mode when Boost / a C++ compiler is unavailable (e.g. on
  a minimal CI container), so the vignette renders cleanly everywhere –
  it just has fewer outputs on Stan-less systems.

- **GitHub Actions: automated pkgdown deployment.** Three workflows live
  in `.github/workflows/`: A new at the repository root explains
  first-time setup (GitHub Pages settings, Actions write permissions,
  URL field in DESCRIPTION) and the local pkgdown::build_site() preview
  workflow. The guide is Rbuildignored so it does not bloat the CRAN
  tarball.

- **Vignette strategy reorganised for the CRAN release.** Through v1.0.0
  development we built up an extensive collection of vignettes covering
  every distribution-specific wrapper, every advanced topic, and the
  internals. Eleven vignettes was a great documentation set but a heavy
  load for a CRAN tarball; CRAN’s guidance for new submissions favours a
  small set of essential vignettes. We now ship **only one vignette** in
  the tarball, `complete-workflow`, which is the canonical end-to-end
  SAE pipeline using
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) and the
  standard Bayesian workflow diagnostics. The remaining ten files are
  kept in the source repository under `vignettes/articles/` (a pkgdown
  convention) and are rendered as articles on the package website at
  <https://madsyair.github.io/hbsaems/>. R CMD build skips
  `vignettes/articles/` via `.Rbuildignore`, so the CRAN tarball is
  leaner (-46 KB) without losing any documentation – users who install
  from CRAN still get the website link via the `URL:` field in
  DESCRIPTION.

- **New article: “AST-based Formula Manipulation”.** A walkthrough of
  how `hbsaems` rewrites user formulas internally to apply the
  `nonlinear`, `measurement_error`, `area_var`, `sampling_variance`, and
  `handle_missing` sugar – and why a regex-based approach would silently
  corrupt many legitimate formulas. Available at
  <https://madsyair.github.io/hbsaems/articles/ast-formula-manipulation.html>.

### Bug fixes

- **`sae_predict(model, newdata = X)` failed with “variables can neither
  be found in ‘data’ nor in ‘data2’” when the model was fit with
  `sampling_variance =` or `fixed_params =`.** Those sugar arguments
  inject internal offset columns named (e.g.  for the Fay-Herriot
  construction) into the training data, and the brms formula then
  carries . A user passing a fresh that did not carry those columns hit
  a cryptic error.

  now repopulates the offset columns automatically when the
  user-supplied has the same number of rows as the training data (the
  typical case of “predict at the same areas, possibly with updated
  covariates”). When differs, sae_predict() raises an informative error
  telling the user either to align the row count or to compute the
  offset column themselves (e.g. ).

  The same fix benefits , which internally calls for each candidate
  model.

  Seven regression tests cover the in-place copy path, the pass-through
  paths (no offset cols, or already populated by user), the
  nrow-mismatch error, and the NULL-training-data guard.

- **GitHub Actions `R-CMD-check --as-cran` failed with “Boost not found”
  on Linux runners.** CRAN’s build farm ships the full Stan toolchain
  (BH / RcppEigen / RcppParallel / StanHeaders) by default, but GitHub’s
  minimal R container does not – and the
  `r-lib/actions/setup-r-dependencies@v2` step had no way to know which
  transitive C++ header packages our `\donttest{}` examples needed. All
  three workflows (`R-CMD-check.yaml`, `vignettes.yaml`, `pkgdown.yaml`)
  now declare these packages explicitly via `extra-packages:`, so the
  rstan compile path works.

- **MCMC settings in `\donttest{}` examples standardised to brms
  defaults.** Examples now consistently use
  `chains = 4, iter = 2000, warmup = 1000` (the brms defaults documented
  in [`?brms::brm`](https://paulbuerkner.com/brms/reference/brm.html)).
  Each example block also carries a brief comment noting that
  production-grade inference on tougher posteriors (funnel geometry,
  weakly identified priors) may require `iter = 4000, warmup = 2000` and
  `control = list(adapt_delta = 0.99)`, with cross-references to the
  complete-workflow vignette.

- **GitHub Actions `R-CMD-check --as-cran` failed with “Boost not found”
  on Linux runners.** CRAN’s build farm ships the full Stan toolchain
  (BH / RcppEigen / RcppParallel / StanHeaders) by default, but GitHub’s
  minimal R container does not – and the
  `r-lib/actions/setup-r-dependencies@v2` step had no way to know which
  transitive C++ header packages our `\donttest{}` examples needed. All
  three workflows (`R-CMD-check.yaml`, `vignettes.yaml`, `pkgdown.yaml`)
  now declare these packages explicitly via `extra-packages:`, so the
  rstan compile path works.

- **MCMC sampling in `\donttest{}` examples was unnecessarily heavy.**
  Many examples ran `chains = 2, iter = 2000` (some
  `chains = 4, iter = 4000`), which added up to several minutes of
  example-run time during R CMD check –run-donttest. CRAN’s guidance is
  that examples should run in a few seconds total per function. All such
  examples have been retuned to `chains = 1, iter = 500, warmup = 250`
  (a few seconds each) and prefixed with a comment explaining that
  production settings should be used in real analyses. The displayed
  code in vignettes still references the full production settings.

- **`vignettes/articles/hbsaems-betalogitnorm-model.Rmd` showed a legacy
  code block that no longer runs.** The article had a “Reproducing the
  legacy hierarchical hyperprior” section that demonstrated the
  pre-v1.0.0 construction by calling . As of v1.0.0, explicitly rejects
  that pattern at construction time with an informative error (/ are no
  longer declared as Stan parameters by the wrapper). Users who
  copy-pasted the article’s example would have hit the runtime error.

  The section has been rewritten: The migration-guide article has been
  updated to cross-reference the new corrected example.

- **Article YAML headers used
  [`rmarkdown::html_vignette`](https://pkgs.rstudio.com/rmarkdown/reference/html_vignette.html).**
  Files under are pkgdown-only and no longer carry a
  `\\VignetteIndexEntry{}` field (that field is what relies on for the
  page title). Manually rendering these files with would emit a benign
  warning about the missing field. We now use for articles, which works
  identically when rendered by pkgdown but is silent under direct too –
  helpful for contributors previewing changes locally.

- **[`?register_hbsae_brms_custom`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md)
  example was non-idempotent.** The example registered every time it ran
  but never cleaned up. Running the example a second time in the same R
  session (which R CMD check’s example runner can do, and which
  interactive users routinely do) would error with . The example now
  removes any pre-existing entry before registering and unregisters at
  the end, so re-runs are clean.

- **subdirectory removed.** The file (31 KB) shipped with the package
  because it lived under . Its content was redundant with the curated
  articles on the package website and bloated the CRAN tarball. Also
  removed three internal maintainer documents that had drifted into :
  (duplicate of the root version), (internal note), and (moved to
  repository root and Rbuildignored). Only the runtime-essential , , and
  remain. Net reduction in installed package size: ~50 KB.

- **[`?hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  example 3 used the removed alpha/beta hyperprior pattern.** R CMD
  check caught a leftover example block that attempted to declare priors
  on and via , both of which were Stan parameters in the pre-v1.0.0
  hierarchical phi construction but were removed in this release (see
  the migration note at the top of ). Example 3 has been rewritten to
  demonstrate the supported v1.0.0 pattern – a non-default phi prior is
  passed via brms’s standard rather than via the legacy stanvars
  sampling statements. The resulting Stan code uses as intended.

- **R CMD check NOTE: `tests/spelling.Rout.save` comparison.** R CMD
  check was diffing the live spelling test output against a saved
  snapshot, which broke any time a new acceptable term appeared in NEWS
  or vignettes (e.g.  from an internal predictions data structure). We
  have removed the comparison file and rely solely on , which uses the
  proper mechanism. This is the approach recommended by Jeroen Ooms
  (author of ) for packages that want spelling checks during development
  but not on CRAN’s machines.

- **AST-based formula rewriting silently dropped
  [`offset()`](https://rdrr.io/r/stats/offset.html) terms.** Both (used
  by the sugar) and (used by the sugar) decompose the formula RHS via
  and reassemble it with . The reassembly step iterated only over ,
  which is the list of predictor terms – those live in a separate
  attribute as integer positions into . As a result, a user formula like
  called with would silently lose the offset, producing . Both helpers
  now extract the offset terms and splice them back into the assembled
  RHS verbatim. Eight regression tests cover offset preservation,
  substring-name safety, wrapper preservation (I, poly, me),
  interaction-term preservation, and the hierarchical .

- **Conflict protection: custom brms families could silently shadow
  built-in or brms-native families.** Three classes of conflict were
  previously possible:

  This release introduces a comprehensive list that covers both
  brms-native AND hbsaems-bundled custom families, makes the
  override-protection logic in both and consult it, adds a probe so
  registrations that would shadow a brms-native family emit an
  informative warning pointing the user at the prefix convention, and
  rewrites to clear non-builtin keys before re-asserting the built-ins.

  An internal option () lets bypass the new check during package load,
  avoiding a chicken-and-egg circular dependency.

  Six regression tests cover all paths.

- **[`dloglogistic()`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  errored when `mu` or `beta` was NA.** treats as failure, so stopped
  with . Now NAs in parameters propagate as NA in the result (matching
  the v1.0.0 NA-in-x fix and the wider R density-function convention),
  while finite non-positive parameters still error explicitly.

- **Stan function names safely prefixed with `hbsae_`.** Audited and to
  confirm every Stan-side function declaration uses the prefix, avoiding
  collision with Stan 2.29+’s built-in . This was already correct in the
  code; the audit added documentation comments and a verifying dev-test.

- **[`dloglogistic()`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  silently coerced NA inputs to 0.** Base R density functions follow the
  convention that , but our returned 0 (the density at , which is also
  0). The vectorised case was particularly misleading because a partial
  NA vector produced no warning even though one position was being
  silently replaced with 0. We now propagate NA from any of , , or –
  matching the behaviour and the wider R density- function convention.

- **[`run_sae_app()`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)
  produced a cryptic error when was missing.** Because lives in , users
  who installed hbsaems with only the modelling dependencies would see
  “could not find function runApp” rather than an actionable error. We
  now check at the top of and emit an informative error pointing to .

- **`run_sae_app(check_deps = FALSE)` carried stale `optional_missing`
  state across sessions.** If the user previously launched with (which
  writes ) and then re-launched with , the GUI banner would still
  display the cached missing-package list. We now clear the option to in
  the branch.

- **`DESCRIPTION` Suggests: missing `energy` and `minerva`.** The Shiny
  app’s exploratory-correlation panel (Distance correlation, MIC)
  depends on these two packages, but was the only place that referenced
  them. Without listing them in Suggests, downstream automated checks
  (e.g. ) would have flagged the unconditional call as a NOTE. Both
  packages are now in Suggests as documented in .

- **Bundled data: HIERARCHICAL CONSISTENCY across datasets.** All four
  bundled datasets now use the same administrative-level labels with the
  same meanings: What differs between datasets is the analysis
  resolution – i.e.  
  at which level the 100 small-area observations live: Label format
  conventions disambiguate at a glance: three-digit suffixes (, ) for
  the 100-level fine analysis areas and two-digit suffixes (, ) for the
  5-level coarse spatial clusters. Through earlier v1.0.0 iterations the
  same role was variously called “regency” (creating dual-semantics
  confusion), “kabupaten” (Indonesian-only), and “county” (US-only);
  none of those reached CRAN. The matrix has been correspondingly
  renamed `adjacency_matrix_car_regency` (5x5, rownames ). Total impact
  on data folder vs original pre-audit state is **+4 bytes** (29822
  -\> 29826) – effectively neutral.

- **Documentation: incorrect spatial pairings in three examples.** Two
  SAR examples paired `spatial_weight_sar` (100x100, regency-named) with
  `spatial_var = "province"` (5 levels) – this would error at fit time
  because brms cannot resolve a 5-level grouping factor against a
  100-row weight matrix. One BYM2 example paired `data_binlogitnorm`
  (which has no `province` column) with `spatial_var = "province"`. All
  three examples in
  [`?hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md),
  `vignette('hbsaems-spatial')`, and inline documentation have been
  corrected.

- **`print.hbcc_results()` reported PASS on all-NA Rhat.** Same pattern
  as the fix, but in the print method: returns TRUE on and silently
  displayed for degenerate fits with no finite Rhat values. Now reports
  .

- **`print.hbsae_results()` printed `NaN%` and `Inf to -Inf`.** When is
  NA / NaN (all areas had zero predictions – see the v1.0.0 RSE guard in
  ) the print method would output literal \code{“NaN %”} and for the
  prediction range. Both are now handled with explicit branches that
  label them as .

- **Bundled-data integrity now checked at every test run.** A new test
  file verifies that: These tests would have caught the historical
  “regency means 100 levels in two datasets and 5 levels in two others”
  mismatch before it shipped.

- **[`hbm_warnings()`](https://madsyair.github.io/hbsaems/reference/hbm_warnings.md)
  failed to flag degenerate model fits.** The R-hat convergence check
  used , which evaluates to (i.e. no warning fires) when all Rhat values
  are NA – the textbook signature of a sampler that produced no useful
  draws. Same issue affected the check. Both now detect the all-NA /
  all-non- finite case explicitly and emit a dedicated warning.

- **[`register_hbsae_model()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md)
  silently overwrote built-in families.** Passing for a built-in key (,
  , , , etc.) silently replaced the package-curated spec, losing the
  validation rules and link handling that the built-in provides. Without
  the function now refuses to touch a built-in; with it emits a warning
  so the choice is conscious.

- **[`register_hbsae_model()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md)
  accepted invalid registry keys.** Keys with whitespace (), leading
  digits (), or other punctuation that would force backtick-quoting are
  not usable as R names and broke downstream registry access. Keys are
  now validated with . Empty-string keys are also rejected.

- **[`register_hbsae_model()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md)
  did not validate type.** Non-character or vector-of- strings values
  would either error cryptically downstream or produce garbled messages.
  Now validated as or a single character string.

- **[`register_hbsae_brms_custom()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md)
  had thinner validation than its sibling.** The brms-custom-family
  registration path used a bare for the key, didn’t validate , , , or ,
  and let callers silently shadow built-in families. Validation now
  matches .

- **[`is_converged()`](https://madsyair.github.io/hbsaems/reference/is_converged.md)
  returned silent TRUE on all-NA Rhat.** Both S3 methods relied on ,
  which returns TRUE for after the NA’s are stripped – producing a
  degenerate “converged” signal whenever the underlying brmsfit had no
  finite Rhat values (e.g. a collapsed sampler, all chains diverged,
  mock object). now returns with an informative warning in that case.

- **[`is_converged()`](https://madsyair.github.io/hbsaems/reference/is_converged.md)
  did not validate the `threshold` argument.** A string threshold (), ,
  length\>1 vector, negative value, or non-finite value all silently
  returned TRUE due to R’s coercion rules in and . We now reject those
  inputs explicitly via .

- **[`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
  silently produced NA / Inf when `target` was non-finite.** Passing or
  propagated through the ratio / difference computation and produced an
  all-NA / all-Inf benchmarked table. Both are now rejected with
  informative errors. Likewise, non-finite values in now raise an early
  error pointing the user back to the model-fit step.

- **Legacy callback removed from the Beta family registry.** The
  pre-v1.0.0 hierarchical hyperprior
  $`\phi \sim \mathrm{Gamma}(\alpha, \beta)`$ with
  $`\alpha, \beta \sim \mathrm{Gamma}(1, 1)`$ was previously hard-wired
  in the registry under . That made silently re-inject the old
  construction even after was simplified to use brms’s default prior in
  . The callback has been removed from the built-in spec; custom
  families can still register one via .

- **[`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  length-1 area_var validation crashed on length\>1 vectors.** When the
  user supplied an `area_var` character vector of length 2 or more (the
  hierarchical-area mode), the validation
  `!is.null(area_var) && !(area_var %in% names(data))` raised the R 4.2+
  error – the `&&` operator does not coerce length\>1 logicals to
  scalar. Replaced with a
  [`setdiff()`](https://rdrr.io/r/base/sets.html)-based check that
  produces an informative error listing every missing column. Same idiom
  applied defensively to `spatial_var` (which is always length-1 by API
  design).

- **`re = ~ (1 | x/y)` nested syntax rejected by `re` validator.**
  `.build_area_re_formula()` synthesises nested random-effect terms with
  the lme4 sugar , but the regex in
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) only
  accepted and . The regex now accepts the `/` separator as well, so
  hierarchical- area models built via
  `area_var = c("province", "regency"), area_re_structure = "nested"`
  reach brms with the correct formula.

- **`sae_aggregate(method = "weighted")` with all-zero weights produced
  NaN.** The internal normalisation `weights / sum(weights)` silently
  produced NaN when all weights were zero (and Inf when sum was
  negative). We now validate that the weight sum is strictly positive
  and that all individual weights are finite, raising informative errors
  otherwise.

- **[`sae_transform()`](https://madsyair.github.io/hbsaems/reference/sae_transform.md)
  recycled scalar return values silently.** A user passing a reducer
  function (e.g. `fun = sum`, `fun = mean`) instead of an element-wise
  transform (`fun = log`, `fun = exp`) saw silent scalar recycling –
  every area’s prediction became identical. We now validate that
  `fun(pred)` returns a numeric vector of the same length as the input
  and emit an informative error otherwise.

- **[`update_hbm()`](https://madsyair.github.io/hbsaems/reference/update_hbm.md)
  failed silently when newdata lacked offset columns.** Models fitted
  with , + (in ), or attach hidden offset columns () to the model data
  frame. When the user passed a that lacked these columns to , brms
  refused to refit with the unhelpful error . now detects the case and:
  Three regression tests added.

- **Multivariate (mi() / joint) models failed in , , and .** brms’s
  returns a 3-D array of shape (draws x obs x responses) for
  multivariate formulas (e.g. ); and downstream produced 2-D outputs
  that broke the construction of . Additionally, requires an explicit
  argument for multivariate models, which we were not providing. All
  three functions now detect the multivariate case, default to the first
  sub-formula’s response (the conventional SAE target), and forward it
  appropriately. Power users can override via in the argument.

- **CRITICAL: `.add_fixed_pforms()` failed on multivariate formulas.**
  When the user combined joint-imputation formulas (e.g. ) with / , the
  helper attempted to append via . brms refused this because the dpar
  formula did not specify which response ( or ) the applies to,
  producing . The helper now detects objects, extracts the primary
  response (first sub-formula by convention), and passes to . Regression
  tests in cover both the end-to-end flow and the helper in isolation.

- **CRITICAL: `sampling_variance` / silently corrupted by brms’s default
  log link on `sigma`.** When the user supplied , the wrapper stored in
  and attached it to the brms formula as . However, brms applies the
  dpar’s link function before plugging the linear predictor into the
  likelihood; for Gaussian / Lognormal / Student families the default
  caused the Stan model to compute instead of . E.g.  should give but
  actually produced – a catastrophic miscalibration of the Fay-Herriot
  model. Same bug affected any user pinning sigma via the generic , and
  would also have affected phi for Beta if had not already manually set
  . The fix forces on the family object for every pinned dpar, so the
  offset values are passed verbatim to the likelihood. Three regression
  tests added to guard against this regression.

- **[`posterior_interval()`](https://madsyair.github.io/hbsaems/reference/posterior_interval.md)
  and
  [`prior_draws()`](https://madsyair.github.io/hbsaems/reference/prior_draws.md)
  now re-export the upstream generics** from and respectively, rather
  than defining new generics with conflicting signatures. This fixes a
  name-collision crash that occurred when was attached together with and
  the user called on an object: the error message no longer occurs. The
  fix follows the standard R package design pattern used by itself.

- **[`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
  scale corruption fix.** The previous default `weights = rep(1 / n, n)`
  silently assumed `target` was a population mean. When users instead
  passed a population total, the implied ratio adjustment came out
  roughly times too large – benchmarked estimates were scale-corrupt by
  a factor of . Fixed by introducing the explicit `target_type` argument
  (see New features) and emitting a message when the default kicks in.

- **[`sae_scale()`](https://madsyair.github.io/hbsaems/reference/sae_scale.md)
  zero-variance NaN propagation.** When all area predictions were
  identical, [`base::scale()`](https://rdrr.io/r/base/scale.html)
  produced `NaN` throughout, silently corrupting `result_table`. Now
  detected with a warning; the method returns centred-only (or raw)
  predictions instead.

- **[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  divergent-transitions on random `phi`.** The prior default
  `link_phi = "identity"` together with the hyperprior let NUTS propose
  negative , triggering evaluations. Default is now resolved
  conditionally (see New features).

- **dev-tests [`library(hbsaems)`](https://madsyair.github.io/hbsaems/)
  auto-attach.** The helper file
  `tests/testthat/dev-tests/helper-dev-setup.R` now attaches the package
  automatically, fixing 150+ spurious `could not find function` errors
  when dev-tests were run via
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html).

- **[`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  early validation of `spatial_var`.** When the user supplied
  `spatial_var` referencing a column that did not exist in `data`, the
  error previously surfaced only inside brms with the unfriendly message
  *“variables can neither be found in ‘data’ nor in ‘data2’”*. The
  wrapper now validates the column up front and emits the friendlier
  *“Spatial variable ‘’ not found in `data`.”* message, matching the
  equivalent guard for `area_var`.

- **[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md)
  `data2` collision with user-supplied `...`.** Calling
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) with
  both `spatial_model = "car"` (or `"sar"`) and a user-supplied
  `data2 = list(...)` via `...` crashed with the R-level error *“formal
  argument ‘data2’ matched by multiple actual arguments”*.
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) builds
  an internal `data2 = list(M = M)` for the spatial weight matrix and
  also splices `...` into the
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  call, producing two `data2` keys at the site. The fix extracts any
  user-supplied from before constructing the brms argument list and
  merges it into the internal via – the spatial-matrix slot () supplied
  by hbsaems wins on collision, but additional user keys (e.g. auxiliary
  matrices for nonlinear models) are preserved. As a related hardening,
  attempts to override other internally- managed brms arguments (, , , ,
  etc.) via now raise an informative error pointing the user to the
  dedicated hbsaems argument.

### Internal

- **Test suite reorganisation.** Per the recommendation of an external
  code review, validation-only tests (those exercising only input
  validation, deprecation warnings, and error paths) have been split out
  from the heavy integration tests and migrated to . Each block was
  classified by static analysis: blocks whose only / call appears inside
  / are CRAN-safe and now run on every check; the remaining ~60
  integration tests that actually compile a Stan model stay in and are
  gated by (or ). Net effect: CRAN test count rose from 391 to ~466 with
  no measurable runtime increase. Internal helpers and in provide
  brms-compatible shells for the few tests that need to exercise the
  deprecation pipeline without compiling Stan.

- **Project polish.** Added `CONTRIBUTING.md` documenting the
  development workflow (test tiers, mock-stub usage, AST-based formula
  manipulation patterns, spelling regeneration); added a light `.lintr`
  configuration for contributors; updated `cran-comments.md` to describe
  the first-submission test environment and intentional notes.

- All exported functions documented with full `roxygen2` blocks
  including `@param`, `@return`, `@examples`, and references where
  appropriate.

- Custom-distribution Stan code stored as separate `.stan` files in
  `inst/stan/` (one source of truth, syntax highlighting, no
  string-escaping noise).

- Test suite reorganised into CRAN-safe unit tests (`tests/testthat/`)
  and heavy integration tests (`tests/testthat/dev-tests/`, gated by
  `skip_on_cran()` and excluded via `.Rbuildignore`).

## hbsaems 0.1.0

- Initial public release on CRAN. Provided
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md),
  [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md),
  and three example datasets (`data_fhnorm`, `data_lnln`,
  `data_betalogitnorm`, `data_binlogitnorm`).
