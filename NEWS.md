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

* **Shiny app now exposes the Fay-Herriot `sampling_variance`
  argument.**  The interactive `run_sae_app()` previously offered
  the survey-design route to fixing $\phi_i$ in the Beta-Logitnormal
  workflow (via `n + deff`) but did NOT expose the equivalent
  `sampling_variance = "psi_i"` slot for the Lognormal-Lognormal
  workflow (where it pins $\sigma_i = \sqrt{\psi_i}$) nor for the
  Custom (Gaussian / Lognormal / Student) workflow.  Both gaps have
  been closed: a new "Sampling Variance (psi_i)" dropdown appears
  on the Lognormal-Lognormal panel and a "Sampling Variance (D_i)"
  dropdown appears under the Custom panel when the user picks a
  family that supports it.  Selecting a column wires it straight
  through to `hbm_lnln()` / `hbm()`, which then apply the
  link-override fix from this release transparently.  This means
  the classical Fay-Herriot SAE workflow is now end-to-end usable
  from the GUI without dropping back to scripted calls.

* **Shiny app exposes `measurement_error` sugar (Ybarra-Lohr 2008).**
  A new collapsible "Measurement-error covariates" box on the
  Modeling tab lets the user mark one auxiliary covariate as
  noisily-measured and supply the column holding its standard
  error.  hbsaems then rewrites \code{y ~ x} into
  \code{y ~ mi(x, se_x)} transparently.

* **Shiny app exposes the generic `fixed_params` slot.**  A new
  collapsible "Fixed distributional parameters (advanced)" box on
  the Modeling tab accepts an R expression evaluating to a named
  list (e.g.\ \code{list(sigma = "D")} or
  \code{list(shape = 2)}).  Power users can now pin any
  distributional parameter (sigma, phi, shape, nu, ...) from the
  GUI without writing scripts.

* **Shiny app exposes the `sae_*` post-processing helpers.**  A new
  "Post-processing" sub-tab under Results lets the user apply
  \code{sae_transform()} (log / exp), \code{sae_scale()} (centre +
  std, or centre only), and \code{sae_filter()} (RSE-threshold
  cutoff) to the most recent prediction and download the
  transformed table.

* **Shiny app exposes multi-model workflows.**  A new "Multi-model"
  sub-tab under Results provides an in-app library of named model
  snapshots.  The user clicks "Snapshot current fit" to save a
  copy of \code{model_fit()} under a custom name, then selects 2+
  snapshots to feed into \code{model_compare_all()} (LOO/WAIC) or
  \code{model_average()} (Bayesian model averaging).  This closes
  the long-standing gap between the scripted multi-model API and
  the GUI workflow.

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
* **`hbm_lnln(sampling_variance = ...)`** for the Fay--Herriot lognormal
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
  (lag / error) via `spatial_var`, `spatial_model`, `car_type`,
  `sar_type`, and `M`.  Weight matrices can be constructed with the
  bundled `build_spatial_weight()` from a shapefile or coordinates.
* **Missing-data handling.**  Three strategies (`deleted`,
  `multiple` via mice, `model` via `brms::mi()`) with auto-selection
  when `handle_missing = NULL`.
* **Shrinkage priors.**  Horseshoe (regularised, Piironen & Vehtari
  2017) and R2D2 (Zhang et al.\ 2022) selectable via `prior_type`.
* **Nonlinear smooth terms with full brms-canonical API.**
  Penalised regression splines via \pkg{mgcv} and Gaussian processes
  via \pkg{brms}:
    * Splines: `spline_k` (basis dim) and `spline_bs` (basis type:
      `"tp"`, `"cr"`, `"cs"`, `"ps"`).
    * GP: `gp_k` (Hilbert-space approximate GP basis dimension --
      Riutort-Mayol et al.\ 2023), `gp_cov` (covariance function:
      `"exp_quad"`, `"matern15"`, `"matern25"`, `"exponential"`),
      `gp_c` (boundary-scale factor).
    * Automatic warning when an exact GP (slow, $O(n^3)$) is
      requested for more than 100 areas, with the recommended
      `gp_k` value.
    * `gp_scale` deprecated in favour of `gp_c`; removal scheduled
      for v2.0.0.
* **Bilingual Shiny dashboard** (`run_sae_app()`).  English /
  Indonesian, dedicated spatial setup tab, in-app code preview, and
  CSV / RDS data upload.  Source under
  `inst/shiny/sae_app/`.
* **Benchmarking helpers** (`sae_benchmark()`,
  `sae_predict()`).  Pfeffermann-style design-consistent benchmarking
  and out-of-sample prediction for unsampled areas.
* **Bayesian model averaging with LOO weights.**  `model_average()`
  now accepts a `method` argument: `"manual"` (default, user weights),
  `"stacking"`, or `"pseudobma"` (both via `loo::loo_model_weights`,
  the canonical Bayesian stacking / pseudo-BMA+ of Yao et al.\ 2018).
* **Power-scale prior sensitivity diagnostics** via
  `prior_sensitivity()`: thin wrapper around
  `priorsense::powerscale_sensitivity()` (Kallioinen et al.\ 2024) for
  detecting prior-data conflict and weak likelihood in fitted models.
* **Hierarchical `area_var` for multi-stage SAE.** `hbm_flex()`,
  `hbm_lnln()`, `hbm_betalogitnorm()`, and `hbm_binlogitnorm()` now
  accept `area_var` as a character vector (highest level first, e.g.
  `c("province", "regency")`) and a companion `area_re_structure`
  argument that selects between `"nested"` (default; produces
  `(1 | province / regency)`) and `"crossed"` random intercepts.
  Length-1 input behaves exactly as in earlier releases, so existing
  code continues to work unchanged.
* **Custom Stan family name prefix `hbsae_`.** To avoid a symbol
  collision with Stan's built-in `loglogistic_lpdf` (Stan >= 2.29),
  the Stan function definitions for the loglogistic and shifted
  loglogistic families are now named
  `hbsae_loglogistic_lpdf` / `hbsae_shifted_loglogistic_lpdf` and live
  in `inst/stan/hbsae_loglogistic.stan` / `inst/stan/hbsae_shifted_loglogistic.stan`.
  The user-facing R helpers (`dloglogistic`, `brms_custom_loglogistic`,
  etc.) and the registry keys (`"loglogistic"`, `"shifted_loglogistic"`)
  are unchanged.
* **`hbm()` now accepts `sampling_variance = "<col>"`** as the
  Fay-Herriot sugar (previously only available in `hbm_lnln()`).
  Pins \eqn{\sigma_i = \sqrt{D_i}} via offset and is the canonical
  way to fit a Gaussian Fay-Herriot model.  Without it the residual
  \eqn{\sigma} and the area-RE \eqn{\sigma_u} compete to explain the
  same variance, producing weak identifiability and divergent
  transitions almost regardless of `adapt_delta`.  All vignettes
  using `data_fhnorm` have been updated accordingly.
  
  **Family compatibility check**: `sampling_variance` is only valid
  for continuous families that expose a residual SD parameter named
  `sigma` (gaussian, lognormal, student, skew_normal, exgaussian,
  asym_laplace).  Passing it with Beta / Binomial / Poisson /
  Gamma / Weibull families now raises an explicit error pointing the
  user at the appropriate family-specific mechanism (e.g.\
  `fixed_params$phi` for Beta via design effect, `trials` for
  Binomial).
* **Centralised sugar-to-`fixed_params` translation.**  The previously
  duplicated translation logic in `hbm()`, `hbm_lnln()`, and
  `hbm_betalogitnorm()` is now consolidated in two internal helpers
  (\code{.translate_sampling_variance}, \code{.translate_n_deff_to_phi})
  with consistent validation, conflict checks against
  `fixed_params`, and error messages.  Behaviour is unchanged from the
  user's point of view; the refactor eliminates ~60 lines of
  duplicated code.
* **`data_fhnorm` regenerated** with a deterministic, well-identified
  simulation (`set.seed(20260518L)`).  Covariates are standardised to
  \eqn{\mathcal{N}(0, 1)}, sigma_u = 1.0, and \eqn{D_i \sim
  \mathrm{Gamma}(4, 4)} so that vignettes fit cleanly with default
  brms / Stan settings.  See `data-raw/data_fhnorm.R` for the
  reproducible generator.
* **`measurement_error` sugar** (Ybarra and Lohr 2008).  `hbm()` and
  the `hbm_*` wrappers accept
  \code{measurement_error = list(x1 = "se_x1", ...)} which rewrites
  the brmsformula on the fly to wrap the listed auxiliary variables
  with \code{mi(var, se_col)}.  Validation enforces non-negative,
  NA-free standard errors and that the named variables are part of
  `auxiliary`.
* **Automatic `mi() / me()` detection.**  When the user writes
  \code{mi(...)} or \code{me(...)} explicitly in the formula,
  `hbm()` no longer demands `handle_missing` be set and no longer
  drops rows with `NA`: brms's joint-modelling / measurement-error
  framework handles the imputation internally.  Internally
  `handle_missing` is silently set to `"model"` in that case.
* **Conditional `link_phi` resolution in `hbm_betalogitnorm()`.**
  Default is now `NULL` and resolves automatically: `"identity"`
  when `phi` is pinned via `fixed_params$phi` (the survey-design
  mode), `"log"` (brms default) when `phi` is estimated via a
  hyperprior.  This eliminates a class of divergent transitions
  caused by NUTS proposing negative \eqn{\phi} on the identity
  scale.  Manually setting `"identity"` in random mode emits a
  warning.
* **Simplified `phi` prior in `hbm_betalogitnorm()` random mode.**
  The pre-existing hierarchical construction
  \eqn{\phi \sim \mathrm{Gamma}(\alpha, \beta),
       \alpha \sim \mathrm{Gamma}(1,1),
       \beta \sim \mathrm{Gamma}(1,1)}
  has been replaced by brms's own default
  \eqn{\phi \sim \mathrm{Gamma}(0.01, 0.01)} (lower bound 0).
  The wrapper no longer declares \code{alpha} or \code{beta} as
  Stan parameters and no longer injects \code{stanvars} sampling
  statements for them.  Rationale: the \eqn{\mathrm{Gamma}(1, 1)}
  prior on \eqn{\alpha} (declared as \code{real<lower=1>}) was on
  the boundary of its support, producing divergent transitions on
  weakly-informative data; the extra layer also inflated the
  effective posterior dimension for what is essentially one scalar
  parameter per area model.  \strong{Migration:} users who relied
  on the old construction can build it manually via the
  \code{stanvars} + \code{prior} arguments; see
  \code{?hbm_betalogitnorm} for the migration note.  Legacy code
  that still passes \code{stanvars} containing sampling statements
  on \code{alpha} / \code{beta} now raises an informative error
  pointing the user at the new mechanism.
* **`sae_benchmark()` defaults made semantically explicit.**  New
  argument `target_type = c("total", "mean")` (default `"total"`)
  is consulted only when `weights = NULL`, choosing a safe default
  weighting (`rep(1, n)` for total, `rep(1 / n, n)` for mean).  An
  informational message is emitted so the chosen weighting is
  always visible.  Production users should still pass explicit
  `weights = N_i` (population size per area).

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
* **Vignettes updated to reflect v1.0.0 changes.**  The
  `hbsaems-betalogitnorm-model` vignette now documents two modes
  for $\phi$ (Random and Fixed) instead of three, and includes a
  legacy-reproduction recipe for users who need the pre-v1.0.0
  hierarchical hyperprior.  The `migration-guide` vignette gained
  two new sections covering the phi prior simplification and the
  critical link function override fix.  The `hbsaems-lnln-model`
  vignette adds an implementation note explaining why the
  `sampling_variance` offset is now interpreted on the natural
  (untransformed) scale.

## New documentation

* **GitHub Actions: automated pkgdown deployment.**  Three workflows
  live in `.github/workflows/`:
  \itemize{
    \item `R-CMD-check.yaml` -- run `R CMD check --as-cran` on five
          OS x R-version combinations on every push and PR.  This
          existed in earlier development snapshots and is kept here
          for completeness.
    \item `vignettes.yaml` -- path-filtered render check of both
          the CRAN vignette and the pkgdown-only articles, with the
          rendered HTMLs uploaded as a CI artifact for PR review.
          Catches Rmd-level breakage in seconds, well before the
          full website build.
    \item `pkgdown.yaml` -- on every push to main and on each
          release, build the full pkgdown site (covering the one
          CRAN vignette plus the ten website articles) and deploy
          to the `gh-pages` branch using the official
          \code{pkgdown::build_site_github_pages()} +
          \code{JamesIves/github-pages-deploy-action} flow.  A
          verification step fails the build if any of the core
          articles (\code{complete-workflow},
          \code{ast-formula-manipulation}, \code{hbsaems-modelling},
          \code{advanced-features}) was not rendered, so an empty
          or truncated site never reaches production.  Pull-request
          runs render the site as a downloadable artifact for
          review without deploying.
  }
  A new \code{PKGDOWN_DEPLOY_GUIDE.md} at the repository root
  explains first-time setup (GitHub Pages settings, Actions write
  permissions, URL field in DESCRIPTION) and the local
  pkgdown::build_site() preview workflow.  The guide is
  Rbuildignored so it does not bloat the CRAN tarball.

* **Vignette strategy reorganised for the CRAN release.**  Through
  v1.0.0 development we built up an extensive collection of
  vignettes covering every distribution-specific wrapper, every
  advanced topic, and the internals.  Eleven vignettes was a great
  documentation set but a heavy load for a CRAN tarball; CRAN's
  guidance for new submissions favours a small set of essential
  vignettes.  We now ship **only one vignette** in the tarball,
  `complete-workflow`, which is the canonical end-to-end SAE
  pipeline using `hbm()` and the standard Bayesian workflow
  diagnostics.  The remaining ten files are kept in the source
  repository under `vignettes/articles/` (a pkgdown convention)
  and are rendered as articles on the package website at
  <https://madsyair.github.io/hbsaems/>.  R CMD build skips
  `vignettes/articles/` via `.Rbuildignore`, so the CRAN tarball
  is leaner (-46 KB) without losing any documentation -- users
  who install from CRAN still get the website link via the
  `URL:` field in DESCRIPTION.

* **New article: "AST-based Formula Manipulation".**  A walkthrough
  of how `hbsaems` rewrites user formulas internally to apply the
  `nonlinear`, `measurement_error`, `area_var`, `sampling_variance`,
  and `handle_missing` sugar -- and why a regex-based approach
  would silently corrupt many legitimate formulas.  Available at
  <https://madsyair.github.io/hbsaems/articles/ast-formula-manipulation.html>.

## Bug fixes

* **`vignettes/articles/hbsaems-betalogitnorm-model.Rmd` showed a
  legacy code block that no longer runs.**  The article had a
  "Reproducing the legacy hierarchical hyperprior" section that
  demonstrated the pre-v1.0.0 \eqn{\phi \sim \mathrm{Gamma}(\alpha,
  \beta)} construction by calling
  \code{hbm_betalogitnorm(..., stanvars = stanvar("alpha ~
  gamma(1, 1);", ...) + stanvar("beta ~ gamma(1, 1);", ...))}.
  As of v1.0.0, \code{hbm_betalogitnorm()} explicitly rejects
  that pattern at construction time with an informative error
  (\code{alpha}/\code{beta} are no longer declared as Stan
  parameters by the wrapper).  Users who copy-pasted the
  article's example would have hit the runtime error.

  The section has been rewritten:
  \itemize{
    \item the recommended v1.0.0 pattern (a phi-prior via
          \code{prior = brms::set_prior("gamma(2, 0.05)", class =
          "phi")}) is shown first;
    \item the legacy-reproduction example now uses
          \code{hbm_flex()} instead of \code{hbm_betalogitnorm()},
          which does not apply the wrapper-specific guard, so the
          code actually runs.  The generated Stan code includes
          the expected \code{lprior += gamma_lpdf(phi | alpha,
          beta);} declaration, verified by inspecting
          \code{brms::make_stancode()} output.
  }
  The migration-guide article has been updated to cross-reference
  the new corrected example.

* **Article YAML headers used `rmarkdown::html_vignette`.**  Files
  under \code{vignettes/articles/} are pkgdown-only and no longer
  carry a `\\VignetteIndexEntry{}` field (that field is what
  \code{html_vignette} relies on for the page title).  Manually
  rendering these files with \code{rmarkdown::render()} would emit
  a benign warning about the missing field.  We now use
  \code{rmarkdown::html_document} for articles, which works
  identically when rendered by pkgdown but is silent under direct
  \code{rmarkdown::render()} too -- helpful for contributors
  previewing changes locally.

* **`?register_hbsae_brms_custom` example was non-idempotent.**
  The example registered \code{loglogistic_user} every time it ran
  but never cleaned up.  Running the example a second time in the
  same R session (which R CMD check's example runner can do, and
  which interactive users routinely do) would error with
  \emph{"Model 'loglogistic_user' is already registered.  Pass
  overwrite = TRUE to replace it"}.  The example now removes any
  pre-existing \code{loglogistic_user} entry before registering
  and unregisters at the end, so re-runs are clean.

* **\code{inst/examples/} subdirectory removed.**  The
  \code{inst/examples/hbsaems-examples.Rmd} file (31 KB) shipped
  with the package because it lived under \code{inst/}.  Its
  content was redundant with the curated articles on the package
  website and bloated the CRAN tarball.  Also removed three
  internal maintainer documents that had drifted into
  \code{inst/}: \code{inst/PKGDOWN_DEPLOY_GUIDE.md} (duplicate of
  the root version), \code{inst/YAML_FILES_GUIDE.md} (internal
  note), and \code{inst/DEPRECATED.md} (moved to repository
  root and Rbuildignored).  Only the runtime-essential
  \code{inst/WORDLIST}, \code{inst/shiny/}, and \code{inst/stan/}
  remain.  Net reduction in installed package size: ~50 KB.

* **`?hbm_betalogitnorm` example 3 used the removed
  alpha/beta hyperprior pattern.**  R CMD check
  \code{--run-donttest} caught a leftover example block that
  attempted to declare priors on \code{alpha} and \code{beta}
  via \code{stanvars}, both of which were Stan parameters in
  the pre-v1.0.0 hierarchical phi construction but were removed
  in this release (see the migration note at the top of
  \code{?hbm_betalogitnorm}).  Example 3 has been rewritten to
  demonstrate the supported v1.0.0 pattern -- a non-default phi
  prior is passed via brms's standard
  \code{prior = brms::set_prior("gamma(2, 0.5)", class = "phi")}
  rather than via the legacy stanvars sampling statements.  The
  resulting Stan code uses \code{phi ~ gamma(2, 0.5)} as
  intended.

* **R CMD check NOTE: `tests/spelling.Rout.save` comparison.**
  R CMD check was diffing the live spelling test output against
  a saved snapshot, which broke any time a new acceptable term
  appeared in NEWS or vignettes (e.g.\ \code{pred} from an
  internal predictions data structure).  We have removed the
  \code{tests/spelling.Rout.save} comparison file and rely solely
  on \code{spelling::spell_check_test(skip_on_cran = TRUE)}, which
  uses the proper \code{inst/WORDLIST} mechanism.  This is the
  approach recommended by Jeroen Ooms (author of \pkg{spelling})
  for packages that want spelling checks during development but
  not on CRAN's machines.

* **AST-based formula rewriting silently dropped `offset()` terms.**
  Both \code{.replace_nl_in_formula()} (used by the
  \code{nonlinear=} sugar) and \code{.apply_measurement_error()}
  (used by the \code{measurement_error=} sugar) decompose the
  formula RHS via \code{stats::terms()} and reassemble it with
  \code{stats::reformulate()}.  The reassembly step iterated only
  over \code{attr(terms, "term.labels")}, which is the list of
  predictor terms \emph{excluding} \code{offset()} -- those live in
  a separate \code{attr(terms, "offset")} attribute as integer
  positions into \code{attr(terms, "variables")}.  As a result, a
  user formula like
  \code{y ~ x1 + offset(log(pop)) + x2} called with
  \code{nonlinear = "x1"} would silently lose the offset, producing
  \code{y ~ s(x1) + x2}.
  Both helpers now extract the offset terms and splice them back
  into the assembled RHS verbatim.  Eight regression tests cover
  offset preservation, substring-name safety, wrapper preservation
  (I, poly, me), interaction-term preservation, and the
  hierarchical \code{.build_area_re_formula()}.

* **Conflict protection: custom brms families could silently shadow
  built-in or brms-native families.**  Three classes of conflict
  were previously possible:
  \enumerate{
    \item Overwriting hbsaems built-in custom families.
          \code{register_hbsae_brms_custom(key = "loglogistic", ...)}
          silently replaced the package-curated \code{loglogistic}
          family because the override check only inspected the
          brms-native list and missed
          \code{loglogistic}/\code{shifted_loglogistic} (which were
          registered separately by \code{.onLoad()}).
    \item Shadowing brms-native families.  A user calling
          \code{register_hbsae_brms_custom(key = "weibull", ...)}
          inserted a custom record that
          \code{.resolve_family_object()} would consult \emph{before}
          falling back to \code{brms::brmsfamily()}, so subsequent
          \code{hbm(hb_sampling = "weibull")} calls silently
          dispatched to the user's stub rather than to brms's
          built-in weibull family.
    \item Stale custom registrations leaking past
          \code{.init_model_registry()} resets.
  }

  This release introduces a comprehensive \code{.builtin_keys()}
  list that covers both brms-native AND hbsaems-bundled custom
  families, makes the override-protection logic in both
  \code{register_hbsae_model()} and
  \code{register_hbsae_brms_custom()} consult it, adds a
  \code{.is_brms_native_family()} probe so registrations that would
  shadow a brms-native family emit an informative warning pointing
  the user at the \code{hbsae_} prefix convention, and rewrites
  \code{.init_model_registry()} to clear non-builtin keys before
  re-asserting the built-ins.

  An internal option (\code{hbsaems.in_builtin_registration}) lets
  \code{.register_builtin_custom_families()} bypass the new check
  during package load, avoiding a chicken-and-egg circular
  dependency.

  Six regression tests cover all paths.

* **`dloglogistic()` errored when `mu` or `beta` was NA.**
  \code{stopifnot(all(mu > 0))} treats \code{NA} as failure, so
  \code{dloglogistic(1, mu = NA, beta = 1)} stopped with
  \emph{"all(mu > 0) is not TRUE"}.  Now NAs in parameters
  propagate as NA in the result (matching the v1.0.0 NA-in-x fix
  and the wider R density-function convention), while finite
  non-positive parameters still error explicitly.

* **Stan function names safely prefixed with `hbsae_`.**  Audited
  \code{inst/stan/hbsae_loglogistic.stan} and
  \code{inst/stan/hbsae_shifted_loglogistic.stan} to confirm every
  Stan-side function declaration uses the \code{hbsae_} prefix,
  avoiding collision with Stan 2.29+'s built-in
  \code{loglogistic_lpdf}.  This was already correct in the code;
  the audit added documentation comments and a verifying dev-test.

* **`dloglogistic()` silently coerced NA inputs to 0.**  Base R
  density functions follow the convention that
  \code{dnorm(NA, 0, 1) = NA}, but our \code{dloglogistic(NA, ...)}
  returned 0 (the density at \eqn{x \le 0}, which is also 0).  The
  vectorised case was particularly misleading because a partial NA
  vector produced no warning even though one position was being
  silently replaced with 0.  We now propagate NA from any of
  \code{x}, \code{mu}, or \code{beta} -- matching the
  \code{dshifted_loglogistic} behaviour and the wider R density-
  function convention.

* **`run_sae_app()` produced a cryptic error when \pkg{shiny} was
  missing.**  Because \pkg{shiny} lives in \code{Suggests}, users
  who installed hbsaems with only the modelling dependencies would
  see "could not find function runApp" rather than an actionable
  error.  We now check \code{requireNamespace("shiny")} at the top
  of \code{run_sae_app()} and emit an informative error pointing
  to \code{install.packages("shiny")}.

* **`run_sae_app(check_deps = FALSE)` carried stale `optional_missing`
  state across sessions.**  If the user previously launched with
  \code{check_deps = TRUE} (which writes
  \code{options(hbsaems.shiny_missing_optional = ...)}) and then
  re-launched with \code{check_deps = FALSE}, the GUI banner would
  still display the cached missing-package list.  We now clear the
  option to \code{character(0L)} in the \code{FALSE} branch.

* **`DESCRIPTION` Suggests: missing `energy` and `minerva`.**  The
  Shiny app's exploratory-correlation panel (Distance correlation,
  MIC) depends on these two packages, but \code{check_shiny_deps()}
  was the only place that referenced them.  Without listing them in
  Suggests, downstream automated checks (e.g.\ \code{R CMD check
  --as-cran}) would have flagged the unconditional
  \code{requireNamespace()} call as a NOTE.  Both packages are now
  in Suggests as documented in \code{check_shiny_deps()}.

* **Bundled data: HIERARCHICAL CONSISTENCY across datasets.**  All
  four bundled datasets now use the same administrative-level
  labels with the same meanings:
  \itemize{
    \item `province` -- Level 1 (Indonesian provinsi).
    \item `regency`  -- Level 2 (Indonesian kabupaten/kota; analogue
          of US county).
    \item `district` -- Level 3 (Indonesian kecamatan; analogue of
          US census tract).
  }
  What differs between datasets is the analysis resolution -- i.e.\
  at which level the 100 small-area observations live:
  \itemize{
    \item `data_fhnorm`, `data_betalogitnorm`: 100 regencies nested
          within 5 provinces (analysis at regency level).
    \item `data_binlogitnorm`, `data_lnln`: 100 districts nested
          within 5 regencies (analysis at district level; "province"
          omitted because it would be a single value above 5
          regencies and contribute no variation).
  }
  Label format conventions disambiguate at a glance:
  three-digit suffixes (\code{regency_001..100},
  \code{district_001..100}) for the 100-level fine analysis areas
  and two-digit suffixes (\code{province_01..05},
  \code{regency_01..05}) for the 5-level coarse spatial clusters.
  Through earlier v1.0.0 iterations the same role was variously
  called "regency" (creating dual-semantics confusion), "kabupaten"
  (Indonesian-only), and "county" (US-only); none of those reached
  CRAN.  The matrix has been correspondingly renamed
  `adjacency_matrix_car_regency` (5x5, rownames
  \code{regency_01..05}).  Total impact on data folder vs original
  pre-audit state is **+4 bytes** (29822 -> 29826) -- effectively
  neutral.

* **Documentation: incorrect spatial pairings in three examples.**
  Two SAR examples paired `spatial_weight_sar` (100x100,
  regency-named) with `spatial_var = "province"` (5 levels) --
  this would error at fit time because brms cannot resolve a
  5-level grouping factor against a 100-row weight matrix.  One
  BYM2 example paired `data_binlogitnorm` (which has no
  `province` column) with `spatial_var = "province"`.  All three
  examples in `?hbm`, `vignette('hbsaems-spatial')`, and inline
  documentation have been corrected.

* **`print.hbcc_results()` reported PASS on all-NA Rhat.**  Same
  pattern as the \code{is_converged()} fix, but in the print
  method: \code{all(rh < 1.1, na.rm = TRUE)} returns TRUE on
  \code{numeric(0)} and silently displayed \emph{"R-hat < 1.1 :
  PASS"} for degenerate fits with no finite Rhat values.  Now
  reports \emph{"R-hat < 1.1 : ?? (all NA -- model fit may be
  degenerate)"}.

* **`print.hbsae_results()` printed `NaN%` and `Inf to -Inf`.**
  When \code{rse_model} is NA / NaN (all areas had zero predictions
  -- see the v1.0.0 RSE guard in \code{sae_predict()}) the print
  method would output literal \code{"NaN %"} and \code{"Inf to -Inf"}
  for the prediction range.  Both are now handled with explicit
  branches that label them as \code{"NA (no finite predictions)"}.

* **Bundled-data integrity now checked at every test run.**  A new
  \code{test-data-integrity.R} test file verifies that:
  \itemize{
    \item the four datasets retain their documented columns and
          dimensions,
    \item all key columns are non-NA, finite, and within their
          domains (\code{y} in \eqn{(0, 1)} for the Beta data,
          \code{y <= n} for the binomial data, \code{psi_i > 0} for
          the lognormal data, etc.),
    \item the documented pairings between adjacency matrices and
          datasets are satisfied at the level of their row-name /
          factor-level coincidence.
  }
  These tests would have caught the historical "regency means 100
  levels in two datasets and 5 levels in two others" mismatch
  before it shipped.

* **`hbm_warnings()` failed to flag degenerate model fits.**  The
  R-hat convergence check used \code{!all(rh < 1.1, na.rm = TRUE)},
  which evaluates to \code{!TRUE} (i.e.\ no warning fires) when all
  Rhat values are NA -- the textbook signature of a sampler that
  produced no useful draws.  Same issue affected the
  \code{neff_ratio} check.  Both now detect the all-NA / all-non-
  finite case explicitly and emit a dedicated
  \dQuote{model fit appears degenerate} warning.

* **`register_hbsae_model()` silently overwrote built-in families.**
  Passing \code{overwrite = TRUE} for a built-in key
  (\code{"gaussian"}, \code{"beta"}, \code{"lognormal"},
  \code{"binomial"}, etc.) silently replaced the package-curated
  spec, losing the validation rules and link handling that the
  built-in provides.  Without \code{overwrite = TRUE} the function
  now refuses to touch a built-in; with \code{overwrite = TRUE} it
  emits a warning so the choice is conscious.

* **`register_hbsae_model()` accepted invalid registry keys.**  Keys
  with whitespace (\code{"my key"}), leading digits
  (\code{"3factor"}), or other punctuation that would force
  backtick-quoting are not usable as R names and broke downstream
  registry access.  Keys are now validated with \code{make.names()}.
  Empty-string keys are also rejected.

* **`register_hbsae_model()` did not validate
  \code{response_check_msg} type.**  Non-character or vector-of-
  strings values would either error cryptically downstream or
  produce garbled \code{stop()} messages.  Now validated as
  \code{NULL} or a single character string.

* **`register_hbsae_brms_custom()` had thinner validation than its
  sibling.**  The brms-custom-family registration path used a bare
  \code{stopifnot()} for the key, didn't validate
  \code{response_check}, \code{response_check_msg},
  \code{supports_mi}, or \code{discrete}, and let callers silently
  shadow built-in families.  Validation now matches
  \code{register_hbsae_model()}.

* **`is_converged()` returned silent TRUE on all-NA Rhat.**  Both
  S3 methods relied on \code{all(rhat < threshold, na.rm = TRUE)},
  which returns TRUE for \code{numeric(0)} after the NA's are
  stripped -- producing a degenerate "converged" signal whenever
  the underlying brmsfit had no finite Rhat values (e.g.\ a
  collapsed sampler, all chains diverged, mock object).
  \code{is_converged()} now returns \code{NA} with an informative
  warning in that case.

* **`is_converged()` did not validate the `threshold` argument.**
  A string threshold (\code{"1.1"}), \code{NULL}, length>1 vector,
  negative value, or non-finite value all silently returned TRUE
  due to R's coercion rules in \code{<} and \code{all()}.  We now
  reject those inputs explicitly via \code{.check_rhat_threshold()}.

* **`sae_benchmark()` silently produced NA / Inf when `target` was
  non-finite.**  Passing \code{target = NA} or \code{target = Inf}
  propagated through the ratio / difference computation and
  produced an all-NA / all-Inf benchmarked table.  Both are
  now rejected with informative errors.  Likewise, non-finite
  values in \code{predictions$pred} now raise an early error
  pointing the user back to the model-fit step.

* **Legacy \code{aux_param_hyperprior} callback removed from the Beta
  family registry.**  The pre-v1.0.0 hierarchical hyperprior
  $\phi \sim \mathrm{Gamma}(\alpha, \beta)$ with
  $\alpha, \beta \sim \mathrm{Gamma}(1, 1)$ was previously hard-wired
  in the registry under \code{models-registry.R}.  That made
  \code{hbm_flex(family_key = "beta", aux_args = list(n = "n", deff
  = "deff"))} silently re-inject the old construction even after
  \code{hbm_betalogitnorm()} was simplified to use brms's default
  \eqn{\mathrm{Gamma}(0.01, 0.01)} prior in
  \href{#}{phi prior simplification}.  The callback has been removed
  from the built-in spec; custom families can still register one via
  \code{\link{register_hbsae_model}}.

* **`hbm_flex()` length-1 area_var validation crashed on length>1 vectors.**
  When the user supplied an `area_var` character vector of length 2 or
  more (the hierarchical-area mode), the validation
  `!is.null(area_var) && !(area_var %in% names(data))` raised the R 4.2+
  error \emph{"'length = 2' in coercion to 'logical(1)'"} -- the `&&`
  operator does not coerce length>1 logicals to scalar.  Replaced with
  a `setdiff()`-based check that produces an informative error listing
  every missing column.  Same idiom applied defensively to
  `spatial_var` (which is always length-1 by API design).

* **`re = ~ (1 | x/y)` nested syntax rejected by `re` validator.**
  `.build_area_re_formula()` synthesises nested random-effect terms
  with the lme4 sugar \code{(1 | g1/g2)}, but the regex in
  `hbm()` only accepted \code{(1 | g)} and \code{(1 | g1 + g2)}.
  The regex now accepts the `/` separator as well, so hierarchical-
  area models built via `area_var = c("province", "regency"),
  area_re_structure = "nested"` reach brms with the correct formula.

* **`sae_aggregate(method = "weighted")` with all-zero weights produced NaN.**
  The internal normalisation `weights / sum(weights)` silently
  produced NaN when all weights were zero (and Inf when sum was
  negative).  We now validate that the weight sum is strictly
  positive and that all individual weights are finite, raising
  informative errors otherwise.

* **`sae_transform()` recycled scalar return values silently.**
  A user passing a reducer function (e.g.\ `fun = sum`, `fun = mean`)
  instead of an element-wise transform (`fun = log`, `fun = exp`) saw
  silent scalar recycling -- every area's prediction became identical.
  We now validate that `fun(pred)` returns a numeric vector of the
  same length as the input and emit an informative error otherwise.

* **`update_hbm()` failed silently when newdata lacked offset columns.**
  Models fitted with \code{sampling_variance}, \code{n} + \code{deff}
  (in \code{hbm_betalogitnorm}), or \code{fixed_params} attach hidden
  offset columns (\code{.hbsaems_<par>_fixed}) to the model data
  frame.  When the user passed a \code{newdata} that lacked these
  columns to \code{update_hbm()}, brms refused to refit with the
  unhelpful error \emph{"variables can neither be found in 'data' nor
  in 'data2'"}.  \code{update_hbm()} now detects the case and:
  \itemize{
    \item warns and copies the offset columns over from
          \code{object$data} when \code{nrow(newdata) ==
          nrow(object$data)} (the typical "same areas, updated
          covariates" use case), or
    \item raises an informative error pointing the user to either
          supply the columns in \code{newdata} explicitly or refit
          from scratch with the original sugar arguments
          (\code{sampling_variance}, \code{n}+\code{deff},
          \code{fixed_params}).
  }
  Three regression tests added.

* **Multivariate (mi() / joint) models failed in
  \code{sae_predict()}, \code{prior_check()}, and
  \code{model_compare()}.**  brms's \code{posterior_predict()} returns
  a 3-D array of shape (draws x obs x responses) for multivariate
  formulas (e.g.\ \code{bf(y | mi() ~ ...) + bf(x1 | mi() ~ ...)});
  \code{colMeans()} and \code{apply()} downstream produced 2-D
  outputs that broke the construction of \code{result_table}.
  Additionally, \code{brms::pp_check()} requires an explicit
  \code{resp = <name>} argument for multivariate models, which we
  were not providing.  All three functions now detect the
  multivariate case, default \code{resp} to the first sub-formula's
  response (the conventional SAE target), and forward it
  appropriately.  Power users can override via \code{resp = ...} in
  the \code{...} argument.

* **CRITICAL: `.add_fixed_pforms()` failed on multivariate formulas.**
  When the user combined \code{mi()} joint-imputation formulas
  (e.g.\ \code{bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ x2)})
  with \code{sampling_variance} / \code{fixed_params}, the helper
  attempted to append \code{sigma ~ 0 + offset(.hbsaems_sigma_fixed)}
  via \code{bform + brms::lf(rhs)}.  brms refused this because the
  dpar formula did not specify which response (\code{y} or
  \code{x1}) the \code{sigma} applies to, producing
  \emph{"Don't know how to add a list object without the response
  variable name"}.  The helper now detects \code{mvbrmsformula}
  objects, extracts the primary response (first sub-formula by
  convention), and passes \code{resp = <primary>} to \code{lf()}.
  Regression tests in \code{test-additions.R} cover both the
  end-to-end \code{mi() + sampling_variance} flow and the helper
  in isolation.

* **CRITICAL: `sampling_variance` /
  \code{fixed_params\$sigma} silently corrupted by brms's default
  log link on `sigma`.**  When the user supplied
  \code{sampling_variance = "D"}, the wrapper stored
  \eqn{\sqrt{D_i}} in \code{.hbsaems_sigma_fixed} and attached it
  to the brms formula as \code{sigma ~ 0 + offset(.hbsaems_sigma_fixed)}.
  However, brms applies the dpar's link function before plugging
  the linear predictor into the likelihood; for Gaussian /
  Lognormal / Student families the default \code{link_sigma = "log"}
  caused the Stan model to compute \code{sigma = exp(sqrt(D_i))}
  instead of \eqn{\sqrt{D_i}}.  E.g.\ \eqn{D_i = 4} should give
  \eqn{\sigma_i = 2.0} but actually produced \eqn{\sigma_i \approx
  e^{2.0} \approx 7.39} -- a catastrophic miscalibration of the
  Fay-Herriot model.  Same bug affected any user pinning sigma via
  the generic \code{fixed_params = list(sigma = ...)}, and would
  also have affected phi for Beta if \code{hbm_betalogitnorm()}
  had not already manually set \code{link_phi = "identity"}.  The
  fix forces \code{link_<par> = "identity"} on the family object
  for every pinned dpar, so the offset values are passed verbatim
  to the likelihood.  Three regression tests added to
  \code{test-additions.R} guard against this regression.

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
* **`sae_benchmark()` scale corruption fix.**  The previous default
  `weights = rep(1 / n, n)` silently assumed `target` was a population
  mean.  When users instead passed a population total, the implied
  ratio adjustment came out roughly \eqn{n} times too large --
  benchmarked estimates were scale-corrupt by a factor of \eqn{n}.
  Fixed by introducing the explicit `target_type` argument (see New
  features) and emitting a message when the default kicks in.
* **`sae_scale()` zero-variance NaN propagation.**  When all area
  predictions were identical, `base::scale()` produced `NaN`
  throughout, silently corrupting `result_table`.  Now detected
  with a warning; the method returns centred-only (or raw)
  predictions instead.
* **`hbm_betalogitnorm()` divergent-transitions on random `phi`.**
  The prior default `link_phi = "identity"` together with the
  hyperprior \eqn{\phi \sim \mathrm{Gamma}(\alpha, \beta)} let NUTS
  propose negative \eqn{\phi}, triggering \eqn{\log(0)} evaluations.
  Default is now resolved conditionally (see New features).
* **dev-tests `library(hbsaems)` auto-attach.**  The helper file
  `tests/testthat/dev-tests/helper-dev-setup.R` now attaches the
  package automatically, fixing 150+ spurious `could not find
  function` errors when dev-tests were run via
  `testthat::test_dir()`.
* **`hbm_betalogitnorm()` early validation of `spatial_var`.**
  When the user supplied `spatial_var` referencing a column that
  did not exist in `data`, the error previously surfaced only
  inside brms with the unfriendly message *"variables can neither
  be found in 'data' nor in 'data2'"*.  The wrapper now validates
  the column up front and emits the friendlier *"Spatial variable
  '<name>' not found in `data`."* message, matching the equivalent
  guard for `area_var`.
* **`hbm()` `data2` collision with user-supplied `...`.**  Calling
  `hbm()` with both `spatial_model = "car"` (or `"sar"`) and a
  user-supplied `data2 = list(...)` via `...` crashed with the
  R-level error *"formal argument 'data2' matched by multiple
  actual arguments"*.  `hbm()` builds an internal `data2 = list(M = M)`
  for the spatial weight matrix and also splices `...` into the
  `brms::brm()` call, producing two `data2` keys at the
  \code{do.call()} site.  The fix extracts any user-supplied
  \code{data2} from \code{...} before constructing the brms argument
  list and merges it into the internal \code{data2} via
  \code{modifyList()} -- the spatial-matrix slot (\code{M}) supplied
  by hbsaems wins on collision, but additional user keys
  (e.g.\ auxiliary matrices for nonlinear models) are preserved.
  As a related hardening, attempts to override other internally-
  managed brms arguments (\code{save_pars}, \code{stanvars},
  \code{prior}, \code{control}, etc.) via \code{...} now raise an
  informative error pointing the user to the dedicated hbsaems
  argument.

## Internal

* **Test suite reorganisation.** Per the recommendation of an external
  code review, validation-only tests (those exercising only input
  validation, deprecation warnings, and error paths) have been split
  out from the heavy integration tests and migrated to
  \code{tests/testthat/}.  Each \code{test_that()} block was
  classified by static analysis: blocks whose only \code{hbm}/
  \code{hbm_*} call appears inside \code{expect_error}/
  \code{expect_warning} are CRAN-safe and now run on every check; the
  remaining ~60 integration tests that actually compile a Stan model
  stay in \code{tests/testthat/dev-tests/} and are gated by
  \code{NOT_CRAN=true} (or \code{_R_RUN_DEV_TESTS_=true}).  Net effect:
  CRAN test count rose from 391 to ~466 with no measurable runtime
  increase.  Internal helpers \code{.brm_stub()} and \code{.hbm_stub()}
  in \code{tests/testthat/helper-mocks.R} provide brms-compatible
  shells for the few tests that need to exercise the deprecation
  pipeline without compiling Stan.
* **Project polish.** Added `CONTRIBUTING.md` documenting the
  development workflow (test tiers, mock-stub usage, AST-based
  formula manipulation patterns, spelling regeneration); added a
  light `.lintr` configuration for contributors; updated
  `cran-comments.md` to describe the first-submission test
  environment and intentional notes.

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
