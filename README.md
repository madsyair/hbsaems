
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# hbsaems: Hierarchical Bayesian Area-Level Small Area Estimation Models <img src="man/figures/hbsaems.png" align="right" height="120" alt="hbsaems logo" />

**🌏 [Baca dalam Bahasa
Indonesia](https://github.com/madsyair/hbsaems/blob/main/README_ID.md)**

<!-- badges: start -->

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![CRAN
status](https://www.r-pkg.org/badges/version/hbsaems)](https://CRAN.R-project.org/package=hbsaems)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/hbsaems)](https://CRAN.R-project.org/package=hbsaems)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub](https://img.shields.io/badge/GitHub-madsyair/hbsaems-blue.svg)](https://github.com/madsyair/hbsaems)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Overview

**hbsaems** is an R package for **area-level Hierarchical Bayesian Small
Area Estimation (HBSAE)**. The methodological foundation follows the
standard SAE literature – primarily **Rao and Molina (2015)** – while
the computational implementation is adapted to the parameterisation and
prior-specification conventions of the **brms** package (Bürkner, 2017),
which targets the **Stan** back-end.

The package is designed to support the principled **Bayesian workflow**
(Gelman and others, 2020) so that SAE modelling becomes systematic:
prior predictive checks, MCMC convergence diagnostics, posterior
predictive checks, leave-one-out cross-validation, Bayesian model
comparison and averaging, prior sensitivity analysis, and design-
consistent benchmarking are all part of the standard pipeline.

> **Scope.** This package focuses on **area-level** SAE models. Unit-
> level SAE models (e.g. the nested error model of Battese, Harter &
> Fuller, 1988) are not the focus.

## Key features

- **Area-level SAE models**: Fay-Herriot normal, lognormal-lognormal,
  beta logit-normal, and binomial logit-normal – each adapted from Rao
  and Molina (2015) to the brms parameterisation.
- **SAE-idiomatic argument naming** (v1.0.0): arguments named after SAE
  concepts (`auxiliary`, `area_var`, `spatial_var`, `spatial_model`,
  `sampling_variance`, `n`, `deff`).
- **Survey-design-informed fixed parameters**:
  - Beta precision: $\phi_i = n_i / \mathrm{deff}_i - 1$ pinned via
    offset.
  - Lognormal: $\sigma_i = \sqrt{\psi_i}$ pinned via offset (Fay-Herriot
    lognormal).
- **Generic `fixed_params` mechanism** for power users and custom
  distributions.
- **Spatial random effects**: CAR (ICAR / proper / BYM2) and SAR (lag /
  error).
- **Custom brms distributions**: built-in Loglogistic and Shifted
  Loglogistic, plus a registration framework for user-defined families.
- **Missing data**: listwise deletion, multiple imputation (`mice`), or
  joint Bayesian imputation (`brms::mi()`) with auto-selection.
- **Comprehensive Bayesian workflow**: prior predictive checks, MCMC
  convergence diagnostics, posterior predictive checks, LOO-CV, Bayesian
  model comparison and averaging, prior sensitivity analysis via
  `priorsense`, design-consistent benchmarking.
- **Shrinkage priors**: Horseshoe and R2D2 selectable via `prior_type`.
- **Nonlinear smooth terms**: thin-plate splines and Gaussian processes.
- **Interactive Shiny application**: bilingual (English/Indonesian) GUI
  via `run_sae_app()`.
- **Ten comprehensive vignettes** covering every supported model and
  advanced topic.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("madsyair/hbsaems")
```

Or with vignettes:

``` r
devtools::install_github("madsyair/hbsaems", build_vignettes = TRUE)
```

## Quick start

``` r
library(hbsaems)

# Load example data
data("data_fhnorm")
str(data_fhnorm[, c("regency", "province", "y", "D", "x1", "x2")])

# Fit a basic Fay-Herriot Normal model with an IID area effect
model <- hbm(
  formula     = bf(y ~ x1 + x2 + x3),
  hb_sampling = "gaussian",
  hb_link     = "identity",
  re          = ~(1 | regency),
  data        = data_fhnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)

# Check the model summary
summary(model)
```

## The three-layer function family

                          hbm()
                          Universal: any brms family, full customisation
                                           ▲
                                           |
                          hbm_flex()
                          Family registry + auxiliary + fixed_params
                                           ▲
                                           |
           hbm_lnln() / hbm_betalogitnorm() / hbm_binlogitnorm()
           SAE-friendly wrappers: response, auxiliary, area_var, ...

Most users start with a wrapper. Step up to `hbm_flex()` when you need a
custom family or the generic `fixed_params` interface; step up to
`hbm()` when you need full brms control.

## Core functions

### Main modeling functions

#### `hbm()` – universal modeling function

``` r
model <- hbm(
  formula        = bf(y ~ x1 + x2 + x3),  # brms formula
  hb_sampling    = "gaussian",            # likelihood family
  hb_link        = "identity",            # link function
  re             = ~(1 | regency),        # IID area RE (formula)
  spatial_var    = "province",            # spatial RE column
  spatial_model  = "car",                 # "car" or "sar"
  M              = adjacency_matrix_car,  # spatial weight matrix
  data           = data_fhnorm
)
```

#### Distribution-specific wrappers

**Beta logit-normal** for proportions in $(0, 1)$:

``` r
fit_beta <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  n         = "n",                # sample size column
  deff      = "deff",             # design effect column
  area_var  = "regency",          # area random effect
  data      = data_betalogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

**Binomial logit-normal** for successes out of trials:

``` r
fit_bin <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  area_var  = "district",         # 100 districts in this dataset
  data      = data_binlogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

**Lognormal-lognormal** (Fay-Herriot variant) for positive, right-skewed
responses:

``` r
fit_lnln <- hbm_lnln(
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  area_var     = "district",
  sampling_variance = "psi_i",          # known sampling variance (log scale)
  data         = data_lnln,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

### Workflow functions

| Function | Purpose | Replaces (legacy) |
|----|----|----|
| `convergence_check()` | MCMC convergence diagnostics: $\hat R$, ESS, Geweke, Heidelberger, Raftery | `hbcc()` |
| `model_compare()` | LOO/WAIC/Bayes-factor model comparison and posterior predictive checks | `hbmc()` |
| `model_compare_all()` | Multi-model ranking analogous to `loo_compare` | – |
| `model_average()` | Bayesian model averaging (manual, stacking, or pseudo-BMA+ weights via `loo`) | – |
| `prior_check()` | Prior predictive checks | `hbpc()` |
| `prior_sensitivity()` | Power-scale prior sensitivity diagnostics (`priorsense`) | – |
| `sae_predict()` | In-sample and out-of-sample SAE prediction | `hbsae()` |
| `sae_benchmark()` | Design-consistent benchmarking | – |

### Auxiliary functions

| Function | Purpose |
|----|----|
| `check_data()` | Data integrity checks (missingness, duplicates, sample size) |
| `check_spatial_weight()` | Spatial weight matrix theoretical-compatibility checks |
| `build_spatial_weight()` | Build adjacency / row-standardised weight matrices |
| `is_converged()` | Quick yes/no convergence check |
| `posterior_interval()` | Credible intervals for posterior draws |
| `posterior_draws()` | Extract posterior draws as a long data frame |
| `hbm_info()` | Inspect model spec, priors, sampler settings |
| `hbm_warnings()` | Surface model fit warnings |

## Custom distributions

`hbsaems` ships with two custom brms families for positive, right-
skewed data:

- **Loglogistic** – canonical Fisk parameterisation following
  `flexsurv::dllogis` and Wikipedia. See `?loglogistic` and
  `?brms_custom_loglogistic`.
- **Shifted Loglogistic** – GEV-style parameterisation of Hosking &
  Wallis (1997). See `?shifted_loglogistic` and
  `?brms_custom_shifted_loglogistic`.

Both are auto-registered with brms at package load via
`register_hbsae_brms_custom()`.

## Interactive Shiny application

The package ships a bilingual (English / Indonesian) Shiny application:

``` r
run_sae_app()
```

The application provides a guided workflow: data upload → exploration →
spatial setup → modelling → diagnostics → results download.

## Vignettes

The package includes ten vignettes that progressively walk through the
modelling workflow:

``` r
browseVignettes("hbsaems")
```

| Vignette | Topic |
|----|----|
| `complete-workflow` | End-to-end Fay-Herriot example with full Bayesian workflow |
| `hbsaems-modelling` | Modelling concepts and the three-layer API |
| `hbsaems-betalogitnorm-model` | Beta logit-normal for proportions |
| `hbsaems-binlogitnorm-model` | Binomial logit-normal for counts |
| `hbsaems-lnln-model` | Lognormal-lognormal Fay-Herriot variant |
| `hbsaems-spatial` | CAR / SAR / BYM2 spatial models |
| `hbsaems-handle-missing` | Three missing-data strategies |
| `advanced-features` | Shrinkage priors, splines, custom families |
| `hbsaems-run_sae_app` | Using the Shiny application |
| `migration-guide` | Migrating from v0.x to v1.0.0 |

## Migrating from v0.x

If you are coming from v0.x of hbsaems, see the `migration-guide`
vignette. The most visible changes:

| Old (deprecated) | New (canonical)       |
|------------------|-----------------------|
| `hbcc()`         | `convergence_check()` |
| `hbmc()`         | `model_compare()`     |
| `hbpc()`         | `prior_check()`       |
| `hbsae()`        | `sae_predict()`       |
| `group =`        | `area_var =`          |
| `sre =`          | `spatial_var =`       |
| `sre_type =`     | `spatial_model =`     |
| `predictors =`   | `auxiliary =`         |
| `sampling_var =` | `sampling_variance =` |

The old names continue to work in v1.0.0 with a deprecation warning and
will be removed in v2.0.0.

## Citation

If you use **hbsaems** in published research, please cite:

``` r
citation("hbsaems")
```

## References

- Battese, G. E., Harter, R. M., & Fuller, W. A. (1988). An
  error-components model for prediction of county crop areas using
  survey and satellite data. *Journal of the American Statistical
  Association*, 83(401), 28-36.
- Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel
  models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
- Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter,
  B., Yao, Y., Kennedy, L., Gabry, J., Bürkner, P.-C., & Modrák, M.
  (2020). Bayesian workflow. *arXiv:2011.01808*.
- Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation* (2nd
  edition). Wiley.

## License

GPL (\>= 3)

## Authors

- **Achmad Syahrul Choir** (maintainer, author) – madsyair@stis.ac.id,
  Politeknik Statistika STIS, Jakarta
- Saniyyah Sri Nurhayati (author)
- Sofi Zamzanah (author)
- Arsyka Laila Oktalia Siregar (author)
