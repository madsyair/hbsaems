# hbsaems: Hierarchical Bayesian Area-Level Small Area Estimation Models <img src="man/figures/hbsaems.png" align="right" height="120" alt="hbsaems logo" />

**🌏 [Baca dalam Bahasa Indonesia](https://github.com/madsyair/hbsaems/blob/main/README_ID.md)**

<!-- badges: start -->
[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![CRAN status](https://www.r-pkg.org/badges/version/hbsaems)](https://CRAN.R-project.org/package=hbsaems)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/hbsaems)](https://CRAN.R-project.org/package=hbsaems)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub](https://img.shields.io/badge/GitHub-madsyair/hbsaems-blue.svg)](https://github.com/madsyair/hbsaems)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Overview

**hbsaems** is an R package for **area-level Hierarchical Bayesian Small Area Estimation (HBSAE)**. The methodological foundation follows the standard SAE literature — primarily **Rao and Molina (2015)** — while computational implementation is adapted to the parameterisation and prior-specification conventions of the **brms** package (Bürkner, 2017), which targets the **Stan** back-end.

The package is designed to support the principled **Bayesian workflow** (Gelman et al., 2020) so that SAE modelling becomes systematic: prior predictive checks, MCMC convergence diagnostics, posterior predictive checks, leave-one-out cross-validation, Bayesian model comparison and averaging, prior sensitivity analysis, and design-consistent benchmarking are all part of the standard pipeline.

> **Scope.** This package focuses on **area-level** SAE models. Unit-level SAE models (e.g., the nested error model of Battese, Harter & Fuller, 1988) are not the focus of this package.

## Key Features

- **Area-level SAE models**: Fay-Herriot normal, lognormal-lognormal, beta logit-normal, and binomial logit-normal — each adapted from Rao and Molina (2015) to the brms parameterisation
- **SAE-idiomatic API**: arguments named after SAE concepts (`auxiliary`, `group`, `sampling_var`, `n`, `deff`)
- **Survey-design-informed fixed parameters**:
  - Beta precision: $\phi_i = n_i / \text{deff}_i - 1$ pinned via offset
  - Lognormal: $\sigma_i = \sqrt{\psi_i}$ pinned via offset (Fay-Herriot lognormal)
- **Generic `fixed_params` mechanism** for power users and custom distributions
- **Spatial random effects**: CAR (ICAR / proper / BYM2) and SAR (lag / error)
- **Custom brms distributions**: built-in Loglogistic and Shifted Loglogistic, plus a registration framework for user-defined families
- **Missing data**: listwise deletion, multiple imputation (`mice`), or joint Bayesian imputation (`brms::mi()`) with auto-selection
- **Comprehensive Bayesian workflow**: prior predictive checks, MCMC convergence diagnostics, posterior predictive checks, LOO-CV, Bayesian model comparison and averaging, prior sensitivity analysis via `priorsense`, design-consistent benchmarking
- **Shrinkage priors**: Horseshoe and R2D2 selectable via `prior_type`
- **Nonlinear smooth terms**: thin-plate splines and Gaussian processes
- **Interactive Shiny dashboard**: bilingual (English/Indonesian) GUI via `run_sae_app()`
- **Ten comprehensive vignettes** covering every supported model and advanced topic

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("madsyair/hbsaems")
```

Or with vignettes:

```r
devtools::install_github("madsyair/hbsaems", build_vignettes = TRUE)
```

## Dependencies

The package requires:

- **brms** (Bayesian regression engine)
- **coda**, **posterior** (MCMC diagnostics)
- **ggplot2** (plotting)
- **mice** (multiple imputation)
- **shiny**, **shinydashboard**, **shinyWidgets**, **readxl**, **DT** (interactive app)
- **priorsense** (prior sensitivity analysis, optional)
- **sf**, **spdep** (spatial weight matrices, optional)

## Quick Start

```r
library(hbsaems)

# Load example data
data("data_fhnorm")

# Fit a basic Fay-Herriot Normal model
model <- hbm(
  formula     = bf(y ~ x1 + x2 + x3),
  hb_sampling = "gaussian",            # Gaussian likelihood
  hb_link     = "identity",            # Identity link
  data        = data_fhnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)

# Check the model summary
summary(model)
```

## The Three-Layer API

```
                      hbm()
                      Universal: any brms family, full customisation
                                       ▲
                                       |
                      hbm_flex()
                      Family registry + auxiliary + fixed_params
                                       ▲
                                       |
       hbm_lnln() / hbm_betalogitnorm() / hbm_binlogitnorm()
       SAE-friendly wrappers: response, auxiliary, group, n/deff, ...
```

Most users start with a wrapper. Step up to `hbm_flex()` when you need a custom family or the generic `fixed_params` interface; step up to `hbm()` when you need full brms control.

## Core Functions

### Main Modeling Functions

#### `hbm()` — Universal Modeling Function

The fully customisable entry point for any brms family:

```r
model <- hbm(
  formula     = bf(y ~ x1 + x2 + x3),  # brms formula
  hb_sampling = "gaussian",             # likelihood family
  hb_link     = "identity",             # link function
  re          = ~(1 | area),            # random effects
  sre         = "region",               # spatial RE variable
  sre_type    = "car",                  # "car" or "sar"
  M           = adjacency_matrix,       # spatial weight matrix
  data        = data_fhnorm
)
```

#### Distribution-Specific Wrappers

**Beta logit-normal** for proportions in (0, 1):

```r
fit_beta <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  n         = "n",                # sample size column
  deff      = "deff",             # design effect column
  group     = "group",            # area random effect
  data      = data_betalogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

**Binomial logit-normal** for successes out of trials:

```r
fit_bin <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  group     = "group",
  data      = data_binlogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

**Lognormal-lognormal** (Fay-Herriot variant) for positive, right-skewed responses:

```r
fit_lnln <- hbm_lnln(
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  group        = "group",
  sampling_var = "psi_i",         # known sampling variance (LOG SCALE)
  data         = data_lnln,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

### Diagnostic Functions

#### `convergence_check()` — MCMC Convergence Diagnostics

Assess MCMC convergence (replaces the legacy `hbcc()`):

```r
diag <- convergence_check(
  model,
  diag_tests = c("rhat", "geweke", "heidel", "raftery"),
  plot_types = c("trace", "dens", "acf", "nuts_energy", "rhat", "neff")
)
print(diag)
```

#### `model_compare()` — Model Comparison and Checking

Evaluate model fit and compare models (replaces the legacy `hbmc()`):

```r
cmp <- model_compare(
  model,
  comparison_metrics    = c("loo", "waic", "bf"),
  run_prior_sensitivity = TRUE,
  sensitivity_vars      = c("b_x1")
)
print(cmp)
```

#### `prior_check()` — Prior Predictive Checks

Validate prior assumptions before fitting (replaces the legacy `hbpc()`):

```r
pcheck <- prior_check(
  model,             # fitted with sample_prior = "only"
  data         = data,
  response_var = "y",
  ndraws_ppc   = 50
)
print(pcheck)
```

### Prediction Function

#### `sae_predict()` — Small Area Estimation

Generate predictions with proper uncertainty quantification, including for unsampled areas (replaces the legacy `hbsae()`):

```r
preds <- sae_predict(
  model,
  newdata          = new_areas,
  allow_new_levels = TRUE
)
print(preds)
```

### Interactive Application

#### `run_sae_app()` — Shiny Dashboard

Launch an interactive bilingual (English/Indonesian) Shiny app for model building, diagnostics, and visualisation:

```r
run_sae_app()
```

## Advanced Features

### Missing Data Handling

Three strategies, auto-selected when `handle_missing = NULL`:

```r
data_with_missing <- data_fhnorm
data_with_missing$y[3:5] <- NA

# 1. Complete-case analysis (listwise deletion)
model_deleted <- hbm(
  formula        = bf(y ~ x1 + x2 + x3),
  data           = data_with_missing,
  handle_missing = "deleted"
)

# 2. Multiple imputation via mice
model_mi <- hbm(
  formula        = bf(y ~ x1 + x2 + x3),
  data           = data_with_missing,
  handle_missing = "multiple",
  m              = 5            # number of imputations
)

# 3. Joint Bayesian imputation via brms::mi()
model_joint <- hbm(
  formula        = bf(y | mi() ~ x1 + x2 + x3),
  data           = data_with_missing,
  handle_missing = "model"
)
```

### Spatial Modeling

CAR, SAR, and BYM2 spatial random effects:

```r
# CAR (Conditional Autoregressive) - default ICAR
data("adjacency_matrix_car")
model_car <- hbm(
  formula  = bf(y ~ x1 + x2 + x3),
  data     = data_fhnorm,
  sre      = "spatial_area",
  sre_type = "car",
  car_type = "icar",
  M        = adjacency_matrix_car
)

# BYM2 (modern Riebler et al. 2016 parameterisation)
model_bym2 <- hbm(
  formula  = bf(y ~ x1 + x2 + x3),
  data     = data_fhnorm,
  re       = ~(1 | spatial_area),
  sre      = "spatial_area",
  sre_type = "car",
  car_type = "bym2",
  M        = adjacency_matrix_car
)

# SAR (Spatial Autoregressive)
data("spatial_weight_sar")
model_sar <- hbm(
  formula  = bf(y ~ x1 + x2 + x3),
  data     = data_fhnorm,
  sre_type = "sar",
  sar_type = "lag",
  M        = spatial_weight_sar
)
```

### Custom brms Families

Two custom families are shipped (Loglogistic and Shifted Loglogistic) with full support for `loo()`, `posterior_predict()`, and `posterior_epred()`:

```r
fit_ll <- hbm_flex(
  family_key = "loglogistic",
  response   = "y",
  auxiliary  = c("x1", "x2"),
  data       = my_data
)
```

Register your own custom family via `register_hbsae_brms_custom()`.

## Complete Workflow Example

```r
library(hbsaems)

# 1. Load and explore data
data("data_fhnorm")
head(data_fhnorm)

# 2. Prior predictive check
model_prior <- hbm(
  formula      = bf(y ~ x1 + x2 + x3),
  data         = data_fhnorm,
  sample_prior = "only",
  prior        = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept")
  )
)
prior_check(model_prior, data = data_fhnorm, response_var = "y")

# 3. Fit the main model
model <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data    = data_fhnorm,
  re      = ~(1 | group),
  prior   = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept")
  ),
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)

# 4. Check convergence
convergence_check(model)

# 5. Model checking and comparison
model_compare(model)

# 6. Generate predictions
sae_predict(model)
```

## Available Datasets

- `data_fhnorm` — Fay-Herriot Normal model data
- `data_binlogitnorm` — Binomial Logit-Normal data
- `data_betalogitnorm` — Beta Logit-Normal data
- `data_lnln` — Lognormal-Lognormal data
- `adjacency_matrix_car` — Adjacency matrix for CAR models
- `spatial_weight_sar` — Spatial weight matrix for SAR models

## Vignettes

The package ships with **10 comprehensive vignettes**:

| Vignette | Topic |
|----------|-------|
| `hbsaems-modelling` | High-level workflow overview (start here) |
| `hbsaems-lnln-model` | Lognormal-lognormal; Fay-Herriot variant via `sampling_var` |
| `hbsaems-betalogitnorm-model` | Beta logit-normal; four modes for $\phi$ |
| `hbsaems-binlogitnorm-model` | Binomial logit-normal |
| `hbsaems-spatial` | CAR / SAR / BYM2 spatial random effects |
| `hbsaems-handle-missing` | Missing data: deletion / multiple / model |
| `hbsaems-run_sae_app` | Interactive Shiny dashboard |
| `complete-workflow` | Complete workflow walkthrough |
| `advanced-features` | Advanced features and customisation |
| `migration-guide` | Migration guide for legacy v0.1.x users |

Browse them with:

```r
browseVignettes("hbsaems")
```

### Comprehensive Examples Reference

In addition to the topical vignettes above, the package ships an
**all-in-one examples reference** at `inst/examples/hbsaems-examples.Rmd`
covering every supported model family, every advanced feature, and
every utility in one document. Locate it with:

```r
system.file("examples", "hbsaems-examples.Rmd", package = "hbsaems")
```

You can render it locally with:

```r
rmd_file <- system.file("examples", "hbsaems-examples.Rmd",
                         package = "hbsaems")
rmarkdown::render(rmd_file, output_dir = tempdir())
```

This is ideal as a quick reference card or as a JSS-style supplementary
materials companion.

## Methodology

The package implements area-level hierarchical Bayesian SAE following the methodological tradition of Rao and Molina (2015), adapted to the brms parameterisation:

- **Borrowing strength** across related small areas
- **Proper uncertainty quantification** for area-level estimates
- **Flexible modelling** for various auxiliary information structures
- **Robust estimation** for areas with small or zero sample sizes
- **Principled Bayesian workflow** (Gelman et al., 2020) integrated throughout

## Deprecated Names (removal scheduled for v2.0.0)

The original v0.1.x short-form names continue to work but emit a soft-deprecation warning:

| Deprecated | Use instead |
|------------|-------------|
| `hbcc()` | `convergence_check()` |
| `hbmc()` | `model_compare()` |
| `hbpc()` | `prior_check()` |
| `hbsae()` | `sae_predict()` |
| `predictors =` argument | `auxiliary =` argument |

## Authors and Contributors

This package was developed at **Politeknik Statistika STIS, Jakarta** by:

- **Achmad Syahrul Choir** ([ORCID 0000-0001-7088-0646](https://orcid.org/0000-0001-7088-0646)) — author, maintainer
- **Saniyyah Sri Nurhayati** — author
- **Sofi Zamzanah** — author
- **Arsyka Laila Oktalia Siregar** — author

## License

GPL (>= 3). See [LICENSE.md](LICENSE.md).

## Citation

If you use this package in your research, please cite:

```r
citation("hbsaems")
```

```
Choir, A. S., Nurhayati, S. S., Zamzanah, S., & Siregar, A. L. O. (2026).
hbsaems: Hierarchical Bayesian Area-Level Small Area Estimation Models.
R package version 1.0.0. https://github.com/madsyair/hbsaems
```

## References

**Primary SAE references**

- Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation* (2nd ed.). Wiley. doi: [10.1002/9781118735855](https://doi.org/10.1002/9781118735855).
- Fay, R. E., & Herriot, R. A. (1979). Estimates of income for small places: An application of James-Stein procedures to census data. *JASA*, 74(366), 269–277.
- Pfeffermann, D. (2013). New important developments in small area estimation. *Statistical Science*, 28(1), 40–68.
- Liu, B. (2009). *Hierarchical Bayes Estimation and Empirical Best Prediction of Small-Area Proportions*. PhD thesis, University of Maryland.
- Slud, E. V., & Maiti, T. (2006). Mean-squared error estimation in transformed Fay-Herriot models. *JRSSB*, 68(2), 239–257.
- You, Y., & Chapman, B. (2006). Small area estimation using area level models and estimated sampling variances. *Survey Methodology*, 32(1), 97–103.
- Fabrizi, E., Ferrante, M. R., & Trivisano, C. (2018). Bayesian Small Area Estimation for Skewed Business Survey Variables. *Journal of the Royal Statistical Society Series C: Applied Statistics*, 67(4), 861–879.

**Bayesian engine and workflow**

- Bürkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. *Journal of Statistical Software*, 80(1), 1–28. doi: [10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01).
- Gelman, A., Vehtari, A., Simpson, D., et al. (2020). Bayesian workflow. *arXiv preprint* doi: [10.48550/arXiv.2011.01808](https://doi.org/10.48550/arXiv.2011.01808).
- Gelman, A., & Hill, J. (2006). *Data Analysis Using Regression and Multilevel/Hierarchical Models*. Cambridge University Press.

**Spatial random effects**

- Besag, J., York, J., & Mollié, A. (1991). Bayesian image restoration. *Annals of the Institute of Statistical Mathematics*, 43, 1–20.
- Riebler, A., Sørbye, S. H., Simpson, D., & Rue, H. (2016). An intuitive Bayesian spatial model for disease mapping that accounts for scaling (BYM2). *Statistical Methods in Medical Research*, 25(4), 1145–1165.
- Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer.

## Getting Help

For questions and support:

- Check the [GitHub Issues](https://github.com/madsyair/hbsaems/issues)
- Review the package documentation and vignettes
- Use the interactive Shiny app (`run_sae_app()`) for guided model building
- Reference website (when published): <https://madsyair.github.io/hbsaems/>
