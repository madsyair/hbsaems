# hbsaems: Hierarchical Bayesian Small Area Estimation Models

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

**hbsaems** is an R package that implements Hierarchical Bayesian Small
Area Estimation (HBSAE) models. It provides a comprehensive framework
for estimating parameters in situations with limited sample sizes by
borrowing strength from related areas through hierarchical modeling. The
package uses the **brms** package for Bayesian inference with Stan,
ensuring robust and efficient computational performance while supporting
modern Bayesian workflow principles.

## Key Features

-   **Multiple Distribution Families**: Support for Gaussian, Beta,
    Binomial/Logit-Normal, and Lognormal distributions, along with other distributions supported by brms.
-   **Spatial Modeling**: Conditional Autoregressive (CAR) and
    Simultaneous Autoregressive (SAR) spatial random effects\
-   **Missing Data Handling**: Three approaches - deletion, model-based
    imputation, and multiple imputation
-   **Comprehensive Diagnostics**: Built-in convergence assessment and
    goodness of fit evaluation
-   **Uncertainty Quantification**: Proper uncertainty quantification
    for predictions
-   **Interactive Interface**: Shiny app for interactive model building
    and visualization
-   **Bayesian Workflow**: Full support for prior specification, model
    checking, and validation

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("madsyair/hbsaems")
```

## Dependencies

The package requires: 

- **brms** (for Bayesian regression modeling) 
- **coda**, **posterior** (for MCMC diagnostics) 
- **ggplot2** (for plotting) 
- **mice** (for multiple imputation) 
- **shiny**, **shinydashboard**, **shinyWidgets**, **readxl**, **DT** (for the
interactive app), 
- **priorsense** (for prior sensitivity analysis)
- **energy**,  **XICOR***,  and **minerva,** (for computing correlation).

## Quick Start

Here's a simple example using the built-in data:

``` r
library(hbsaems)

# Load example data
data("data_fhnorm")
data<-data_fhnorm
# Fit a basic Gaussian Model
model <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  hb_sampling = "gaussian",     # Gaussian family for continuous outcomes
  hb_link = "identity",         # Identity link function
  data = data_fhnorm,           # Dataset
  chains = 2,                   # Number of MCMC chains
  iter = 4000,                  # Total MCMC iterations
  warmup = 2000,                # Number of warmup iterations
  cores = 2                     # Number of cores for parallel processing
)

# Check model summary
summary(model)
```

## Core Functions

### Main Modeling Functions

#### `hbm()` - Flexible Modeling Function

The primary function for fitting hierarchical Bayesian models:

``` r
model <- hbm(
  formula = bf(y ~ x1 + x2 + x3),    # Formula using brms::bf()
  hb_sampling = "gaussian",           # Distribution family
  hb_link = "identity",              # Link function
  re = ~(1|area),                    # Random effects formula
  sre = "region",                    # Spatial random effect variable
  sre_type = "car",                  # Type of spatial model: "car" or "sar"
  M = adjacency_matrix,              # Spatial matrix
  data = data_fhnorm                     # Dataset for model fitting
)
```

#### Distribution-Specific Functions

-   `hbm_betalogitnorm()`: For Beta distribution with logit-normal
    structure
-   `hbm_binlogitnorm()`: For binomial data with logit-normal model
-   `hbm_lnln()`: For lognormal-lognormal models

### Diagnostic Functions

#### `hbcc()` - Convergence Diagnostics

Assess model convergence with multiple diagnostic tests:

``` r
convergence <- hbcc(
  model,                                           # A brmsfit or hbmfit object
  diag_tests = c("rhat", "geweke", "heidel", "raftery"), # Diagnostic tests
  plot_types = c("trace", "dens", "acf", "nuts_energy", "rhat", "neff") # Plot types
)
print(convergence)
```

#### `hbmc()` - Model Comparison and Checking

Evaluate model fit and compare models:

``` r
fit <- hbmc(
  model,                    # A brmsfit or hbmfit object
  comparison_metrics = c("loo", "waic", "bf"), # Comparison metrics
  run_prior_sensitivity = TRUE,  # Prior sensitivity analysis
  sensitivity_vars = c("b_x1")   # Variables for sensitivity analysis
)
print(fit)
```

#### `hbpc()` - Prior Predictive Checking

Validate prior assumptions before model fitting:

``` r
prior_check <- hbpc(
  model,              # Model fitted with sample_prior = "only"
  data = data,        # Dataset
  response_var = "y", # Response variable name
  ndraws_ppc = 50     # Number of draws
)
print(prior_check)
```

### Prediction Function

#### `hbsae()` - Small Area Estimation

Generate predictions with uncertainty quantification:

``` r
predictions <- hbsae(
  model,              # A brmsfit or hbmfit object
  newdata = NULL      # Optional new data for predictions
)
print(predictions)
```

### Interactive Application

#### `run_sae_app()` - Shiny Application

Launch an interactive Shiny app for model building and visualization:

``` r
run_sae_app()
```

## Advanced Features

### Missing Data Handling

The package provides three approaches for handling missing data:

``` r
data_with_missing <- data_fhnorm
data_with_missing$y[3:5] <- NA 

# 1. Complete case analysis
model_deleted <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data_with_missing,
  handle_missing = "deleted"
)

# 2. Multiple imputation
model_multiple <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data_with_missing,
  handle_missing = "multiple",
  m = 5  # Number of imputations
)

# 3. Model-based imputation (for continuous variables only)
model_mi <- hbm(
  formula = bf(y | mi() ~ x1 + x2 + x3),
  data = data_with_missing,
  handle_missing = "model"
)
```

### Spatial Modeling

Support for spatial models with CAR and SAR structures:

``` r
# CAR (Conditional Autoregressive) model
data("adjacency_matrix_car")
model_car <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data_fhnorm,
  sre = "spatial_area",         # Spatial grouping variable
  sre_type = "car",            # CAR structure
  car_type = "icar",           # Intrinsic CAR
  M = adjacency_matrix_car     # Adjacency matrix
)

# SAR (Simultaneous Autoregressive) model
data("spatial_weight_sar")
model_sar <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data_fhnorm,
  sre_type = "sar",            # SAR structure
  sar_type = "lag",            # Lag model
  M = spatial_weight_sar       # Spatial weight matrix
)
```

## Complete Workflow Example

``` r
library(hbsaems)

# 1. Load and explore data
data("data_fhnorm")
head(data_fhnorm)

# 2. Prior predictive check
model_prior <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data_fhnorm,
  sample_prior = "only",
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept")
  )
)

# Check priors
prior_check <- hbpc(model_prior, data = data_fhnorm, response_var = "y")
print(prior_check$prior_predictive_plot)

# 3. Fit the main model
model <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data_fhnorm,
  re = ~(1|group),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 2), class = "Intercept")
  ),
  chains = 4,
  iter = 4000,
  cores = 2
)

# 4. Check convergence
convergence <- hbcc(model)
print(convergence$plots$trace)

# 5. Model checking
model_check <- hbmc(model)
print(model_check$primary_model_diagnostics$pp_check_plot)

# 6. Generate predictions
predictions <- hbsae(model)
summary(predictions)
```

## Available Datasets

The package includes several simulated datasets for demonstration:

-   `data_fhnorm`: Fay-Herriot Normal model data
-   `data_binlogitnorm`: Binomial Logit-Normal data
-   `data_betalogitnorm`: Beta Logit-Normal data
-   `data_lnln`: Lognormal-Lognormal data
-   `adjacency_matrix_car`: Adjacency matrix for CAR models
-   `spatial_weight_sar`: Spatial weight matrix for SAR models

## Methodology

The package implements hierarchical Bayesian approaches following the
theoretical framework described in Rao & Molina (2015). The models
provide:

-   **Borrowing Strength**: Leverage information across related small
    areas
-   **Uncertainty Quantification**: Proper credible intervals for
    estimates
-   **Flexible Modeling**: Accommodate various auxiliary information
    structures
-   **Robust Estimation**: Handle areas with small or zero sample sizes

## Authors and Contributors

This package was developed by: - Achmad Syahrul Choir - Saniyyah Sri
Nurhayati\
- Sofi Zamzanah - Arsyka Laila Oktalia Siregar

## License

GPL-3

## Citation

If you use this package in your research, please cite:

```         
Choir, A.S, Nurhayati, S.S, Zamzanah, S. & Siregar, A.L.O, (2025). 
hbsaems: Hierarchical Bayesian Small Area Estimation Models. 
R package version 0.1.0.
```

## References

-   Rao, J. N. K., & Molina, I. (2015). *Small area estimation*. John
    Wiley & Sons.
-   Liu, B. (2009). Hierarchical Bayes Estimation and Empirical Best
    Prediction of Small-Area Proportions. College Park, University of
    Maryland.
-   Fabrizi, E., Ferrante, M. R., & Trivisano, C. (2018). Bayesian Small
    Area Estimation for Skewed Business Survey Variables. Journal of the
    Royal Statistical Society Series C: Applied Statistics, 67(4),
    861–879.
-   Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel
    models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
-   Gelman, A., & Hill, J. (2006). *Data analysis using regression and
    multilevel/hierarchical models*. Cambridge University Press.
-   Gelman, A. (2006). Prior Distributions for Variance Parameters in
    Hierarchical Models (Comment on Article by Browne and Draper).
    Bayesian Analysis, 1(3), 527–528.
-   Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A Weakly
    Informative Default Prior Distribution for Logistic and Other
    Regression Models.

## Getting Help

For questions and support: - Check the [GitHub
Issues](https://github.com/madsyair/hbsaems/issues) - Review the package
documentation and vignettes - Use the interactive Shiny app
(`run_sae_app()`) for guided model building
