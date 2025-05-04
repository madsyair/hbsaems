# hbsaems: Hierarchical Bayesian Small Area Estimation Models

## Overview

`hbsaems` is an R package that implements Hierarchical Bayesian Small Area Estimation (HBSAE) models. It provides a comprehensive framework for estimating parameters in situations with limited sample sizes by borrowing strength from related areas through hierarchical modeling. The package uses the `brms` package for Bayesian inference with Stan.

### Key features:

- Fit hierarchical Bayesian models with various distribution families
- Support for spatial random effects using CAR and SAR models
- Handle missing data through multiple imputation or model-based approaches
- Diagnostic tools for model convergence and goodness of fit
- Prediction functionality with uncertainty quantification
- Interactive Shiny app for model building and visualization

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("madsyair/hbsaems")
```

## Dependencies

The package requires:

- `brms` (for Bayesian regression modeling)
- `coda`, `posterior` (for MCMC diagnostics)
- `ggplot2` (for plotting)
- `shiny`, `shinydashboard`, `shinyWidgets`, `readxl`, `DT` (for the interactive app)

## Basic Usage

Here's a simple example of fitting a basic Hierarchical Bayesian model:

```r
library(hbsaems)

# Prepare dataset
data("mtcars")

# Fit a basic Gaussian Model
model <- hbm(
  formula = bf(mpg ~ cyl + disp + hp),
  data = mtcars,
  hb_sampling = "gaussian",      # Gaussian family for continuous outcomes
  hb_link = "identity",          # Identity link function (no transformation)
  chains = 2,                    # Number of MCMC chains
  iter = 4000,                   # Total MCMC iterations
  warmup = 2000,                 # Number of warmup iterations
  cores = 2                      # Number of cores for parallel processing
)

# Check model summary
summary(model)

# Check model convergence
convergence <- hbcc(model)
print(convergence)

# Check model fit
fit <- hbmc(model)
print(fit)

# Make predictions
predictions <- hbsae(model)
print(predictions)
```

## Core Functions

### Modeling Functions

#### `hbm()`: General Hierarchical Bayesian Model

```r
model <- hbm(
  formula = bf(y ~ x1 + x2 + x3),    # Formula using brms::bf()
  hb_sampling = "gaussian",          # Distribution family
  hb_link = "identity",              # Link function
  re = ~(1|area),                    # Random effects formula
  sre = "region",                    # Spatial random effect variable
  sre_type = "car",                  # Type of spatial model: "car" or "sar"
  M = adjacency_matrix,              # Adjacency matrix for spatial effects
  data = my_data                     # Dataset for model fitting
)
```

#### Distribution-Specific Models

- `hbm_beta()`: For proportion data (0 < y < 1)
- `hbm_logitnormal()`: For binary/binomial data with logit transformation
- `hbm_lognormal()`: For positive continuous data with log transformation

### Diagnostic Functions

#### `hbcc()`: Hierarchical Bayesian Convergence Checks

```r
convergence <- hbcc(
  model,                                     # A brmsfit or hbmfit object
  diag_tests = c("rhat", "geweke", "heidel", "raftery"), # Diagnostic tests
  plot_types = c("trace", "dens", "acf")     # Types of diagnostic plots
)
```

#### `hbmc()`: Model Goodness of Fit Checks

```r
fit <- hbmc(
  model,           # A brmsfit or hbmfit object
  model2 = model2, # Optional second model for comparison
  ndraws = 100     # Number of draws for posterior predictive checks
)
```

### Prediction Function

#### `hbsae()`: Hierarchical Bayesian Small Area Estimation

```r
predictions <- hbsae(
  model,          # A brmsfit or hbmfit object
  newdata = NULL  # Optional new data for predictions
)
```

### Shiny App

#### `run_sae_app()`: Launch the Interactive Shiny App

```r
run_sae_app()
```

## Handling Missing Data

The package provides three approaches for handling missing data:

1. **"deleted"** - Remove observations with missing values
2. **"model"** - Use model-based imputation with `mi()` notation
3. **"multiple"** - Use multiple imputation

Example with model-based imputation:

```r
# Create data with missing values
data_missing <- mtcars
data_missing$mpg[c(5,10)] <- NA

# Fit model with missing data
model_missing <- hbm(
  formula = bf(mpg|mi() ~ cyl + disp + hp),
  data = data_missing,
  hb_sampling = "gaussian",
  hb_link = "identity",
  handle_missing = "model",
  chains = 2,
  iter = 4000,
  warmup = 2000,
  cores = 2
)
```

## Spatial Models

The package supports spatial models with Conditional Autoregressive (CAR) and Simultaneous Autoregressive (SAR) structures:

```r
# Create an adjacency matrix
library(Matrix)
data_spatial <- mtcars
data_spatial$spatial_gear <- factor(data_spatial$gear)
n <- length(levels(data_spatial$spatial_gear))
M <- bandSparse(n = n, k = c(-1, 0, 1), 
               diag = list(rep(1, n - 1), rep(0, n), rep(1, n - 1)))
rownames(M) <- colnames(M) <- levels(data_spatial$spatial_gear)

# Fit spatial model
model_spatial <- hbm(
  formula = bf(mpg ~ cyl + disp + hp),
  data = data_spatial,
  re = NULL,                    # No regular random effects
  sre = "spatial_gear",         # Include spatial random effect
  sre_type = "car",             # Use CAR structure
  M = M,                        # Provide adjacency matrix
  hb_sampling = "gaussian",
  hb_link = "identity",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2
)
```

## Credit

This package was developed by [Your Name] and contributors.

## License

License: GPL-3

## Citation

If you use this package, please cite:

```
Choir, A.S, Nurhayati, S.S, Zamzanah, S.& Siregar, A.L.O, (2025). hbsaems: Hierarchical Bayesian Small Area Estimation Models. R package version 0.1.0.
```

## References

- Gelman, A., & Hill, J. (2006). Data analysis using regression and multilevel/hierarchical models. Cambridge University Press.
- Rao, J. N. K., & Molina, I. (2015). Small area estimation. John Wiley & Sons.
- Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. Journal of Statistical Software, 80(1), 1-28.