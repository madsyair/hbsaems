# Package index

## Model fitting (entry points)

Three layers of API for fitting a hierarchical Bayesian SAE model, from
the most user-friendly wrappers to the universal brms interface.

- [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) : hbm:
  Hierarchical Bayesian Small Area Models
- [`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
  : Fit a Flexible HBSAE Model with Any Registered Family
- [`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md)
  : Small Area Estimation under a Lognormal-Lognormal Model
- [`hbm_betalogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md)
  : Small Area Estimation Under a Beta Likelihood (Logit-Normal Link)
- [`hbm_binlogitnorm()`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md)
  : Small Area Estimation under a Binomial Logit-Normal Model
- [`update_hbm()`](https://madsyair.github.io/hbsaems/reference/update_hbm.md)
  : Update a Fitted HBM

## Diagnostics & comparison

Post-fit assessment of MCMC convergence, predictive performance, and
model selection.

- [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
  : MCMC Convergence Diagnostics for Fitted HBMs
- [`is_converged()`](https://madsyair.github.io/hbsaems/reference/is_converged.md)
  : Test Whether a Fitted HBM Has Converged
- [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
  : Compare One or Two Fitted HBMs
- [`model_compare_all()`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md)
  : Compare Multiple Fitted HBMs
- [`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
  : Bayesian Model Averaging on Small-Area Estimates
- [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md)
  : Prior Predictive Check for Fitted HBMs
- [`diagnostic_summary()`](https://madsyair.github.io/hbsaems/reference/diagnostic_summary.md)
  : Extract a Diagnostic Summary
- [`is.hbsaems_check()`](https://madsyair.github.io/hbsaems/reference/is.hbsaems_check.md)
  : Test Whether an Object Is an hbsaems Check Result
- [`summary(`*`<hbsaems_check>`*`)`](https://madsyair.github.io/hbsaems/reference/summary.hbsaems_check.md)
  : Generic Summary Method for hbsaems Check Results

## Prediction & benchmarking

Out-of-sample prediction for unsampled areas and design-consistent
benchmarking against direct estimates.

- [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  : Generate Small Area Estimates
- [`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md)
  : Benchmark Small-Area Estimates to Known Totals
- [`sae_aggregate()`](https://madsyair.github.io/hbsaems/reference/sae_aggregate.md)
  : Aggregate Predictions from Multiple hbsae_results
- [`sae_scale()`](https://madsyair.github.io/hbsaems/reference/sae_scale.md)
  : Standardise SAE Predictions
- [`sae_transform()`](https://madsyair.github.io/hbsaems/reference/sae_transform.md)
  : Apply a Transformation to SAE Predictions
- [`sae_filter()`](https://madsyair.github.io/hbsaems/reference/sae_filter.md)
  : Filter SAE Predictions by a Logical Condition

## Spatial weights

Construct, validate, and inspect neighbourhood weight matrices for CAR /
SAR / BYM2 spatial random effects.

- [`build_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
  : build_spatial_weight: Construct M for CAR / SAR models
- [`check_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md)
  : Validate a Spatial Weight Matrix Against CAR/SAR Theory

## Custom brms families

Built-in custom distributions and the framework for registering new
ones.

- [`brms_custom_loglogistic()`](https://madsyair.github.io/hbsaems/reference/brms_custom_loglogistic.md)
  : Loglogistic as a Custom Distribution Family for brms
- [`brms_custom_shifted_loglogistic()`](https://madsyair.github.io/hbsaems/reference/brms_custom_shifted_loglogistic.md)
  : Shifted Loglogistic as a Custom Distribution Family for brms
- [`dloglogistic()`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  [`ploglogistic()`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  [`qloglogistic()`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  [`rloglogistic()`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  : Loglogistic Distribution Functions
- [`dshifted_loglogistic()`](https://madsyair.github.io/hbsaems/reference/shifted_loglogistic.md)
  [`pshifted_loglogistic()`](https://madsyair.github.io/hbsaems/reference/shifted_loglogistic.md)
  [`qshifted_loglogistic()`](https://madsyair.github.io/hbsaems/reference/shifted_loglogistic.md)
  [`rshifted_loglogistic()`](https://madsyair.github.io/hbsaems/reference/shifted_loglogistic.md)
  : Shifted (3-Parameter) Loglogistic Distribution
- [`build_brms_custom_family()`](https://madsyair.github.io/hbsaems/reference/build_brms_custom_family.md)
  : Build a brms Custom Family + Stanvars Pair from a Single Spec
- [`read_stan_function()`](https://madsyair.github.io/hbsaems/reference/read_stan_function.md)
  : Read the Stan Function Code for a Custom Distribution
- [`register_hbsae_brms_custom()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md)
  : Register a brms Custom Family with the hbsaems Model Registry
- [`register_hbsae_model()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md)
  : Register a Custom HBSAE Model
- [`list_hbsae_models()`](https://madsyair.github.io/hbsaems/reference/list_hbsae_models.md)
  : List Registered HBSAE Models
- [`get_hbsae_model()`](https://madsyair.github.io/hbsaems/reference/get_hbsae_model.md)
  : Inspect a Registered HBSAE Model Specification

## Configuration bundles

Helpers for assembling sampler / prior / nonlinear-term arguments.

- [`hbm_control()`](https://madsyair.github.io/hbsaems/reference/hbm_control.md)
  : Sampler Configuration for HBSAE Models
- [`hbm_priors()`](https://madsyair.github.io/hbsaems/reference/hbm_priors.md)
  : Prior Configuration for HBSAE Models
- [`hbm_nonlinear()`](https://madsyair.github.io/hbsaems/reference/hbm_nonlinear.md)
  : Nonlinear-Term Configuration for HBSAE Models

## Object methods & Internal state

S3 methods for the `hbmfit` class and internal object inspection.

- [`hbmfit()`](https://madsyair.github.io/hbsaems/reference/hbmfit.md) :
  User-Facing Helper to Build an hbmfit Object
- [`new_hbmfit()`](https://madsyair.github.io/hbsaems/reference/new_hbmfit.md)
  : Create a New hbmfit Object
- [`validate_hbmfit()`](https://madsyair.github.io/hbsaems/reference/validate_hbmfit.md)
  : Validate an hbmfit Object
- [`hbm_data()`](https://madsyair.github.io/hbsaems/reference/hbm_data.md)
  : Return the Data Used to Fit an hbmfit
- [`hbmfit-methods`](https://madsyair.github.io/hbsaems/reference/hbmfit-methods.md)
  : Standard S3 Methods for hbmfit
- [`hbmfit-class`](https://madsyair.github.io/hbsaems/reference/hbmfit-class.md)
  : The hbmfit S3 Class
- [`hbm-info`](https://madsyair.github.io/hbsaems/reference/hbm-info.md)
  : Model Inspection Helpers
- [`hbm_info()`](https://madsyair.github.io/hbsaems/reference/hbm_info.md)
  : Get Comprehensive Model Information
- [`hbm_warnings()`](https://madsyair.github.io/hbsaems/reference/hbm_warnings.md)
  : Get Model Warnings
- [`is.hbmfit()`](https://madsyair.github.io/hbsaems/reference/is-hbsaems.md)
  [`is.hbcc_results()`](https://madsyair.github.io/hbsaems/reference/is-hbsaems.md)
  [`is.hbmc_results()`](https://madsyair.github.io/hbsaems/reference/is-hbsaems.md)
  [`is.hbpc_results()`](https://madsyair.github.io/hbsaems/reference/is-hbsaems.md)
  [`is.hbsae_results()`](https://madsyair.github.io/hbsaems/reference/is-hbsaems.md)
  : Test Whether an Object Belongs to an hbsaems Result Class
- [`posterior-methods`](https://madsyair.github.io/hbsaems/reference/posterior-methods.md)
  : Posterior and Prior Extraction Methods for hbmfit
- [`plot(`*`<hbmfit>`*`)`](https://madsyair.github.io/hbsaems/reference/plot.hbmfit.md)
  : Plot a Fitted hbmfit Object

## Posterior & Prior Extraction

Tools for extracting draws and summaries from fitted models.

- [`posterior_draws()`](https://madsyair.github.io/hbsaems/reference/posterior_draws.md)
  : Extract Posterior Draws as a Matrix
- [`posterior_interval(`*`<hbmfit>`*`)`](https://madsyair.github.io/hbsaems/reference/posterior_interval.md)
  : Compute Credible Intervals for an hbmfit Object
- [`posterior_summary_hbm()`](https://madsyair.github.io/hbsaems/reference/posterior_summary_hbm.md)
  : Comprehensive Posterior Summary
- [`prior_draws(`*`<hbmfit>`*`)`](https://madsyair.github.io/hbsaems/reference/prior_draws.md)
  : Extract Prior Draws

## Data validation

Helpers for checking input data consistency.

- [`check_data()`](https://madsyair.github.io/hbsaems/reference/check_data.md)
  : Inspect Data Before Fitting an HBSAE Model

## Shiny dashboard

Launch the interactive bilingual SAE GUI.

- [`run_sae_app()`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)
  : run_sae_app: Interactive Small Area Estimation Dashboard
- [`check_shiny_deps()`](https://madsyair.github.io/hbsaems/reference/check_shiny_deps.md)
  : Check Shiny App Dependencies
- [`tr()`](https://madsyair.github.io/hbsaems/reference/tr.md) :
  Translate a UI String for the Shiny SAE App
- [`tr_langs()`](https://madsyair.github.io/hbsaems/reference/tr_langs.md)
  : List Available Languages
- [`tr_keys()`](https://madsyair.github.io/hbsaems/reference/tr_keys.md)
  : List All Translation Keys (for a Reference Language)

## Datasets

Example datasets shipped for vignettes and tests.

- [`data_fhnorm`](https://madsyair.github.io/hbsaems/reference/data_fhnorm.md)
  : Simulated Fay-Herriot Normal Data
- [`data_lnln`](https://madsyair.github.io/hbsaems/reference/data_lnln.md)
  : Simulated Lognormal-Lognormal Data
- [`data_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/data_betalogitnorm.md)
  : Simulated Beta Logit-Normal Data
- [`data_binlogitnorm`](https://madsyair.github.io/hbsaems/reference/data_binlogitnorm.md)
  : Simulated Binomial Logit-Normal Data
- [`adjacency_matrix_car`](https://madsyair.github.io/hbsaems/reference/adjacency_matrix_car.md)
  : Adjacency Matrix for Conditional Autoregressive Models
- [`spatial_weight_sar`](https://madsyair.github.io/hbsaems/reference/spatial_weight_sar.md)
  : Spatial Weight Matrix for Simultaneous Autoregressive Models

## Deprecated

Retained for backward compatibility; scheduled for removal in v2.0.0.

- [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
  : Deprecated Functions
- [`model-compare`](https://madsyair.github.io/hbsaems/reference/model-compare.md)
  : Compare Fitted HBMs
