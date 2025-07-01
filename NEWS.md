# hbsaems 0.1.0

* Initial GitHub submission.

* Added the main function `hbm()` for general hierarchical Bayesian modeling in the context of Small Area Estimation (SAE).

* Added model-specific functions:
  - `hbm_beta()` for Beta distribution modeling.
  - `hbm_logitnormal()` for Logit-Normal distribution modeling.
  - `hbm_lognormal()` for Log-Normal distribution modeling.

* Added model diagnostic functions:
  - `hbcc()` for convergence checking (e.g., using trace plots, Rhat, and effective sample size).
  - `hbmc()` for evaluating model goodness-of-fit.

* Added `hbsae()` function for producing area-level predictions and estimates based on fitted models.

* Added `run_sae_app()` to launch an interactive Shiny application for upload data, model specification, fitting, checking, and result exploration.

# hbsaems 0.1.1

* Updated `hbmc()`:
  - Added handling for problematic `k` Pareto values in LOO diagnostics.
  - Included prior sensitivity analysis as part of the model checking process.

* Improved `hbsae()`:
  - Replaced `posterior_predict()` with `posterior_epred()` from the **brms** package for better compatibility and interpretation.
  - Added computation of Mean Squared Error (MSE) for predicted estimates.
  - Removed the scaling option for prediction; SAE predictions are now standardized using `posterior_epred()` consistently.

* Updated model-specific functions (`hbm_beta()`, `hbm_logitnormal()`, and `hbm_lognormal()`):
  - Weakly informative priors are now used by default for all regression coefficients (betas), enhancing model stability while preserving flexibility.

* Shiny App Enhancements via `run_sae_app()`:
  - Added **Update Model** menu for re-fitting existing models with new data or configurations.
  - Added **Explore Data** tab for visual and tabular examination of uploaded datasets.
  - Integrated **Prior Sensitivity Analysis** tab for evaluating the impact of prior assumptions on model results.
  - Improved posterior predictive plot and download features.



