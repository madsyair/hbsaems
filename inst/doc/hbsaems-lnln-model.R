## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----preload, eval = TRUE, include = FALSE------------------------------------
suppressPackageStartupMessages(library(hbsaems))

## ----show-data, eval = TRUE---------------------------------------------------
data("data_lnln")
str(data_lnln[, c("district", "y_obs", "psi_i", "x1", "x2", "x3")])

## ----fit-basic----------------------------------------------------------------
#  library(hbsaems)
#  fit <- hbm_lnln(
#    response  = "y_obs",
#    auxiliary = c("x1", "x2", "x3"),
#    area_var   = "district",
#    data      = data_lnln,
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )
#  summary(fit)

## ----fit-fh-------------------------------------------------------------------
#  fit_fh <- hbm_lnln(
#    response     = "y_obs",
#    auxiliary    = c("x1", "x2", "x3"),
#    area_var   = "district",
#    sampling_variance = "psi_i",     # <- pins sigma_i = sqrt(psi_i)
#    data         = data_lnln,
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )
#  summary(fit_fh)

## ----fit-custom-sigma---------------------------------------------------------
#  fit_custom <- hbm_lnln(
#    response  = "y_obs",
#    auxiliary = c("x1", "x2", "x3"),
#    area_var   = "district",
#    data      = data_lnln,
#    prior     = brms::set_prior("normal(0.3, 0.05)", class = "sigma"),
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )

## ----fit-flex-----------------------------------------------------------------
#  fit_flex <- hbm_flex(
#    family_key   = "lognormal",
#    response     = "y_obs",
#    auxiliary    = c("x1", "x2", "x3"),
#    area_var   = "district",
#    fixed_params = list(sigma = ~ sqrt(psi_i)),
#    data         = data_lnln,
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----fit-spline---------------------------------------------------------------
#  fit_spline <- hbm_lnln(
#    response          = "y_obs",
#    auxiliary         = c("x1", "x2", "x3"),
#    area_var          = "district",
#    sampling_variance = "psi_i",
#    data              = data_lnln,
#    nonlinear         = "x1",
#    nonlinear_type    = "spline",
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----conv-check---------------------------------------------------------------
#  convergence_check(fit_fh)

## ----predict------------------------------------------------------------------
#  new_areas <- data.frame(
#    group = 101:105,
#    x1    = rnorm(5),
#    x2    = rnorm(5),
#    x3    = rnorm(5)
#  )
#  preds <- sae_predict(fit_fh, newdata = new_areas, allow_new_levels = TRUE)
#  preds

## ----loo-compare--------------------------------------------------------------
#  loo_compare <- loo::loo_compare(
#    loo::loo(fit$brmsfit),
#    loo::loo(fit_fh$brmsfit)
#  )
#  loo_compare

