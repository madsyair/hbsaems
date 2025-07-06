## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(hbsaems)

## -----------------------------------------------------------------------------
data <- data_fhnorm
head(data)

## -----------------------------------------------------------------------------
model_prior_pred <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data,
  hb_sampling = "gaussian",
  hb_link = "log",
  chains = 4,
  iter = 500,
  warmup = 250,
  sample_prior = "only",
  prior = c(
    prior("normal(1, 0.2)", class = "Intercept"),  
    prior("normal(0, 0.1)", class = "b"),
    prior("exponential(5)", class = "sd")
  )
)

## -----------------------------------------------------------------------------
summary(model_prior_pred)

## -----------------------------------------------------------------------------
result_hbpc <- hbpc(model_prior_pred)
summary(result_hbpc)

## -----------------------------------------------------------------------------
result_hbpc$prior_predictive_plot

## -----------------------------------------------------------------------------
model <- hbm(
  formula = bf(y ~ x1 + x2 + x3),
  data = data,
  hb_sampling = "gaussian",
  hb_link = "log",
  chains = 4,
  iter = 500,
  warmup = 250,
  sample_prior = "no",
  prior = c(
    prior("normal(1, 0.2)", class = "Intercept"),  
    prior("normal(0, 0.1)", class = "b"),
    prior("exponential(5)", class = "sd")
  )
)

## -----------------------------------------------------------------------------
summary(model)

## -----------------------------------------------------------------------------
result_hbcc <- hbcc(model)
summary(result_hbcc)

## -----------------------------------------------------------------------------
result_hbcc <- hbcc(model)
summary(result_hbcc)

## -----------------------------------------------------------------------------
result_hbcc$plots$trace

## -----------------------------------------------------------------------------
result_hbcc$plots$dens

## -----------------------------------------------------------------------------
result_hbcc$plots$acf

## -----------------------------------------------------------------------------
result_hbcc$plots$nuts_energy

## -----------------------------------------------------------------------------
result_hbcc$plots$rhat

## -----------------------------------------------------------------------------
result_hbcc$plots$neff

## -----------------------------------------------------------------------------
result_hbmc <- hbmc(
      model = list(model),
      comparison_metrics = c("loo", "waic", "bf"),
      run_prior_sensitivity= TRUE, 
      sensitivity_vars = c("b_Intercept", "b_x1")
  )
  
  
summary(result_hbmc)

## -----------------------------------------------------------------------------
result_hbmc$primary_model_diagnostics$pp_check_plot

## -----------------------------------------------------------------------------
result_hbmc$primary_model_diagnostics$params_plot

## -----------------------------------------------------------------------------
result_hbsae <- hbsae(model)
summary(result_hbsae)

