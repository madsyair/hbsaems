## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(hbsaems)

## -----------------------------------------------------------------------------
# library(hbsaems)
# data("data_binlogitnorm")
# data <- data_binlogitnorm
# head(data)

## -----------------------------------------------------------------------------
# model_prior_pred <- hbm_binlogitnorm(
#    response = "y",
#    trials = "n",
#    predictors = c("x1", "x2", "x3"),
#    data = data,
#    sample_prior = "only",
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_prior_pred)

## -----------------------------------------------------------------------------
# result_hbpc <- hbpc(model_prior_pred, response_var = "y")
# summary(result_hbpc)

## -----------------------------------------------------------------------------
# result_hbpc$prior_predictive_plot

## -----------------------------------------------------------------------------
# model <- hbm_binlogitnorm(
#    response = "y",
#    trials = "n",
#    predictors = c("x1", "x2", "x3"),
#    data = data,
#    sample_prior = "no",
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model)

## -----------------------------------------------------------------------------
# model_with_defined_re <- hbm_binlogitnorm(
#    response = "y",
#    trials = "n",
#    predictors = c("x1", "x2", "x3"),
#    group = "group",
#    data = data,
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_with_defined_re)

## -----------------------------------------------------------------------------
# data_missing <- data
# data_missing$y[3:5] <- NA

## -----------------------------------------------------------------------------
# model_deleted <- hbm_binlogitnorm(
#    response = "y",
#    trials = "n",
#    predictors = c("x1", "x2", "x3"),
#    data = data_missing,
#    handle_missing = "deleted",
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_deleted)

## -----------------------------------------------------------------------------
# data_missing <- data
# data_missing$y[3:5] <- NA
# data_missing$x1[6:7] <- NA

## -----------------------------------------------------------------------------
# model_multiple <- hbm_binlogitnorm(
#    response = "y",
#    trials = "n",
#    predictors = c("x1", "x2", "x3"),
#    data = data_missing,
#    handle_missing = "multiple",
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_multiple)

## -----------------------------------------------------------------------------
# M <- adjacency_matrix_car

## -----------------------------------------------------------------------------
# model_spatial <- hbm_binlogitnorm(
#    response = "y",
#    trials = "n",
#    predictors = c("x1", "x2", "x3"),
#    sre = "sre",                # Spatial random effect variable
#    sre_type = "car",
#    car_type = "icar",
#    M = M,
#    data = data,
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_spatial)

## -----------------------------------------------------------------------------
# result_hbcc <- hbcc(model)
# summary(result_hbcc)

## -----------------------------------------------------------------------------
# model <- update_hbm(model, iter = 8000)

## -----------------------------------------------------------------------------
# summary(model)

## -----------------------------------------------------------------------------
# result_hbcc <- hbcc(model)
# summary(result_hbcc)

## -----------------------------------------------------------------------------
# result_hbcc$plots$trace

## -----------------------------------------------------------------------------
# result_hbcc$plots$dens

## -----------------------------------------------------------------------------
# result_hbcc$plots$acf

## -----------------------------------------------------------------------------
# result_hbcc$plots$nuts_energy

## -----------------------------------------------------------------------------
# result_hbcc$plots$rhat

## -----------------------------------------------------------------------------
# result_hbcc$plots$neff

## -----------------------------------------------------------------------------
# result_hbmc <- hbmc(
#       model = list(model, model_spatial),
#       comparison_metrics = c("loo", "waic", "bf"),
#       run_prior_sensitivity= TRUE,
#       sensitivity_vars = c ("b_x1")
#   )
# 
# summary(result_hbmc)

## -----------------------------------------------------------------------------
# result_hbmc$primary_model_diagnostics$pp_check_plot

## -----------------------------------------------------------------------------
# result_hbmc$primary_model_diagnostics$params_plot

## -----------------------------------------------------------------------------
# result_hbsae <- hbsae(model)
# summary(result_hbsae)

