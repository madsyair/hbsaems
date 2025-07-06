## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(hbsaems)
# data("data_betalogitnorm")
# data <- data_betalogitnorm
# 
# data("adjacency_matrix_car")
# M <- adjacency_matrix_car

## -----------------------------------------------------------------------------
# model_prior_pred <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    data = data,
#    sample_prior = "only",
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b"),
#     set_prior("gamma(2, 0.1)", class = "phi")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_prior_pred)

## -----------------------------------------------------------------------------
# model_prior_pred_phi <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    data = data,
#    sample_prior = "only",
#    prior = c(
#     set_prior("normal(-1, 0.7)", class = "Intercept"),
#     set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_prior_pred_phi)

## -----------------------------------------------------------------------------
# result_hbpc <- hbpc(model_prior_pred, response_var="y")
# summary(result_hbpc)

## -----------------------------------------------------------------------------
# result_hbpc$prior_predictive_plot

## -----------------------------------------------------------------------------
# model <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    data = data,
#    sample_prior = "no",
#    prior = c(
#      set_prior("normal(-1, 0.7)", class = "Intercept"),
#      set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model)

## -----------------------------------------------------------------------------
# model_with_defined_re <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    group = "group",
#    data = data,
#    sample_prior = "no",
#    prior = c(
#      set_prior("normal(-1, 0.7)", class = "Intercept"),
#      set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_with_defined_re)

## -----------------------------------------------------------------------------
# # Prepare Missing Data
# data_missing <- data
# data_missing$y[sample(1:30, 3)] <- NA  # 3 missing values in response

## -----------------------------------------------------------------------------
# model_deleted <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    group = "group",
#    data = data_missing,
#    handle_missing = "deleted",
#    sample_prior = "no",
#    prior = c(
#      set_prior("normal(-1, 0.7)", class = "Intercept"),
#      set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_deleted)

## -----------------------------------------------------------------------------
# # Prepare missing data
# data_missing <- data
# data_missing$y[3:5] <- NA
# data_missing$x1[6:7] <- NA

## -----------------------------------------------------------------------------
# model_multiple <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    group = "group",
#    data = data_missing,
#    handle_missing = "multiple",
#    sample_prior = "no",
#    prior = c(
#      set_prior("normal(-1, 0.7)", class = "Intercept"),
#      set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_multiple)

## -----------------------------------------------------------------------------
# data_missing <- data
# data_missing$x1[3:5] <- NA
# data_missing$x2[14:17] <- NA

## -----------------------------------------------------------------------------
# model_during_model <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    group = "group",
#    data = data_missing,
#    handle_missing = "model",
#    sample_prior = "no",
#    prior = c(
#      set_prior("normal(-1, 0.7)", class = "Intercept"),
#      set_prior("normal(0, 0.5)", class = "b")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_during_model)

## -----------------------------------------------------------------------------
# model_spatial <- hbm_betalogitnorm(
#    response = "y",
#    predictors = c("x1", "x2", "x3"),
#    n = "n",
#    deff = "deff",
#    group = "group",
#    sre = "sre",
#    sre_type = "car",
#    car_type = "icar",
#    M = M,
#    data = data,
#    sample_prior = "no",
#    prior = c(
#      set_prior("normal(-1, 0.7)", class = "Intercept"),
#      set_prior("normal(0, 0.5)", class = "b")
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

