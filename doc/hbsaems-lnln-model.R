## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(hbsaems)

## -----------------------------------------------------------------------------
# data <- data_lnln
# head(data)

## -----------------------------------------------------------------------------
# summary(data)

## -----------------------------------------------------------------------------
# model.check_prior <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   group = "group",
#   data = data,
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "only",
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model.check_prior)

## -----------------------------------------------------------------------------
# result.hbpc <- hbpc(model.check_prior, response_var="y_obs")
# summary(result.hbpc)

## -----------------------------------------------------------------------------
# result.hbpc$prior_predictive_plot

## -----------------------------------------------------------------------------
# model <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   data = data,
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "no", # the default is "no", you can skip it
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model)

## -----------------------------------------------------------------------------
# model.re <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   group = "group",
#   data = data,
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "no", # the default is "no", you can skip it
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model.re)

## -----------------------------------------------------------------------------
# # Prepare Missing Data
# data.missing1 <- data
# data.missing1$y_obs[sample(1:30, 3)] <- NA  # 3 missing values in response

## -----------------------------------------------------------------------------
# model.deleted <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   group = "group",
#   data = data.missing1,
#   handle_missing = "deleted",
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "no", # the default is "no", you can skip it
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model.deleted)

## -----------------------------------------------------------------------------
# model.multiple <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   group = "group",
#   data = data.missing1,
#   handle_missing = "multiple",
#   m = 5,
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "no", # the default is "no", you can skip it
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model.multiple)

## -----------------------------------------------------------------------------
# data.missing2 <- data
# data.missing1$y_obs[sample(1:30, 3)] <- NA  # 3 missing values in response
# data.missing2$x1[3:5] <- NA # missing values in predictor
# data.missing2$x2[14:17] <- NA

## -----------------------------------------------------------------------------
# model.during_model <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   group = "group",
#   data = data.missing2,
#   handle_missing = "model",
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "no", # the default is "no", you can skip it
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model.during_model)

## -----------------------------------------------------------------------------
# M <- adjacency_matrix_car
# M

## -----------------------------------------------------------------------------
# model.spatial <- hbm_lnln(
#   response = "y_obs",
#   predictors = c( "x1", "x2", "x3"),
#   group = "group",
#   data = data,
#   sre = "sre",                # Spatial random effect variable
#   sre_type = "car",
#   car_type = "icar",
#   M = M,
#   prior = c(
#     prior("normal(0.1,0.1)", class = "b"),
#     prior("normal(1,1)", class = "Intercept")
#   ),
#   sample_prior = "no", # the default is "no", you can skip it
#   iter = 4000,
#   warmup = 2000,
#   chains = 2,
#   seed = 123
# )

## -----------------------------------------------------------------------------
# summary(model.spatial)

## -----------------------------------------------------------------------------
# result.hbcc <- hbcc(model)
# summary(result.hbcc)

## -----------------------------------------------------------------------------
# result.hbcc$plots$trace

## -----------------------------------------------------------------------------
# result.hbcc$plots$dens

## -----------------------------------------------------------------------------
# result.hbcc$plots$acf

## -----------------------------------------------------------------------------
# result.hbcc$plots$nuts_energy

## -----------------------------------------------------------------------------
# result.hbcc$plots$rhat

## -----------------------------------------------------------------------------
# result.hbcc$plots$neff

## -----------------------------------------------------------------------------
# model.update <- update_hbm(
#   model = model,
#   iter = 12000,
#   warmup = 6000,
#   chains = 2,
#   control = list(adapt_delta = 0.95)
# )

## -----------------------------------------------------------------------------
# summary(model.update)

## -----------------------------------------------------------------------------
# result.hbmc <- hbmc(
#       model = list(model, model.spatial),
#       comparison_metrics = c("loo", "waic", "bf"),
#       run_prior_sensitivity= TRUE,
#       sensitivity_vars = c("b_Intercept", "b_x")
# )
# 
# summary(result.hbmc)

## -----------------------------------------------------------------------------
# result.hbmc$primary_model_diagnostics$pp_check_plot

## -----------------------------------------------------------------------------
# result.hbmc$primary_model_diagnostics$params_plot

## -----------------------------------------------------------------------------
# result.hbsae <- hbsae(model)
# summary(result.hbsae)

