## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----preload, eval = TRUE, include = FALSE------------------------------------
suppressPackageStartupMessages(library(hbsaems))

## ----show-data, eval = TRUE---------------------------------------------------
data("data_binlogitnorm")
str(data_binlogitnorm[, c("district", "y", "n", "p", "x1", "x2", "x3")])

## ----fit-basic----------------------------------------------------------------
# library(hbsaems)
# library(brms)
# 
# fit <- hbm_binlogitnorm(
#   response  = "y",
#   trials    = "n",
#   auxiliary = c("x1", "x2", "x3"),
#   area_var   = "district",
#   data      = data_binlogitnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4,
#   seed = 1
# )
# summary(fit)

## ----fit-spatial--------------------------------------------------------------
# data("adjacency_matrix_car")
# fit_car <- hbm_binlogitnorm(
#   response  = "y",
#   trials    = "n",
#   auxiliary = c("x1", "x2", "x3"),
#   spatial_var = "regency",                       # spatial-RE column
#   spatial_model  = "car",                       # CAR structure
#   M         = adjacency_matrix_car,        # neighbour weight matrix
#   data      = data_binlogitnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4,
#   seed = 1
# )
# summary(fit_car)

## ----fit-prior----------------------------------------------------------------
# fit_prior <- hbm_binlogitnorm(
#   response  = "y",
#   trials    = "n",
#   auxiliary = c("x1", "x2", "x3"),
#   area_var   = "district",
#   data      = data_binlogitnorm,
#   prior     = brms::set_prior("normal(0, 0.1)", class = "b", coef = "x3"),
#   chains = 4, iter = 4000, warmup = 2000, cores = 4,
#   seed = 1
# )

## ----fit-missing--------------------------------------------------------------
# data_with_na <- data_binlogitnorm
# data_with_na$x1[c(3, 14, 27)] <- NA          # 3 areas with missing x1
# 
# fit_miss <- hbm_binlogitnorm(
#   response  = "y",
#   trials    = "n",
#   auxiliary = c("x1", "x2", "x3"),
#   area_var   = "district",
#   data      = data_with_na,
#   # handle_missing not given -> auto-select "multiple" (mice)
#   chains = 4, iter = 4000, warmup = 2000, cores = 4,
#   seed = 1
# )

## ----conv-predict-------------------------------------------------------------
# convergence_check(fit)
# 
# new_areas <- data.frame(
#   group = 101:105,
#   n     = c(150, 120, 180, 110, 140),
#   x1    = rnorm(5),
#   x2    = rnorm(5),
#   x3    = rnorm(5)
# )
# preds <- sae_predict(fit, newdata = new_areas, allow_new_levels = TRUE)
# preds

