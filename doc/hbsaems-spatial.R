## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(hbsaems)
# 
# # Load data
# data("data_fhnorm")
# data <- data_fhnorm
# head(data)
# 
# # Load adjacency matrix
# data("adjacency_matrix_car")
# adjacency_matrix_car

## -----------------------------------------------------------------------------
# model_car <- hbm(
#   formula = bf(y ~ x1 + x2 + x3),  # Formula model
#   hb_sampling = "gaussian",        # Gaussian family for continuous outcomes
#   hb_link = "identity",            # Identity link function (no transformation)
#   re = ~(1|group),
#   sre = "sre",                    # Spatial random effect variable
#   sre_type = "car",
#   car_type = "icar",
#   M = adjacency_matrix_car,
#   data = data)                    # Dataset
# 
# summary(model_car)

## -----------------------------------------------------------------------------
# # Load data
# data("data_betalogitnorm")
# head(data_betalogitnorm)
# 
# model_car_beta <- hbm_betalogitnorm(response = "y",
#                                     predictors = c("x1", "x2", "x3"),
#                                     sre = "sre",
#                                     sre_type = "car",
#                                     car_type = "icar",
#                                     M = adjacency_matrix_car,
#                                     data = data_betalogitnorm)
# summary(model_car_beta)

## -----------------------------------------------------------------------------
# library(hbsaems)
# 
# # Load data
# data("data_fhnorm")
# data <- data_fhnorm
# head(data)
# 
# # Load adjacency matrix
# data("spatial_weight_sar")
# spatial_weight_sar

## -----------------------------------------------------------------------------
# model_sar <- hbm(
#   formula = bf(y ~ x1 + x2 + x3),  # Formula model
#   hb_sampling = "gaussian",        # Gaussian family for continuous outcomes
#   hb_link = "identity",            # Identity link function (no transformation)
#   re = ~(1|group),
#   sre_type = "sar",
#   sar_type = "lag",
#   M = spatial_weight_sar,
#   data = data)                    # Dataset
# 
# summary(model_sar)

