## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----preload, eval = TRUE, include = FALSE------------------------------------
suppressPackageStartupMessages(library(hbsaems))

## ----weight-mat, eval = TRUE--------------------------------------------------
data("adjacency_matrix_car")
dim(adjacency_matrix_car)
data("spatial_weight_sar")
dim(spatial_weight_sar)

## ----build-weights------------------------------------------------------------
# # Example workflow (sf must be installed)
# library(sf)
# shp <- st_read("kecamatan.shp", quiet = TRUE)
# W   <- build_spatial_weight(shp,
#                             neighbour_method = "queen",
#                             standardise      = "row")

## ----fit-icar-----------------------------------------------------------------
# library(hbsaems)
# data("data_fhnorm")
# data("adjacency_matrix_car")
# 
# fit_icar <- hbm(
#   formula  = brms::bf(y ~ x1 + x2 + x3),
#   hb_sampling = "gaussian",
#   sre      = "sre",
#   sre_type = "car",
#   M        = adjacency_matrix_car,
#   data     = data_fhnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4,
#   seed = 1
# )
# summary(fit_icar)

## ----fit-escar----------------------------------------------------------------
# fit_escar <- hbm(
#   formula  = brms::bf(y ~ x1 + x2 + x3),
#   sre      = "sre",
#   sre_type = "car",
#   car_type = "escar",
#   M        = adjacency_matrix_car,
#   data     = data_fhnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----fit-bym2-----------------------------------------------------------------
# fit_bym2 <- hbm(
#   formula  = brms::bf(y ~ x1 + x2 + x3),
#   re       = ~ (1 | sre),                    # IID counterpart
#   sre      = "sre",
#   sre_type = "car",
#   car_type = "bym2",
#   M        = adjacency_matrix_car,
#   data     = data_fhnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----fit-sar------------------------------------------------------------------
# data("spatial_weight_sar")
# fit_sar <- hbm(
#   formula  = brms::bf(y ~ x1 + x2 + x3),
#   sre      = "sre",
#   sre_type = "sar",
#   M        = spatial_weight_sar,
#   data     = data_fhnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )
# summary(fit_sar)

## ----wrapper-spatial----------------------------------------------------------
# # Beta + CAR
# fit_beta_car <- hbm_betalogitnorm(
#   response  = "y", auxiliary = c("x1", "x2", "x3"),
#   n         = "n", deff = "deff",
#   sre       = "sre", sre_type = "car", M = adjacency_matrix_car,
#   data      = data_betalogitnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )
# 
# # Binomial + BYM2
# fit_bin_bym2 <- hbm_binlogitnorm(
#   response  = "y", trials = "n",
#   auxiliary = c("x1", "x2", "x3"),
#   re        = ~ (1 | group),     # IID counterpart
#   sre       = "sre", sre_type = "car", car_type = "bym2",
#   M         = adjacency_matrix_car,
#   data      = data_binlogitnorm,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----validate-----------------------------------------------------------------
# check_spatial_weight(adjacency_matrix_car)

## ----loo-spatial--------------------------------------------------------------
# fit_iid <- hbm(formula = brms::bf(y ~ x1 + x2 + x3),
#                re = ~ (1 | sre), data = data_fhnorm,
#                chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1)
# 
# loo_compare <- loo::loo_compare(
#   loo::loo(fit_iid$brmsfit),
#   loo::loo(fit_icar$brmsfit),
#   loo::loo(fit_bym2$brmsfit)
# )
# loo_compare

