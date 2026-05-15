## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----preload, eval = TRUE, include = FALSE------------------------------------
suppressPackageStartupMessages(library(hbsaems))

## ----delete-------------------------------------------------------------------
# library(hbsaems)
# data("data_fhnorm")
# data_miss_y      <- data_fhnorm
# data_miss_y$y[c(3, 14, 27)] <- NA
# 
# fit_deleted <- hbm(
#   formula        = brms::bf(y ~ x1 + x2 + x3),
#   re             = ~ (1 | group),
#   data           = data_miss_y,
#   handle_missing = "deleted",
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----mi-----------------------------------------------------------------------
# data_miss_x         <- data_fhnorm
# data_miss_x$x1[6:8] <- NA
# 
# fit_mi <- hbm(
#   formula        = brms::bf(y ~ x1 + x2 + x3),
#   re             = ~ (1 | group),
#   data           = data_miss_x,
#   handle_missing = "multiple",
#   m              = 5,                       # 5 imputations
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----mi-args------------------------------------------------------------------
# fit_mi2 <- hbm(
#   formula        = brms::bf(y ~ x1 + x2 + x3),
#   re             = ~ (1 | group),
#   data           = data_miss_x,
#   handle_missing = "multiple",
#   m              = 10,
#   mice_args      = list(method = "pmm", maxit = 20),
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----mi-model-----------------------------------------------------------------
# fit_model <- hbm(
#   formula        = brms::bf(y  | mi() ~ mi(x1) + x2 + x3) +
#                    brms::bf(x1 | mi() ~ x2 + x3),
#   re             = ~ (1 | group),
#   data           = data_miss_x,
#   handle_missing = "model",
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----auto---------------------------------------------------------------------
# fit_auto <- hbm_lnln(
#   response  = "y_obs",
#   auxiliary = c("x1", "x2", "x3"),
#   group     = "group",
#   data      = data_with_some_na,
#   # handle_missing not given
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----compare------------------------------------------------------------------
# loo_compare <- loo::loo_compare(
#   loo::loo(fit_deleted$brmsfit),
#   loo::loo(fit_mi$brmsfit),
#   loo::loo(fit_model$brmsfit)
# )
# loo_compare

