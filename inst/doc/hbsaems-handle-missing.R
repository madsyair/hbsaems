## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----preload, eval = TRUE, include = FALSE------------------------------------
suppressPackageStartupMessages(library(hbsaems))

## ----delete-------------------------------------------------------------------
#  library(hbsaems)
#  data("data_fhnorm")
#  
#  # Pattern 1: missing Y only, X complete
#  data_miss_y      <- data_fhnorm
#  data_miss_y$y[c(3, 14, 27)] <- NA
#  
#  fit_deleted <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    re             = ~ (1 | regency),
#    data           = data_miss_y,
#    handle_missing = "deleted",
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----delete-error-------------------------------------------------------------
#  data_miss_x <- data_fhnorm
#  data_miss_x$x1[6:8] <- NA
#  
#  hbm(brms::bf(y ~ x1 + x2 + x3),
#      data           = data_miss_x,
#      re             = ~ (1 | regency),
#      handle_missing = "deleted")

## ----mi-x---------------------------------------------------------------------
#  data_miss_x         <- data_fhnorm
#  data_miss_x$x1[6:8] <- NA
#  
#  fit_mi <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    re             = ~ (1 | regency),
#    data           = data_miss_x,
#    handle_missing = "multiple",
#    m              = 5,                       # 5 imputations
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----mi-y-warning-------------------------------------------------------------
#  data_miss_both <- data_fhnorm
#  data_miss_both$y[2:3]   <- NA           # missing Y
#  data_miss_both$x1[6:8]  <- NA           # missing X
#  
#  fit_mi_both <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    re             = ~ (1 | regency),
#    data           = data_miss_both,
#    handle_missing = "multiple"
#  )

## ----mi-y-only-continuous-----------------------------------------------------
#  data_miss_y_only <- data_fhnorm
#  data_miss_y_only$y[2:3] <- NA           # only Y missing
#  
#  # Gaussian family -> auto-converts to "model" with | mi() on LHS
#  fit_auto1 <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    re             = ~ (1 | regency),
#    data           = data_miss_y_only,
#    handle_missing = "multiple"
#  )

## ----mi-args------------------------------------------------------------------
#  fit_mi2 <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    re             = ~ (1 | regency),
#    data           = data_miss_x,
#    handle_missing = "multiple",
#    m              = 10,
#    mice_args      = list(method = "pmm", maxit = 20),
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----mi-model-----------------------------------------------------------------
#  data_miss_both <- data_fhnorm
#  data_miss_both$y[2:3]   <- NA
#  data_miss_both$x1[6:8]  <- NA
#  
#  fit_model <- hbm(
#    formula        = brms::bf(y  | mi() ~ mi(x1) + x2 + x3) +
#                     brms::bf(x1 | mi() ~ x2 + x3),
#    re             = ~ (1 | regency),
#    data           = data_miss_both,
#    handle_missing = "model",
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----auto---------------------------------------------------------------------
#  # Build a data frame with some missing values to demonstrate
#  data("data_lnln")
#  data_with_some_na <- data_lnln
#  data_with_some_na$x1[c(3, 14, 27)] <- NA    # missing covariate
#  
#  fit_auto <- hbm_lnln(
#    response  = "y_obs",
#    auxiliary = c("x1", "x2", "x3"),
#    area_var  = "district",
#    data      = data_with_some_na,
#    # handle_missing not given
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----compare------------------------------------------------------------------
#  loo_compare <- loo::loo_compare(
#    loo::loo(fit_deleted$brmsfit),
#    loo::loo(fit_mi$brmsfit),
#    loo::loo(fit_model$brmsfit)
#  )
#  loo_compare

