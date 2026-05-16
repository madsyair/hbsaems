## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----preload, eval = TRUE, include = FALSE------------------------------------
suppressPackageStartupMessages(library(hbsaems))

## ----show-data, eval = TRUE---------------------------------------------------
data("data_betalogitnorm")
str(data_betalogitnorm[, c("group", "y", "n", "deff", "x1", "x2", "x3")])

## ----fit-random---------------------------------------------------------------
#  library(hbsaems)
#  library(brms)
#  
#  fit_random <- hbm_betalogitnorm(
#    response  = "y",
#    auxiliary = c("x1", "x2", "x3"),
#    group     = "group",
#    data      = data_betalogitnorm,
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )
#  summary(fit_random)

## ----fit-fixed----------------------------------------------------------------
#  fit_fixed <- hbm_betalogitnorm(
#    response  = "y",
#    auxiliary = c("x1", "x2", "x3"),
#    n         = "n",                   # <- column with sample sizes
#    deff      = "deff",                # <- column with design effects
#    group     = "group",
#    data      = data_betalogitnorm,
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )
#  summary(fit_fixed)

## ----fit-custom-hyperprior----------------------------------------------------
#  fit_custom <- hbm_betalogitnorm(
#    response  = "y",
#    auxiliary = c("x1", "x2", "x3"),
#    group     = "group",
#    data      = data_betalogitnorm,
#    stanvars  = stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
#                stanvar(scode = "beta  ~ gamma(2, 3);", block = "model"),
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )
#  summary(fit_custom)

## ----fit-phi-prior------------------------------------------------------------
#  fit_phi <- hbm_betalogitnorm(
#    response  = "y",
#    auxiliary = c("x1", "x2", "x3"),
#    group     = "group",
#    data      = data_betalogitnorm,
#    prior     = brms::set_prior("gamma(2, 0.05)", class = "phi"),
#    chains = 4, iter = 4000, warmup = 2000, cores = 4,
#    seed = 1
#  )

## ----fit-flex-----------------------------------------------------------------
#  fit_flex <- hbm_flex(
#    family_key   = "Beta",
#    response     = "y",
#    auxiliary    = c("x1", "x2", "x3"),
#    group        = "group",
#    data         = data_betalogitnorm,
#    fixed_params = list(phi = ~ I(n / deff - 1)),    # <- formula spec
#    chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
#  )

## ----conv-check---------------------------------------------------------------
#  convergence_check(fit_fixed)

## ----loo-compare--------------------------------------------------------------
#  loo_compare <- loo::loo_compare(
#    loo::loo(fit_random$brmsfit),
#    loo::loo(fit_fixed$brmsfit)
#  )
#  loo_compare

## ----predict------------------------------------------------------------------
#  new_areas <- data.frame(
#    group = 51:55,
#    x1    = rnorm(5),
#    x2    = rnorm(5),
#    x3    = rnorm(5)
#  )
#  preds <- sae_predict(fit_fixed, newdata = new_areas, allow_new_levels = TRUE)
#  preds

