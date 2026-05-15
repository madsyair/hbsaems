## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE     # heavy Stan fits are not run at vignette-build time
)

## ----data-prep, eval = TRUE, include = FALSE----------------------------------
# These chunks always evaluate (no Stan fits): they're used to populate
# inline data summaries.
suppressPackageStartupMessages(library(hbsaems))

## ----data-glimpse, eval = TRUE------------------------------------------------
data("data_lnln")
str(data_lnln[, c("group", "y_obs", "psi_i", "x1", "x2", "x3")])

## ----fit-basic----------------------------------------------------------------
# library(hbsaems)
# fit <- hbm_lnln(
#   response  = "y_obs",
#   auxiliary = c("x1", "x2", "x3"),
#   group     = "group",
#   data      = data_lnln,
#   chains    = 4, iter = 4000, warmup = 2000, cores = 4,
#   seed      = 1
# )

## ----fit-fh-------------------------------------------------------------------
# fit_fh <- hbm_lnln(
#   response     = "y_obs",
#   auxiliary    = c("x1", "x2", "x3"),
#   group        = "group",
#   sampling_var = "psi_i",     # <- pins sigma_i = sqrt(psi_i)
#   data         = data_lnln,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )

## ----inspect------------------------------------------------------------------
# summary(fit_fh)

## ----inspect-data, eval = TRUE------------------------------------------------
data("data_lnln")
summary(data_lnln[, c("y_obs", "psi_i", "x1", "x2", "x3")])

## ----conv-check---------------------------------------------------------------
# convergence_check(fit)

## ----loo-compare--------------------------------------------------------------
# fit_no_x3 <- hbm_lnln(
#   response  = "y_obs",
#   auxiliary = c("x1", "x2"),         # drop x3
#   group     = "group",
#   data      = data_lnln,
#   chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
# )
# loo_compare <- loo::loo_compare(loo::loo(fit$brmsfit),
#                                  loo::loo(fit_no_x3$brmsfit))
# loo_compare

## ----predict------------------------------------------------------------------
# # Suppose we have auxiliary data for areas 101 to 110 (no direct estimates)
# new_data <- data.frame(
#   group = 101:110,
#   x1    = rnorm(10),
#   x2    = rnorm(10),
#   x3    = rnorm(10)
# )
# preds <- sae_predict(fit, newdata = new_data, allow_new_levels = TRUE)
# head(preds)

