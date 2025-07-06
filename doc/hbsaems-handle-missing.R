## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(hbsaems)
# data("data_fhnorm")
# data <- data_fhnorm
# head(data)

## -----------------------------------------------------------------------------
# data_missing <- data
# data_missing$y[3:5] <- NA

## -----------------------------------------------------------------------------
# model_deleted <- hbm(
#           formula = bf(y ~ x1 + x2 + x3),
#           hb_sampling = "gaussian",
#           hb_link = "log",
#           re = ~(1|group),
#           data = data_missing,
#           handle_missing = "deleted",
# )

## -----------------------------------------------------------------------------
# summary(model_deleted)

## -----------------------------------------------------------------------------
# data_missing <- data
# data_missing$y[3:5] <- NA
# data_missing$x1[6:7] <- NA

## -----------------------------------------------------------------------------
# model_during_model <- hbm(
#   formula = bf(y | mi() ~ mi(x1) + x2 + x3) + bf(x1 | mi() ~ x2 + x3),
#   hb_sampling = "gaussian",
#   hb_link = "log",
#   re = ~(1|group),
#   data = data_missing,
#   handle_missing = "model",
#   prior = c(
#     prior("normal(1, 0.2)", class = "Intercept", resp = "y"),
#     prior("normal(0, 0.1)", class = "b", resp = "y"),
#     prior("exponential(5)", class = "sd", resp = "y"),
# 
#     prior("normal(1, 0.2)", class = "Intercept", resp = "x1"),
#     prior("normal(0, 0.1)", class = "b", resp = "x1"),
#     prior("exponential(5)", class = "sd", resp = "x1")
#   )
# )

## -----------------------------------------------------------------------------
# summary(model_during_model)

## -----------------------------------------------------------------------------
# model_during_model <- hbm_lnln(
#       response = "y",
#       predictors = c("x1", "x2", "x3"),
#       data = data_missing,
#       handle_missing = "model"
# )

## -----------------------------------------------------------------------------
# summary(model_during_model)

## -----------------------------------------------------------------------------
# model_multiple <- hbm(
#   formula = bf(y ~ x1 + x2 + x3),
#   hb_sampling = "gaussian",
#   hb_link = "log",
#   re = ~(1|group),
#   data = data_missing,
#   handle_missing = "multiple"
# )

## -----------------------------------------------------------------------------
# summary(model_multiple)

