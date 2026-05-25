## ----setup, include = FALSE---------------------------------------------------
# Vignette evaluation strategy:
#
# By default we run lightweight diagnostic chunks (data inspection,
# convergence summaries, prediction tables) so that the vignette
# carries real output.  The model-fitting chunks themselves use
# `eval = FALSE` for two reasons:
#
#   1. Stan compile + warmup on a single chain takes ~20-60 seconds
#      on a fast machine and well over a minute on CRAN's slower
#      build agents.  With 4 model fits in this workflow the budget
#      is too tight to evaluate them all.
#   2. The vignette is meant as a reference / cookbook, not a
#      validation run -- the displayed code is what users should
#      paste into their own R session.
#
# Mini-fits used for the diagnostic chunks below are cached at the
# start of the vignette so subsequent chunks see a real hbmfit
# object without re-running Stan repeatedly.

knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE     # default: display-only
)

# Whether to actually run the mini-fit and diagnostic chunks.
# Set to FALSE in environments without a working Stan toolchain (e.g.
# CI containers that ship rstan but not the BH/Boost headers needed for
# Stan compile, or on CRAN's slow build agents where a few extra
# seconds matter).  The probe tries to compile a trivial Stan program
# in-memory; if that fails we silently fall back to display-only mode.
RUN_DIAGNOSTICS <- (function() {
  if (!requireNamespace("brms",  quietly = TRUE)) return(FALSE)
  if (!requireNamespace("rstan", quietly = TRUE)) return(FALSE)
  # Toolchain probe: try compiling the simplest Stan program possible.
  # If Boost / a C++ compiler is missing we'll get a clear error here
  # rather than later in a more confusing place.
  ok <- tryCatch({
    rstan::stan_model(
      model_code = "parameters { real x; } model { x ~ normal(0, 1); }",
      verbose = FALSE
    )
    TRUE
  }, error = function(e) FALSE,
     warning = function(w) FALSE)
  isTRUE(ok)
})()

## ----load-libs, eval = TRUE, message = FALSE, warning = FALSE-----------------
library(hbsaems)

## ----data-inspect, eval = TRUE------------------------------------------------
data("data_fhnorm")
str(data_fhnorm, max.level = 1)
head(data_fhnorm[, c("regency", "province", "y", "D", "x1", "x2", "x3")], 4)

## ----mini-fit, eval = RUN_DIAGNOSTICS, message = FALSE, warning = FALSE, cache = TRUE----
# Mini fit -- iter = 200, chains = 1 -- for vignette demonstration only.
# Do NOT use these settings for inference: the chains have not
# converged at this length and the posterior will be biased.
fit_demo <- suppressWarnings(
  hbm(
    formula           = brms::bf(y ~ x1 + x2 + x3),
    data              = data_fhnorm,
    re                = ~ (1 | regency),
    sampling_variance = "D",
    chains  = 1,
    iter    = 200,
    warmup  = 100,
    refresh = 0,
    seed    = 1
  )
)

## ----diagnostics, eval = RUN_DIAGNOSTICS--------------------------------------
# Operate on the mini fit_demo above (NOT a substitute for production
# diagnostics on full chains).
diag <- convergence_check(fit_demo)
is_converged(fit_demo)
summary(diag)
hbm_warnings(fit_demo)

## ----predictions, eval = RUN_DIAGNOSTICS--------------------------------------
# In-sample prediction (default): operates on the data stored
# inside the fitted object.
est <- sae_predict(fit_demo)
summary(est)
head(est$result_table, 5)

## ----session-info, eval = TRUE------------------------------------------------
sessionInfo()

