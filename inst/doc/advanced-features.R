## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## -----------------------------------------------------------------------------
#  library(hbsaems)

## -----------------------------------------------------------------------------
#  data("data_fhnorm")
#  
#  chk <- check_data(
#    data       = data_fhnorm,
#    response   = "y",
#    auxiliary  = c("x1", "x2", "x3"),
#    spatial_var = "province"
#  )
#  
#  print(chk)
#  #>  HBSAE Data Check  [hbsaems_data_check]
#  #>  ---------------------------------------
#  #>   Observations    : 50
#  #>   Missing pattern : none (data complete)
#  #>   Issues          : none
#  #>
#  #>   - No missing values detected. handle_missing can stay NULL.

## -----------------------------------------------------------------------------
#  is.hbsaems_check(chk)   # TRUE

## -----------------------------------------------------------------------------
#  d2 <- data_fhnorm
#  d2$y[1:5] <- NA
#  chk2 <- check_data(d2, response = "y", auxiliary = c("x1", "x2", "x3"))
#  chk2$non_sample_warning

## -----------------------------------------------------------------------------
#  # Queen contiguity + binary -> for CAR (Besag 1974)
#  M_car <- build_spatial_weight(
#    shp       = "path/to/areas.shp",
#    for_model = "car"         # implies type = "queen", style = "B"
#  )
#  
#  # K-nearest + row-standardised -> for SAR (Anselin 1988)
#  M_sar <- build_spatial_weight(
#    shp       = "path/to/areas.shp",
#    for_model = "sar",        # implies type = "knn", style = "W"
#    k         = 4
#  )

## -----------------------------------------------------------------------------
#  check_spatial_weight(M_car, spatial_model = "car")
#  #> Spatial Weight Matrix Diagnostic
#  #> ---------------------------------
#  #>   Square          : TRUE
#  #>   Zero diagonal   : TRUE
#  #>   Symmetric       : TRUE
#  #>   Detected style  : B
#  #>   Isolated areas  : 0
#  #>   Components      : 1
#  #>   Matrix is theoretically compatible.

## -----------------------------------------------------------------------------
#  # Suppose we have SAE estimates per kecamatan, with a kabupaten column
#  estimates <- sae_predict(fit_hbm)
#  
#  # Official BPS totals
#  T_kab <- c(Bogor = 110, Sukabumi = 145, Cianjur = 145)
#  
#  bm <- sae_benchmark(
#    predictions = estimates,
#    target      = T_kab,                  # one target per kabupaten
#    weights     = data_fhnorm$populasi,   # population per kecamatan
#    groups      = data_fhnorm$kabupaten,  # mapping kec -> kab
#    method      = "raking"
#  )
#  
#  bm$benchmark_info$converged    # raking with one group var: converges in 1 sweep

## -----------------------------------------------------------------------------
#  # Pass posterior = TRUE if predictions object carries draws
#  bm_bayes <- sae_benchmark(
#    predictions = estimates,
#    target      = 1000,
#    method      = "ratio",
#    posterior   = TRUE,                # NEW in v1.0.0
#    probs       = c(0.025, 0.5, 0.975)
#  )
#  
#  bm_bayes$result_table
#  #>    Prediction      SD  RSE_percent      Q025      Q50      Q975
#  #> 1     ...           ...     ...          ...      ...      ...
#  
#  # Or supply your own draws matrix (D x n)
#  draws <- posterior_predict(fit$model)
#  bm_bayes2 <- sae_benchmark(estimates, target = 1000,
#                              method = "ratio",
#                              posterior = draws)

## -----------------------------------------------------------------------------
#  # Default: thin-plate regression spline, basis dimension chosen by mgcv
#  fit_spline <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    data           = data_fhnorm,
#    re             = ~ (1 | regency),       # area-level random effect (Fay-Herriot)
#    nonlinear      = c("x2", "x3"),
#    nonlinear_type = "spline",
#    spline_k       = 7L,                    # basis dim (-1 = auto)
#    spline_bs      = "tp",                  # "tp" (default), "cr", "cs", "ps"
#    chains = 2, iter = 2000, refresh = 0
#  )

## -----------------------------------------------------------------------------
#  # RECOMMENDED: HSGP with Matérn 5/2 covariance
#  fit_gp <- hbm(
#    formula        = brms::bf(y ~ x1 + x2),
#    data           = data_fhnorm,
#    re             = ~ (1 | regency),
#    nonlinear      = "x1",
#    nonlinear_type = "gp",
#    gp_k           = 20L,                  # basis dim — REQUIRED for n > 100
#    gp_cov         = "matern25",           # more stable than "exp_quad"
#    gp_c           = 1.25,                 # boundary scale (brms default)
#    chains = 2, iter = 2000, refresh = 0
#  )

## -----------------------------------------------------------------------------
#  # Build the smooth-term spec once, reuse across models
#  nl <- hbm_nonlinear(c("x1"), type = "gp",
#                       k = 20, gp_cov = "matern25")
#  
#  fit <- hbm_flex(family_key = "lognormal",
#                   response  = "y_obs",
#                   auxiliary = c("x1", "x2"),
#                   area_var  = "district",
#                   data      = data_lnln,
#                   nl)                       # spliced in automatically

## -----------------------------------------------------------------------------
#  fit_hs <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3 + x4 + x5),
#    data           = my_data,
#    re             = ~ (1 | regency),
#    prior_type     = "horseshoe",
#    hs_df          = 1,        # local half-t df  (1 = original HS)
#    hs_df_global   = 1,        # global half-t df
#    hs_df_slab     = 4,        # slab half-t df   (regularised HS+)
#    hs_scale_slab  = 2,        # slab scale
#    hs_autoscale   = TRUE,     # scale with residual sigma (Gaussian only)
#    chains = 2, iter = 2000, refresh = 0
#  )

## -----------------------------------------------------------------------------
#  fit_r2d2 <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3 + x4 + x5),
#    data           = my_data,
#    re             = ~ (1 | regency),
#    prior_type     = "r2d2",
#    r2d2_mean_R2   = 0.5,      # prior mean of R^2
#    r2d2_prec_R2   = 2,        # prior precision of R^2
#    r2d2_autoscale = TRUE,
#    chains = 2, iter = 2000, refresh = 0
#  )

## -----------------------------------------------------------------------------
#  # Model with linear coefficients + spline + Gaussian process
#  fit_combined <- hbm(
#    formula        = brms::bf(y ~ x1 + x2 + x3),
#    data           = my_data,
#    re             = ~ (1 | regency),
#    nonlinear      = c("x2", "x3"),
#    nonlinear_type = "spline",       # spline for x2 AND x3
#    prior_type     = "horseshoe",    # cascades automatically
#    chains = 2, iter = 2000, refresh = 0
#  )

## -----------------------------------------------------------------------------
#  fit_bin_hs <- hbm_binlogitnorm(
#    response       = "y",
#    trials         = "n",
#    auxiliary      = c("x1", "x2", "x3", "x4", "x5"),
#    area_var       = "district",
#    data           = data_binlogitnorm,
#    prior_type     = "horseshoe",
#    hs_autoscale   = FALSE,            # binomial: no residual SD to scale by
#    chains = 2, iter = 2000, refresh = 0
#  )

## -----------------------------------------------------------------------------
#  list_hbsae_models()
#  #> [1] "bernoulli" "Beta" "beta-binomial" "binomial" ...
#  
#  get_hbsae_model("beta")
#  #> $family             : "Beta"
#  #> $link               : "logit"
#  #> $discrete           : FALSE
#  #> $supports_mi        : TRUE
#  #> $aux_param_hyperprior: function(args, data) ...   # phi ~ gamma(alpha, beta)

## -----------------------------------------------------------------------------
#  register_hbsae_model(
#    key                = "gamma_log",
#    family             = "Gamma",
#    link               = "log",
#    discrete           = FALSE,
#    supports_mi        = TRUE,
#    response_check     = function(y) all(y > 0, na.rm = TRUE),
#    response_check_msg = "Gamma response must be strictly positive."
#  )
#  
#  # Use immediately
#  fit_gamma <- hbm_flex(
#    family_key = "gamma_log",
#    response   = "expenditure",
#    auxiliary  = c("age", "income", "education"),
#    area_var   = "kecamatan",                    # area-level random effect
#    data       = my_household_data
#  )

## -----------------------------------------------------------------------------
#  register_hbsae_model(
#    key            = "gamma_with_hyperprior",
#    family         = "Gamma",
#    link           = "log",
#    response_check = function(y) all(y > 0, na.rm = TRUE),
#  
#    # Inject Stan code via stanvars + a prior on the shape parameter
#    aux_param_hyperprior = function(args, data) {
#      if (!isTRUE(args$add_shape_prior)) return(NULL)
#      list(
#        stanvars = brms::stanvar(
#                    scode = "real<lower=0> shape_a;\n  real<lower=0> shape_b;",
#                    block = "parameters") +
#                  brms::stanvar(
#                    scode = "shape_a ~ gamma(1, 1);\n  shape_b ~ gamma(1, 1);",
#                    block = "model"),
#        prior    = brms::set_prior("gamma(shape_a, shape_b)", class = "shape")
#      )
#    }
#  )
#  
#  # Trigger the hyperprior via aux_args
#  fit <- hbm_flex(
#    family_key = "gamma_with_hyperprior",
#    response   = "y",
#    auxiliary  = c("x1", "x2"),
#    area_var   = "area_id",                      # area-level random effect
#    data       = my_data,
#    aux_args   = list(add_shape_prior = TRUE)
#  )

## -----------------------------------------------------------------------------
#  # Fit two competing models
#  fit_linear <- hbm(brms::bf(y ~ x1 + x2 + x3),
#                     data = my_data, re = ~(1 | regency))
#  fit_smooth <- hbm(brms::bf(y ~ x1 + x2 + x3),
#                     data           = my_data,
#                     re             = ~(1 | regency),
#                     nonlinear      = c("x2", "x3"),
#                     nonlinear_type = "spline")
#  
#  # Stacking (recommended Bayesian model averaging)
#  avg_stack <- model_average(fit_linear, fit_smooth,
#                              method = "stacking")
#  
#  # Inspect the computed weights
#  attr(avg_stack, "weights")
#  #> [1] 0.34 0.66
#  attr(avg_stack, "weight_method")
#  #> [1] "stacking"

## -----------------------------------------------------------------------------
#  fit <- hbm(brms::bf(y ~ x1 + x2 + x3),
#             data           = my_data,
#             re             = ~(1 | regency),
#             prior_type     = "horseshoe",
#             chains = 4, iter = 4000)
#  
#  # Report power-scaling diagnostics for every monitored parameter
#  ps <- prior_sensitivity(fit)
#  print(ps)
#  #> # A tibble: 8 x 4
#  #>   variable                  prior  likelihood diagnosis
#  #>   <chr>                     <dbl>       <dbl> <chr>
#  #> 1 b_Intercept              0.012        0.821 -
#  #> 2 b_x1                     0.156        0.683 -
#  #> 3 b_x2                     0.834        0.412 prior-data conflict
#  #> ...

## -----------------------------------------------------------------------------
#  # 1. Check data
#  chk <- check_data(my_data, response = "y",
#                     auxiliary  = c("x1", "x2", "x3"),
#                     spatial_var = "kecamatan")
#  
#  # 2. Build CAR matrix from shapefile
#  M <- build_spatial_weight("kecamatan.shp",
#                              for_model = "car",
#                              id_col    = "kec_code")
#  
#  # 3. Configure with helpers (v1.0.0+); pass bundles directly to hbm()
#  priors <- hbm_priors(prior_type = "horseshoe", hs_df_slab = 4)
#  nl     <- hbm_nonlinear(c("x1"), type = "spline", k = 5)
#  ctrl   <- hbm_control(chains = 4, iter = 4000, cores = 4,
#                         adapt_delta = 0.95)
#  
#  # 4. Fit
#  fit <- hbm(
#    formula = brms::bf(y ~ x1 + x2 + x3),
#    data    = my_data,
#    spatial_var = "kecamatan", spatial_model = "car", M = M,
#    handle_missing = chk$recommended_method,
#    priors, nl, ctrl
#  )
#  
#  # 5. Predict at all areas (including non-sample)
#  est <- sae_predict(fit)
#  
#  # 6. Benchmark to kabupaten totals (fully Bayesian)
#  T_kab <- c(Bogor = 110, Sukabumi = 145, Cianjur = 145)
#  bm <- sae_benchmark(est,
#                       target  = T_kab,
#                       weights = my_data$populasi,
#                       groups  = my_data$kabupaten,
#                       method  = "raking",
#                       posterior = TRUE)
#  
#  # 7. Use the corrected uncertainty
#  bm$result_table

