# R/models-registry.R
# =============================================================================
# Model registry: a single source of truth for distribution-specific
# behaviour in hbsaems.
#
# Replaces the hardcoded `discrete_families` list in hbm() and the long
# duplicated wrappers in hbm_lnln.R / hbm_binlogitnorm.R / hbm_betalogitnorm.R.
#
# A "model spec" is a named list with:
#   $family               -- brms family name (passed to hb_sampling)
#   $link                 -- default link
#   $discrete             -- logical; affects mi() availability
#   $supports_mi          -- logical; whether brms::mi() can impute Y
#   $has_addition_term    -- logical; e.g. binomial needs y | trials(n)
#   $addition_template    -- "%s | trials(%s) ~ %s" if has_addition_term
#   $response_check       -- function(y) -> logical; domain validation
#   $response_check_msg   -- character; error message on validation fail
#   $default_priors       -- function(...) -> brmsprior or NULL
#
#   $aux_param_hyperprior -- function(args, data) -> list(prior, stanvars)
#                            optional; used by distributions like Beta that
#                            have an auxiliary parameter (phi, shape, ...)
#                            with a hyperprior expressed in raw Stan code.
#                            'args' contains model-specific input (e.g.
#                            n_var, deff_var, etc.).
#
# Although individual spec slots still use the term "family" (because that
# matches brms terminology -- $family stores the brms family name), the
# spec as a whole describes a *model* type: family + link + addition term +
# missing-data behaviour + auxiliary-parameter hyperprior callback, etc.
# Hence the user-facing functions uses "model" wording (register/list/get).
#
# The registry lives in a package-internal environment so it can be
# extended at runtime via register_hbsae_model() without touching
# hbsaems source.
# =============================================================================


# ---- Internal environment holding the registry ------------------------------
.hbsae_model_env <- new.env(parent = emptyenv())


# ---- Built-in families ------------------------------------------------------

.builtin_models <- function() {
  list(
    # ---- Gaussian (Fay-Herriot) --------------------------------------------
    gaussian = list(
      family             = "gaussian",
      link               = "identity",
      discrete           = FALSE,
      supports_mi        = TRUE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) TRUE,
      response_check_msg = NULL,
      default_priors     = function(...) NULL
    ),

    # ---- Beta (proportion in (0,1)) ----------------------------------------
    beta = list(
      family             = "Beta",
      link               = "logit",
      discrete           = FALSE,
      supports_mi        = TRUE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) {
        v <- y[!is.na(y)]
        length(v) == 0L || all(v > 0 & v < 1)
      },
      response_check_msg = paste0(
        "Beta response must be strictly in (0, 1). ",
        "Found value(s) at or beyond the boundary."
      ),
      default_priors     = function(...) {
        c(brms::set_prior("student_t(4, 0, 10)",  class = "Intercept"),
          brms::set_prior("student_t(4, 0, 2.5)", class = "b"))
      },
      # ---- Auxiliary phi hyperprior (Tier 2) ------------------------------
      # Triggered when the user supplies n and deff in the wrapper call.
      # Constructs phi ~ gamma(alpha, beta) with alpha, beta themselves
      # given a gamma(1, 1) hyperprior, injected via stanvars.
      aux_param_hyperprior = function(args, data) {
        if (is.null(args$n) || is.null(args$deff)) return(NULL)
        if (!(args$n %in% names(data)))
          stop(sprintf("Variable '%s' (sample size) not found in 'data'.",
                       args$n), call. = FALSE)
        if (!(args$deff %in% names(data)))
          stop(sprintf("Variable '%s' (design effect) not found in 'data'.",
                       args$deff), call. = FALSE)

        list(
          stanvars = brms::stanvar(
              scode = "real<lower=1> alpha;\n  real<lower=0> beta;",
              block = "parameters") +
            brms::stanvar(
              scode = "alpha ~ gamma(1, 1);\n  beta ~ gamma(1, 1);",
              block = "model"),
          prior = brms::set_prior("gamma(alpha, beta)", class = "phi")
        )
      }
    ),

    # ---- Binomial (counts out of trials) -----------------------------------
    binomial = list(
      family             = "binomial",
      link               = "logit",
      discrete           = TRUE,
      supports_mi        = FALSE,
      has_addition_term  = TRUE,
      addition_template  = "%s | trials(%s) ~ %s",
      response_check     = function(y) {
        v <- y[!is.na(y)]
        length(v) == 0L ||
          (all(v >= 0) &&
           all(abs(v - round(v)) < .Machine$double.eps^0.5))
      },
      response_check_msg = paste0(
        "Binomial successes variable must be non-negative integers."
      ),
      default_priors     = function(...) {
        c(brms::set_prior("student_t(4, 0, 10)",  class = "Intercept"),
          brms::set_prior("student_t(4, 0, 2.5)", class = "b"))
      }
    ),

    # ---- Lognormal (positive continuous) -----------------------------------
    lognormal = list(
      family             = "lognormal",
      link               = "identity",
      discrete           = FALSE,
      supports_mi        = TRUE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) {
        v <- y[!is.na(y)]
        length(v) == 0L || all(v > 0)
      },
      response_check_msg = paste0(
        "Lognormal response must be strictly positive (y > 0)."
      ),
      default_priors     = function(...) NULL
    ),

    # ---- Other discrete families flagged for mi() guard --------------------
    bernoulli         = list(family = "bernoulli",         discrete = TRUE,
                              supports_mi = FALSE),
    poisson           = list(family = "poisson",           discrete = TRUE,
                              supports_mi = FALSE),
    negbinomial       = list(family = "negbinomial",       discrete = TRUE,
                              supports_mi = FALSE),
    geometric         = list(family = "geometric",         discrete = TRUE,
                              supports_mi = FALSE),
    categorical       = list(family = "categorical",       discrete = TRUE,
                              supports_mi = FALSE),
    multinomial       = list(family = "multinomial",       discrete = TRUE,
                              supports_mi = FALSE),
    `beta-binomial`   = list(family = "beta-binomial",     discrete = TRUE,
                              supports_mi = FALSE),
    zero_inflated_poisson      = list(family = "zero_inflated_poisson",
                                       discrete = TRUE, supports_mi = FALSE),
    zero_inflated_negbinomial  = list(family = "zero_inflated_negbinomial",
                                       discrete = TRUE, supports_mi = FALSE),
    zero_inflated_binomial     = list(family = "zero_inflated_binomial",
                                       discrete = TRUE, supports_mi = FALSE),
    zero_inflated_beta_binomial = list(family = "zero_inflated_beta_binomial",
                                        discrete = TRUE, supports_mi = FALSE)
  )
}


# ---- Initialise on package load --------------------------------------------

.init_model_registry <- function() {
  for (key in names(.builtin_models())) {
    assign(key, .builtin_models()[[key]], envir = .hbsae_model_env)
  }
}


# ---- Internal accessors -----------------------------------------------------

# Look up a family by either the user-supplied key (e.g. "lognormal") or by
# the underlying brms family name (e.g. "Beta").  Returns NULL if not found.
.get_model <- function(key) {
  if (is.null(key)) return(NULL)
  if (exists(key, envir = .hbsae_model_env, inherits = FALSE))
    return(get(key, envir = .hbsae_model_env))

  # Fall back to lookup by $family field
  for (nm in ls(.hbsae_model_env)) {
    fam <- get(nm, envir = .hbsae_model_env)
    if (identical(fam$family, key)) return(fam)
  }
  NULL
}

# Boolean: is the given brms family discrete?
.model_is_discrete <- function(family_name) {
  spec <- .get_model(family_name)
  isTRUE(spec$discrete)
}

# Boolean: does this family support brms::mi() Y-imputation?
.family_supports_mi <- function(family_name) {
  spec <- .get_model(family_name)
  isTRUE(spec$supports_mi)
}

# All registered family keys
.list_models <- function() ls(.hbsae_model_env)
