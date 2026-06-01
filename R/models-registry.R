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
      }
      # ---- Auxiliary phi hyperprior (Tier 2) ------------------------------
      # As of v1.0.0, the legacy
      #   phi ~ gamma(alpha, beta), alpha ~ gamma(1, 1), beta ~ gamma(1, 1)
      # hierarchical hyperprior is no longer registered for the Beta
      # family.  phi now uses brms's own default gamma(0.01, 0.01) prior.
      # The n + deff survey-design route in hbm_betalogitnorm() pins phi
      # via the generic `fixed_params` mechanism instead -- there is no
      # need for an aux_param_hyperprior callback any more.  Custom
      # families that DO need a hyperprior (e.g.\ user-registered via
      # register_hbsae_model()) can still supply one via the callback;
      # see ?register_hbsae_model.
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
    # NOTE: bernoulli is NOT registered separately; users should use the
    # binomial family with trials = 1 (the brms-canonical approach).
    poisson           = list(family = "poisson",           link = "log",
                              discrete = TRUE,  supports_mi = FALSE),
    negbinomial       = list(family = "negbinomial",       link = "log",
                              discrete = TRUE,  supports_mi = FALSE),
    geometric         = list(family = "geometric",         link = "log",
                              discrete = TRUE,  supports_mi = FALSE),
    categorical       = list(family = "categorical",       link = "logit",
                              discrete = TRUE,  supports_mi = FALSE),
    multinomial       = list(family = "multinomial",       link = "logit",
                              discrete = TRUE,  supports_mi = FALSE),
    `beta-binomial`   = list(family = "beta-binomial",     link = "logit",
                              discrete = TRUE,  supports_mi = FALSE),
    zero_inflated_poisson      = list(family = "zero_inflated_poisson",
                                       discrete = TRUE, supports_mi = FALSE),
    zero_inflated_negbinomial  = list(family = "zero_inflated_negbinomial",
                                       discrete = TRUE, supports_mi = FALSE),
    zero_inflated_binomial     = list(family = "zero_inflated_binomial",
                                       discrete = TRUE, supports_mi = FALSE),
    zero_inflated_beta_binomial = list(family = "zero_inflated_beta_binomial",
                                        discrete = TRUE, supports_mi = FALSE),

    # ---- SAE-oriented continuous families (v1.1.0) -------------------------
    # Gamma: positive continuous responses (incomes, expenditures, rates).
    # A common alternative to lognormal for right-skewed area means; the log
    # link keeps the mean positive.  supports_mi = FALSE because mi() on a
    # log-link Gamma is rarely well-behaved for SAE.
    gamma = list(
      family             = "Gamma",
      link               = "log",
      discrete           = FALSE,
      supports_mi        = FALSE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) {
        v <- y[!is.na(y)]
        length(v) == 0L || all(v > 0)
      },
      response_check_msg =
        "Gamma response must be strictly positive (y > 0).",
      default_priors     = function(...) NULL
    ),

    # Skew-normal: continuous responses with skewness the Gaussian cannot
    # capture (e.g. mildly skewed area means).  dpars = mu, sigma, alpha.
    skew_normal = list(
      family             = "skew_normal",
      link               = "identity",
      discrete           = FALSE,
      supports_mi        = TRUE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) TRUE,
      response_check_msg = NULL,
      default_priors     = function(...) NULL
    ),

    # Student-t: heavy-tailed continuous responses; the robust Fay-Herriot
    # variant that down-weights outlying areas (nu controls tail weight).
    student = list(
      family             = "student",
      link               = "identity",
      discrete           = FALSE,
      supports_mi        = TRUE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) TRUE,
      response_check_msg = NULL,
      default_priors     = function(...) NULL
    ),

    # Hurdle-lognormal: positive continuous responses with an excess of exact
    # zeros (e.g. expenditure/income where some areas report zero).  The
    # hurdle part models P(y = 0); the lognormal part models y > 0.
    hurdle_lognormal = list(
      family             = "hurdle_lognormal",
      link               = "identity",
      discrete           = FALSE,
      supports_mi        = FALSE,
      has_addition_term  = FALSE,
      addition_template  = NULL,
      response_check     = function(y) {
        v <- y[!is.na(y)]
        length(v) == 0L || all(v >= 0)
      },
      response_check_msg =
        "Hurdle-lognormal response must be non-negative (y >= 0).",
      default_priors     = function(...) NULL
    )
  )
}


# ---- Initialise on package load --------------------------------------------

.init_model_registry <- function() {
  # Defensive: clear any stale custom registrations before re-loading
  # built-ins.  Without this, calling .init_model_registry() to "reset"
  # the registry would leave behind any custom families the user
  # registered earlier in the session (or that were overwritten with
  # overwrite=TRUE in tests).  We rm() any key NOT in the built-in
  # list; the .onLoad path then re-adds loglogistic and
  # shifted_loglogistic via .register_builtin_custom_families().
  builtin_native <- names(.builtin_models())
  for (k in ls(.hbsae_model_env)) {
    if (!(k %in% builtin_native)) {
      rm(list = k, envir = .hbsae_model_env)
    }
  }
  # Re-assign all brms-native built-ins (idempotent).
  for (key in builtin_native) {
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

# All BUILT-IN family keys -- this is the comprehensive list used by the
# override-protection logic in register_hbsae_model() and
# register_hbsae_brms_custom().  It comprises:
#   1. brms-native families registered by .init_model_registry() (gaussian,
#      beta, binomial, lognormal, poisson, etc.); and
#   2. hbsaems-bundled custom brms families registered by
#      .register_builtin_custom_families() in .onLoad
#      (loglogistic, shifted_loglogistic).
#
# Keeping this central avoids the v1.0.0 bug where the
# .register_hbsae_brms_custom() override check only looked at (1) and
# would silently let the user clobber `loglogistic` or
# `shifted_loglogistic`.
.builtin_keys <- function() {
  c(names(.builtin_models()),
    "loglogistic", "shifted_loglogistic")
}

# Internal: probes whether a name corresponds to a brms-native family.
# Used by register_hbsae_brms_custom() to warn when a user-supplied key
# would shadow a brms-native family at lookup time.
#
# Implementation note: brms exposes no public predicate for this, so we
# call brmsfamily() inside tryCatch().  Success means "this is a real
# brms family"; an error (with the diagnostic listing supported families)
# means "not a brms family".  The probe is cheap -- brmsfamily() does
# only string lookup + record construction, no Stan code generation.
.is_brms_native_family <- function(name) {
  if (!requireNamespace("brms", quietly = TRUE)) return(FALSE)
  if (!is.character(name) || length(name) != 1L) return(FALSE)
  res <- tryCatch(brms::brmsfamily(name),
                  error = function(e) NULL)
  inherits(res, "brmsfamily")
}

# Internal: resolve a user-supplied `family` argument to a registry key.
# Accepts:
#   * a character string  -> treated directly as a family key;
#   * a brms family object (class "brmsfamily" / "family" / "customfamily",
#     e.g. gaussian(), Gamma(), binomial(), or brms_custom_loglogistic()).
# Returns a list(key=, link=) on success.  Errors with actionable guidance
# when the family cannot be mapped to a registered hbsaems spec.
#
# Rationale (custom distributions): a brms custom_family() carries the
# parameter/link metadata but NOT the Stan function definitions (stanvars),
# which live separately in the registry.  So an *unregistered* custom family
# object cannot be fitted from the object alone -- we direct the user to
# register it first (which stores the family + stanvars pair under a key).
.resolve_family_to_key <- function(family) {
  if (is.null(family)) return(NULL)

  # (a) character: use as a key directly.
  if (is.character(family) && length(family) == 1L) {
    return(list(key = family, link = NULL))
  }

  # (b) brms family object: extract the family name + link.
  is_family_obj <- inherits(family, "brmsfamily") ||
                   inherits(family, "customfamily") ||
                   inherits(family, "family")
  if (!is_family_obj)
    stop("`family` must be a family name (string) or a brms family object ",
         "such as gaussian(), Gamma(), binomial(), or a registered custom ",
         "family (e.g. brms_custom_loglogistic()).", call. = FALSE)

  fam_name <- family$family %||% NA_character_
  fam_link <- family$link   %||% NULL

  # (b1) custom family object: must already be registered (for its stanvars).
  if (inherits(family, "customfamily")) {
    # brms::custom_family() sets $family = "custom" for ALL custom families;
    # the discriminating identifier is $name (e.g. "hbsae_loglogistic").
    # Match on $name first to avoid collisions between custom families, then
    # fall back to $family for older registrations without a name.
    fam_id <- family$name %||% fam_name
    for (nm in .list_models()) {
      spec <- get(nm, envir = .hbsae_model_env)
      cf <- spec$custom_family
      if (is.null(cf)) next
      cf_id <- cf$name %||% cf$family
      if (identical(cf_id, fam_id))
        return(list(key = nm, link = fam_link))
    }
    stop("Custom family '", fam_id, "' is not registered with hbsaems, so ",
         "its Stan code (stanvars) is unavailable and the model cannot be ",
         "compiled.\n",
         "  Register it first, then pass the family or its key:\n",
         "    register_hbsae_brms_custom(key = \"<key>\", ",
         "custom_family = <obj>, stanvars = <obj>)\n",
         "  Built-in custom families: loglogistic, shifted_loglogistic.",
         call. = FALSE)
  }

  # (b2) native brms family object: map by name to a registered spec.
  if (is.na(fam_name) || !nzchar(fam_name))
    stop("Could not determine the family name from the supplied object.",
         call. = FALSE)
  spec <- .get_model(fam_name)
  if (is.null(spec))
    stop("Family '", fam_name, "' is not registered with hbsaems.\n",
         "  Registered families: ",
         paste(.list_models(), collapse = ", "), ".\n",
         "  Register a new one via register_hbsae_model() (brms-native) or ",
         "register_hbsae_brms_custom() (custom likelihood).", call. = FALSE)
  # Return the registry KEY (the env name), which may differ from the brms
  # family name (e.g. brms "Gamma" -> registry key "gamma").  Find the key
  # whose spec matches; fall back to fam_name if it is itself a key.
  reg_key <- if (exists(fam_name, envir = .hbsae_model_env, inherits = FALSE)) {
    fam_name
  } else {
    hit <- NA_character_
    for (nm in .list_models()) {
      if (identical(get(nm, envir = .hbsae_model_env)$family, fam_name)) {
        hit <- nm; break
      }
    }
    if (is.na(hit)) fam_name else hit
  }
  list(key = reg_key, link = fam_link)
}
