# R/zzz.R
# =============================================================================
# Startup hook: populate the model registry with built-in and v1.0.0 custom
# brms families (loglogistic, shifted_loglogistic) so they are available in
# every fresh R session.
# =============================================================================

.onLoad <- function(libname, pkgname) {
  # 1. Built-in (brms-native) families
  .init_model_registry()

  # 2. Built-in custom brms families (require brms namespace, so we
  # guard with requireNamespace to keep load-time light if brms is missing).
  if (requireNamespace("brms", quietly = TRUE)) {
    .register_builtin_custom_families()
  }
}

# ---- Internal: register loglogistic + shifted_loglogistic ----
# Called from .onLoad; kept in zzz.R for clarity.  Each family is
# auto-registered with the canonical key it shipped with so the user can
# refer to it by string in hbm()/hbm_flex() without any setup.
.register_builtin_custom_families <- function() {

  # Loglogistic
  if (!exists("loglogistic", envir = .hbsae_model_env, inherits = FALSE)) {
    ll <- brms_custom_loglogistic()
    register_hbsae_brms_custom(
      key                = "loglogistic",
      custom_family      = ll$custom_family,
      stanvars           = ll$stanvars_family,
      response_check     = function(y) all(y > 0, na.rm = TRUE),
      response_check_msg = paste0(
        "Loglogistic response must be strictly positive (y > 0)."
      ),
      supports_mi        = FALSE,
      discrete           = FALSE
    )
  }

  # Shifted Loglogistic
  if (!exists("shifted_loglogistic",
              envir = .hbsae_model_env, inherits = FALSE)) {
    sll <- brms_custom_shifted_loglogistic()
    register_hbsae_brms_custom(
      key                = "shifted_loglogistic",
      custom_family      = sll$custom_family,
      stanvars           = sll$stanvars_family,
      response_check     = NULL,        # full real line in principle
      response_check_msg = NULL,
      supports_mi        = FALSE,
      discrete           = FALSE
    )
  }

  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion("hbsaems")
  packageStartupMessage(
    "hbsaems ", v, "\n",
    "  Main functions : convergence_check() | model_compare() | ",
    "sae_predict() | prior_check()\n",
    "  Custom dist : loglogistic, shifted_loglogistic (brms custom families)\n",
    "  Deprecated  : hbcc() | hbmc() | hbpc() | hbsae() ",
    "(removal scheduled for v2.0.0)\n",
    "  Type ?convergence_check or browseVignettes(\"hbsaems\")."
  )
}
