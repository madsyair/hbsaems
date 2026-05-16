# R/register-custom-brms.R
# =============================================================================
# Bridge between `brms::custom_family()` and the hbsaems model registry.
#
# Whereas register_hbsae_model() supports families that brms recognises by
# name (Gaussian, Beta, Binomial, ...), this function lets users plug in a
# fully custom likelihood -- a `brms::custom_family` object accompanied by
# stanvars defining the Stan-side log density.  The neodistr package follows
# exactly the same pattern for neo-normal distributions; this bridge means
# hbsaems users can register any neodistr (or self-defined) family with one
# call.
# =============================================================================


#' Register a brms Custom Family with the hbsaems Model Registry
#'
#' Wraps a \code{brms::custom_family} + \code{stanvars} pair into a model
#' spec usable by \code{\link{hbm}} and the flexible factory
#' \code{\link{hbm_flex}}.  The custom likelihood is then available
#' throughout the package -- in \code{\link{run_sae_app}}, in
#' \code{\link{sae_predict}}, in the Shiny application, and so on -- as if it
#' were a built-in family.
#'
#' @param key Character.  Unique registry key (e.g.\ \code{"loglogistic"}).
#' @param custom_family A \code{brms::custom_family} object describing the
#'   parameters, links, and constraints of the distribution.  Typically
#'   produced by \code{brms::custom_family()} or by a helper such as
#'   \code{\link{brms_custom_loglogistic}}.
#' @param stanvars A \code{brms::stanvars} object containing the Stan-side
#'   function definitions (log-PDF / log-CDF / RNG).
#' @param response_check Optional function \code{function(y) -> logical}
#'   for response-domain validation (e.g.\ positivity check for the
#'   loglogistic).
#' @param response_check_msg Character.  Error message if
#'   \code{response_check} fails.
#' @param supports_mi Logical.  Whether \code{brms::mi()} can impute the
#'   response under this likelihood (default \code{FALSE}; custom families
#'   typically do not support \code{mi()}).
#' @param discrete Logical.  Whether the family is discrete (default
#'   \code{FALSE}).
#' @param overwrite Logical.  If \code{TRUE}, replace an existing entry with
#'   the same \code{key}.
#'
#' @return Invisibly returns the registered model spec (a list).
#'
#' @details
#' After registration, the family is usable in any of the following ways:
#'
#' \preformatted{
#'   # Direct via the registry key
#'   fit <- hbm_flex("loglogistic", response = "y",
#'                    predictors = c("x1", "x2"),
#'                    data = d, re = ~ (1 | area))
#'
#'   # Direct via hbm() (canonical):
#'   fit <- hbm(brms::bf(y ~ x1 + x2 + (1 | area)),
#'              data = d,
#'              hb_sampling = "loglogistic")
#' }
#'
#' Internally, \code{\link{hbm}} detects that the registered family is a
#' \code{brms::custom_family} and (i) passes the family object directly to
#' \code{brms::brm()} instead of constructing a built-in
#' \code{brms::brmsfamily()}, and (ii) merges the family's stanvars with
#' any user-supplied \code{stanvars}.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#'
#' # Loglogistic (built in to hbsaems v0.6.0; this just shows the registration mechanism)
#' ll <- brms_custom_loglogistic()
#' register_hbsae_brms_custom(
#'   key             = "loglogistic_user",
#'   custom_family   = ll$custom_family,
#'   stanvars        = ll$stanvars_family,
#'   response_check  = function(y) all(y > 0, na.rm = TRUE),
#'   response_check_msg = "Loglogistic response must be positive."
#' )
#' "loglogistic_user" %in% list_hbsae_models()
#' }
#'
#' @seealso \code{\link{register_hbsae_model}},
#'   \code{\link{brms_custom_loglogistic}},
#'   \code{\link{brms_custom_shifted_loglogistic}},
#'   \href{https://paulbuerkner.com/brms/articles/brms_customfamilies.html}{brms vignette on custom families}.
#' @export
register_hbsae_brms_custom <- function(key,
                                       custom_family,
                                       stanvars,
                                       response_check     = NULL,
                                       response_check_msg = NULL,
                                       supports_mi        = FALSE,
                                       discrete           = FALSE,
                                       overwrite          = FALSE) {

  # ---- Input validation ----------------------------------------------------
  stopifnot(is.character(key), length(key) == 1L, nzchar(key))
  if (!inherits(custom_family, "customfamily"))
    stop("`custom_family` must be a brms::custom_family() object.",
         call. = FALSE)
  if (!inherits(stanvars, "stanvars"))
    stop("`stanvars` must be a brms::stanvars object.", call. = FALSE)

  # ---- Build the model spec ------------------------------------------------
  # We store the custom_family object IN the spec under `$family` so the
  # downstream code (.resolve_family_object() in hbm.R) can use it directly.
  spec <- list(
    family             = custom_family,        # the customfamily object
    link               = custom_family$link    %||% "identity",
    discrete           = discrete,
    supports_mi        = supports_mi,
    has_addition_term  = FALSE,
    addition_template  = NULL,
    response_check     = response_check,
    response_check_msg = response_check_msg,
    default_priors     = function(...) NULL,
    aux_param_hyperprior = NULL,
    # Flag for downstream code; .is_custom_family() helper checks this.
    custom_family      = custom_family,
    custom_stanvars    = stanvars
  )

  # ---- Conflict check ------------------------------------------------------
  if (exists(key, envir = .hbsae_model_env, inherits = FALSE) &&
      !overwrite)
    stop("Model '", key, "' is already registered. ",
         "Pass overwrite = TRUE to replace it.", call. = FALSE)

  assign(key, spec, envir = .hbsae_model_env)
  invisible(spec)
}


# ---- Internal helper: identify a custom-family spec ------------------------

# Returns TRUE if the spec was registered via register_hbsae_brms_custom().
# Used by hbm() to switch between brms::brmsfamily() and the custom_family
# object when assembling the brm() call.
.is_custom_family_spec <- function(spec) {
  is.list(spec) &&
    !is.null(spec$custom_family) &&
    inherits(spec$custom_family, "customfamily")
}
