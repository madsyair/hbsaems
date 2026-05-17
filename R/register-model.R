# R/register-model.R
# =============================================================================
# Public Function Set for extending hbsaems with custom HBSAE model types.
#
# Downstream packages, plugins, and end-users can call
# register_hbsae_model() at startup (e.g. in their .onLoad) to add a new
# model spec (likelihood family + link + auxiliary hyperpriors + ...)
# without modifying hbsaems source.
# =============================================================================


#' Register a Custom HBSAE Model
#'
#' Adds a new model spec to the \pkg{hbsaems} model registry so that it
#' can be used by \code{\link{hbm}} and the flexible factory
#' \code{\link{hbm_flex}}.  Useful for extending the package with new
#' likelihoods (e.g.\ \emph{Gamma}, \emph{Tweedie}, \emph{Skew-Normal}),
#' new link functions, or new auxiliary-parameter hyperpriors without
#' modifying \pkg{hbsaems} source.
#'
#' @param key          Character.  Unique identifier (e.g.\ \code{"gamma_log"}).
#' @param family       Character.  The \pkg{brms} family name passed to
#'   \code{hb_sampling}.
#' @param link         Character.  Default link function (default
#'   \code{"identity"}).
#' @param discrete     Logical.  Is the response discrete?  Affects whether
#'   \code{handle_missing = "model"} (joint Bayesian imputation via
#'   \code{brms::mi()}) is allowed (default \code{FALSE}).
#' @param supports_mi  Logical.  Whether \code{brms::mi()} can impute the
#'   response variable for this family (default \code{!discrete}).
#' @param has_addition_term Logical.  Whether the LHS uses an addition term
#'   such as \code{|\ trials(n)} (default \code{FALSE}).
#' @param addition_template Character.  An \code{sprintf} template used when
#'   \code{has_addition_term = TRUE}.  Must contain three \code{\%s} slots
#'   for response, addition variable, and RHS.  Example:
#'   \code{"\%s | trials(\%s) ~ \%s"}.
#' @param response_check Function \code{function(y)} returning \code{TRUE}
#'   when the response domain is valid, \code{FALSE} otherwise (default:
#'   accept anything).
#' @param response_check_msg Character.  Error message displayed when
#'   \code{response_check(y)} returns \code{FALSE}.
#' @param default_priors Function \code{function(...)} returning a
#'   \code{brmsprior} object, or \code{NULL} to use \pkg{brms} defaults.
#' @param aux_param_hyperprior Optional function
#'   \code{function(args, data)} returning a list with components
#'   \code{prior} (a \code{brmsprior}) and \code{stanvars} (a
#'   \code{\link[brms]{stanvar}} object).  Used by distributions that have
#'   an auxiliary parameter (e.g.\ \eqn{\phi} for Beta, \emph{shape} for
#'   Gamma) requiring a hyperprior expressed in raw Stan code.  The
#'   \code{args} list contains family-specific user inputs forwarded
#'   through \code{aux_args} in \code{\link{hbm_flex}}.  Return
#'   \code{NULL} to skip injection for a given call.
#' @param overwrite    Logical.  Permit overwriting an existing key
#'   (default \code{FALSE}).
#'
#' @return Invisibly returns the registered model spec (a named list).
#'
#' @details
#' After registering, you can fit a model directly with
#' \code{\link{hbm_flex}}:
#'
#' \preformatted{
#' register_hbsae_model(
#'   key            = "gamma_log",
#'   family         = "Gamma",
#'   link           = "log",
#'   discrete       = FALSE,
#'   supports_mi    = TRUE,
#'   response_check = function(y) all(y > 0, na.rm = TRUE),
#'   response_check_msg = "Gamma response must be strictly positive."
#' )
#'
#' fit <- hbm_flex(
#'   family_key = "gamma_log",
#'   response   = "expenditure",
#'   auxiliary  = c("x1", "x2"),
#'   data       = my_data
#' )
#' }
#'
#' @seealso \code{\link{hbm_flex}}, \code{\link{list_hbsae_models}}
#' @export
register_hbsae_model <- function(key,
                                  family,
                                  link               = "identity",
                                  discrete           = FALSE,
                                  supports_mi        = !discrete,
                                  has_addition_term  = FALSE,
                                  addition_template  = NULL,
                                  response_check     = function(y) TRUE,
                                  response_check_msg = NULL,
                                  default_priors     = function(...) NULL,
                                  aux_param_hyperprior = NULL,
                                  overwrite          = FALSE) {

  # -- Argument validation ---------------------------------------------------
  if (!is.character(key)        || length(key) != 1L)
    stop("'key' must be a single character string.", call. = FALSE)
  if (!is.character(family)     || length(family) != 1L)
    stop("'family' must be a single character string.", call. = FALSE)
  if (!is.character(link)       || length(link) != 1L)
    stop("'link' must be a single character string.", call. = FALSE)
  if (!is.logical(discrete)     || length(discrete) != 1L)
    stop("'discrete' must be a single logical.", call. = FALSE)
  if (!is.logical(supports_mi)  || length(supports_mi) != 1L)
    stop("'supports_mi' must be a single logical.", call. = FALSE)
  if (!is.logical(has_addition_term) || length(has_addition_term) != 1L)
    stop("'has_addition_term' must be a single logical.", call. = FALSE)
  if (has_addition_term && (is.null(addition_template) ||
                             !is.character(addition_template)))
    stop("'addition_template' is required when has_addition_term = TRUE.",
         call. = FALSE)
  if (!is.function(response_check))
    stop("'response_check' must be a function.", call. = FALSE)
  if (!is.function(default_priors))
    stop("'default_priors' must be a function.", call. = FALSE)
  if (!is.null(aux_param_hyperprior) && !is.function(aux_param_hyperprior))
    stop("'aux_param_hyperprior' must be a function or NULL.", call. = FALSE)

  # -- Conflict check --------------------------------------------------------
  if (exists(key, envir = .hbsae_model_env, inherits = FALSE) &&
      !overwrite)
    stop("Model '", key, "' is already registered. ",
         "Pass overwrite = TRUE to replace it.", call. = FALSE)

  # -- Build spec ------------------------------------------------------------
  spec <- list(
    family             = family,
    link               = link,
    discrete           = discrete,
    supports_mi        = supports_mi,
    has_addition_term  = has_addition_term,
    addition_template  = addition_template,
    response_check     = response_check,
    response_check_msg = response_check_msg,
    default_priors     = default_priors,
    aux_param_hyperprior = aux_param_hyperprior
  )

  assign(key, spec, envir = .hbsae_model_env)
  invisible(spec)
}


#' List Registered HBSAE Models
#'
#' Returns the keys of all model specs currently registered in the
#' \pkg{hbsaems} model registry.  Built-in models (\code{"gaussian"},
#' \code{"beta"}, \code{"binomial"}, \code{"lognormal"}, etc.) are always
#' included; user-registered models appear in addition.
#'
#' @return Character vector of model keys.
#' @examples
#' list_hbsae_models()
#'
#' @seealso \code{\link{register_hbsae_model}}, \code{\link{hbm_flex}}
#' @export
list_hbsae_models <- function() {
  sort(.list_models())
}


#' Inspect a Registered HBSAE Model Specification
#'
#' @param key Character.  A model key returned by
#'   \code{\link{list_hbsae_models}}.
#' @return The named list spec, or \code{NULL} if not found.
#'
#' @examples
#' get_hbsae_model("lognormal")
#'
#' @seealso \code{\link{register_hbsae_model}}
#' @export
get_hbsae_model <- function(key) {
  spec <- .get_model(key)
  if (is.null(spec))
    warning("Model '", key, "' is not registered.", call. = FALSE)
  spec
}
