# R/stan-helpers.R
# =============================================================================
# Centralised helpers for loading Stan function definitions from disk and
# building brms::custom_family() objects on top of them.
#
# Layered design:
#
#   inst/stan/<name>.stan          -- raw Stan code (single source of truth)
#         |
#         v
#   read_stan_function(name)       -- read + return character scalar
#         |
#         v
#   build_brms_custom_family(...)  -- pair custom_family() with stanvar()
#         |
#         v
#   register_hbsae_brms_custom()   -- plug into hbsaems registry
#
# Each layer is independently usable, so adding a new distribution does not
# require touching the lower layers.
# =============================================================================


#' Read the Stan Function Code for a Custom Distribution
#'
#' Loads the contents of \code{inst/stan/<name>.stan} from the installed
#' \pkg{hbsaems} (or from the local source tree when running tests).  This is
#' a thin wrapper around \code{readLines()} that resolves the package path
#' once and validates that the file exists.
#'
#' @param name Character.  The distribution name -- must match a
#'   \code{<name>.stan} file shipped under \code{inst/stan/}.
#'
#' @return A character scalar containing the Stan code (functions block).
#'
#' @examples
#' code <- read_stan_function("loglogistic")
#' cat(code)
#'
#' @seealso \code{\link{build_brms_custom_family}},
#'   \code{\link{register_hbsae_brms_custom}}.
#' @export
read_stan_function <- function(name) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))

  fn <- system.file("stan", paste0(name, ".stan"),
                     package = "hbsaems", mustWork = FALSE)
  # Fall back to local source tree when devtools::load_all() is in use:
  # walk up from the calling file's location to find an inst/stan/ dir.
  if (!nzchar(fn) || !file.exists(fn)) {
    # Try common local paths in order of likelihood
    candidates <- c(
      file.path("inst", "stan", paste0(name, ".stan")),
      file.path("..", "inst", "stan", paste0(name, ".stan"))
    )
    hit <- candidates[file.exists(candidates)][1L]
    if (!is.na(hit)) fn <- hit
  }
  if (!nzchar(fn) || !file.exists(fn)) {
    stop("Stan file for distribution '", name, "' not found.\n",
         "  Expected at: inst/stan/", name, ".stan",
         call. = FALSE)
  }

  paste(readLines(fn, warn = FALSE), collapse = "\n")
}


#' Build a brms Custom Family + Stanvars Pair from a Single Spec
#'
#' Convenience function that combines \code{\link[brms]{custom_family}} and
#' \code{\link[brms]{stanvar}} into the standard \code{list(custom_family,
#' stanvars_family)} pair returned by every \code{brms_custom_*} helper in
#' \pkg{hbsaems}.  The Stan code is read directly from
#' \code{inst/stan/<name>.stan} -- the user does not need to maintain it as
#' an R string.
#'
#' Stan code is built once per call; if your code is reused many times,
#' wrap the returned object in a function and call it lazily.
#'
#' @param name Character.  Family name; must match a
#'   \code{<name>.stan} file under \code{inst/stan/}.
#' @param dpars Character vector of distributional parameter names.  The
#'   first MUST be \code{"mu"} (brms convention).
#' @param links Character vector of link functions, same length as
#'   \code{dpars}.  Common values: \code{"identity"}, \code{"log"},
#'   \code{"logit"}.
#' @param lb,ub Numeric vectors of lower / upper bounds; \code{NA} for none.
#' @param type \code{"real"} (continuous) or \code{"int"} (discrete).
#' @param loop Logical.  \code{FALSE} (default) selects the vectorised brms
#'   convention (Stan signatures take vectors of \code{y} and \code{mu});
#'   \code{TRUE} selects scalar Stan signatures.  The default matches the
#'   neodistr convention.  Whichever you choose, the corresponding
#'   \code{.stan} file under \code{inst/stan/} must use the same
#'   convention.
#' @param log_lik Optional function for computing observation-level
#'   log-likelihoods (used by \code{loo()}, \code{waic()}).  Signature
#'   must be \code{function(i, prep)}.  See the brms vignette
#'   "Define Custom Response Distributions" for details.
#' @param posterior_predict Optional function for drawing from the posterior
#'   predictive distribution (used by \code{pp_check()},
#'   \code{posterior_predict()}).  Signature
#'   \code{function(i, prep, ...)}.
#' @param posterior_epred Optional function returning the conditional
#'   expectation \eqn{E[Y \mid X]} (used by \code{fitted()},
#'   \code{conditional_effects()}).  Signature
#'   \code{function(prep)}.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{custom_family}}{A \code{brms::customfamily} object.}
#'   \item{\code{stanvars_family}}{A \code{brms::stanvars} object with the
#'     Stan code in the \code{functions} block.}
#' }
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#'
#' # Build the loglogistic family from inst/stan/loglogistic.stan
#' ll <- build_brms_custom_family(
#'   name  = "loglogistic",
#'   dpars = c("mu", "beta"),
#'   links = c("log", "log"),
#'   lb    = c(0,    0),
#'   ub    = c(NA,   NA),
#'   type  = "real"
#' )
#' class(ll$custom_family)
#' }
#'
#' @seealso \code{\link{read_stan_function}},
#'   \code{\link{register_hbsae_brms_custom}}.
#' @export
build_brms_custom_family <- function(name,
                                     dpars,
                                     links,
                                     lb                = NA,
                                     ub                = NA,
                                     type              = c("real", "int"),
                                     loop              = FALSE,
                                     log_lik           = NULL,
                                     posterior_predict = NULL,
                                     posterior_epred   = NULL) {

  # ---- input validation ----------------------------------------------------
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  stopifnot(is.character(dpars), length(dpars) >= 1L)
  if (dpars[1L] != "mu")
    stop("First entry of `dpars` must be \"mu\" (brms convention).",
         call. = FALSE)
  stopifnot(is.character(links), length(links) == length(dpars))
  type <- match.arg(type)
  if (length(lb) == 1L) lb <- rep(lb, length(dpars))
  if (length(ub) == 1L) ub <- rep(ub, length(dpars))
  stopifnot(length(lb) == length(dpars),
            length(ub) == length(dpars))
  # Optional function args
  if (!is.null(log_lik)           && !is.function(log_lik))
    stop("`log_lik` must be a function or NULL.",           call. = FALSE)
  if (!is.null(posterior_predict) && !is.function(posterior_predict))
    stop("`posterior_predict` must be a function or NULL.", call. = FALSE)
  if (!is.null(posterior_epred)   && !is.function(posterior_epred))
    stop("`posterior_epred` must be a function or NULL.",   call. = FALSE)

  # ---- build brms custom_family ------------------------------------------
  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package 'brms' is required to build a custom family.",
         call. = FALSE)

  # Pass post-processing functions through only if supplied -- brms's
  # custom_family() distinguishes "function NULL" (none registered) from
  # "function set to a no-op", which would break loo()/waic().
  cf_args <- list(
    name  = name,
    dpars = dpars,
    links = links,
    lb    = lb,
    ub    = ub,
    type  = type,
    loop  = loop
  )
  if (!is.null(log_lik))           cf_args$log_lik           <- log_lik
  if (!is.null(posterior_predict)) cf_args$posterior_predict <- posterior_predict
  if (!is.null(posterior_epred))   cf_args$posterior_epred   <- posterior_epred

  cf <- do.call(brms::custom_family, cf_args)

  # ---- attach Stan code from inst/stan/ ----------------------------------
  scode <- read_stan_function(name)
  sv    <- brms::stanvar(scode = scode, block = "functions")

  list(
    custom_family   = cf,
    stanvars_family = sv
  )
}
