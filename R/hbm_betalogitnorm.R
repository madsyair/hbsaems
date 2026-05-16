# R/hbm_betalogitnorm.R
# =============================================================================
# Convenience wrapper for SAE under a Beta likelihood with a logit-normal
# link.  Supports two modes for the precision parameter `phi`:
#
#   (1) FIXED mode  -- user supplies `n` (sample size) and `deff` (design
#       effect); phi is then computed deterministically as
#         phi_i = n_i / deff_i - 1
#       and attached to the model via an offset on the phi linear predictor.
#       Standard practice for SAE with known survey design (Liu 2009;
#       Rao & Molina 2015).
#
#   (2) RANDOM mode -- if neither `n` nor `deff` is supplied, `phi` is
#       given a hierarchical hyperprior:
#         phi   ~ gamma(alpha, beta)
#         alpha ~ gamma(1, 1)              (default; overridable via stanvars)
#         beta  ~ gamma(1, 1)              (default; overridable via stanvars)
#       This matches the pattern of the original GitHub hbsaems package.
#
# Both modes also feed the generic `fixed_params` machinery in hbm() so
# that power users (and custom-distribution authors) can pin `phi`
# directly without the `n` + `deff` sugar.
# =============================================================================


#' Small Area Estimation Under a Beta Likelihood (Logit-Normal Link)
#'
#' Convenience wrapper around \code{\link{hbm}} for SAE problems where
#' the response \eqn{y_i \in (0, 1)} is modelled as
#' \deqn{y_i \mid \theta_i, \phi_i \sim \mathrm{Beta}(\theta_i \phi_i,
#'        (1 - \theta_i)\phi_i),}
#' \deqn{\mathrm{logit}(\theta_i) = x_i^\top \boldsymbol{\beta} + u_i,
#'        \quad u_i \sim \mathcal{N}(0, \sigma_u^2).}
#'
#' The precision parameter \eqn{\phi_i} can either be pinned to a survey
#' design effect (\code{n} + \code{deff}) or sampled with a hierarchical
#' hyperprior (\eqn{\phi \sim \mathrm{Gamma}(\alpha, \beta)},
#' \eqn{\alpha \sim \mathrm{Gamma}(1,1)}, \eqn{\beta \sim \mathrm{Gamma}(1,1)}
#' by default).
#'
#' @param response Character. Name of the response column (must lie strictly
#'   between 0 and 1).
#' @param auxiliary Character vector of auxiliary (fixed-effect) variable
#'   names; corresponds to area-level covariates in SAE literature
#'   (see Rao & Molina 2015 Ch. 4).
#' @param predictors \strong{Deprecated.}  Use \code{auxiliary} instead.
#'   Kept for backward compatibility; will be removed in v2.0.0.
#' @param fixed_params Optional named list pinning distributional
#'   parameters to known values.  See \code{\link{hbm}} for the spec
#'   format.  Allows power-user access to the same machinery used by
#'   the \code{n}/\code{deff} arguments.
#' @param data A data frame.
#' @param n Character or \code{NULL}.  Name of the column giving the
#'   per-area sample size used to compute the fixed \eqn{\phi}.  Must be
#'   supplied together with \code{deff}.  When supplied, \eqn{\phi_i =
#'   n_i / \text{deff}_i - 1} is pinned via offset.  Default: \code{NULL}
#'   (treats \code{phi} as random with hyperprior).
#' @param deff Character or \code{NULL}.  Name of the design-effect
#'   column.  Required when \code{n} is supplied (and vice versa).
#' @param group Character or \code{NULL}.  Name of the area-grouping
#'   variable; if supplied, adds \code{(1 | group)} as a random
#'   intercept.
#' @param sre,sre_type,car_type,sar_type,M Spatial random-effect arguments,
#'   forwarded to \code{\link{hbm}}.
#' @param link_phi Character. Link function for \code{phi}; default
#'   \code{"identity"} when \code{phi} is pinned (offset must be on the
#'   identity scale).
#' @param prior Optional \code{brmsprior} object.  If \code{NULL}, sensible
#'   defaults are filled in:
#'   \itemize{
#'     \item \code{Intercept ~ student_t(4, 0, 10)}
#'     \item \code{b ~ student_t(4, 0, 2.5)}
#'     \item \code{phi ~ gamma(alpha, beta)} (random mode only)
#'   }
#'   The user may pass a partial prior: missing default classes are
#'   filled in automatically.
#' @param stanvars Optional \code{brmsstanvars} object specifying
#'   hyperpriors for \code{alpha} and \code{beta} when \code{phi} is
#'   random.  If \code{NULL} (default), \code{alpha ~ gamma(1, 1)} and
#'   \code{beta ~ gamma(1, 1)} are used.  The user may also supply
#'   custom \code{stanvar()} expressions to override either or both
#'   hyperpriors, e.g.:
#'   \preformatted{
#'   stanvars = stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
#'              stanvar(scode = "beta  ~ gamma(2, 3);", block = "model")
#'   }
#' @param fixed_params Optional named list pinning distributional
#'   parameters to known values.  See \code{\link{hbm}} for the spec
#'   format.  Conflicts with \code{n} + \code{deff} on the \code{phi}
#'   parameter; use one or the other.
#' @param predictors \strong{Deprecated.}  Use \code{auxiliary} instead.
#'   Kept for backward compatibility; will be removed in v2.0.0.
#' @param handle_missing,m,control,chains,iter,warmup,cores,sample_prior,...
#'   Passed through to \code{\link{hbm}}.
#'
#' @section Conflict policy:
#' When the precision parameter \eqn{\phi} is pinned via \code{n} +
#' \code{deff} (or via \code{fixed_params$phi}), the function refuses
#' any additional specification that would also set \eqn{\phi}.
#' Specifically, all of the following are rejected with an informative
#' error at construction time:
#' \itemize{
#'   \item \code{n} supplied without \code{deff}, or vice versa.
#'   \item \code{n} + \code{deff} \emph{and} \code{fixed_params$phi}.
#'   \item \code{n} + \code{deff} \emph{and} a user \code{prior} on
#'         \code{class = "phi"}.
#'   \item \code{n} + \code{deff} \emph{and} a \code{stanvars}
#'         hyperprior on \code{alpha} or \code{beta} (the
#'         \eqn{\mathrm{Gamma}(\alpha, \beta)} hyperparameters used in
#'         random-\eqn{\phi} mode).
#'   \item \code{auxiliary} \emph{and} the deprecated \code{predictors}
#'         in the same call.
#' }
#'
#' @return An object of class \code{hbmfit}.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_betalogitnorm")
#' data <- data_betalogitnorm
#'
#' # -- 1. Basic model (phi random, with default hyperprior) --------------------
#' model1 <- hbm_betalogitnorm(
#'   response   = "y",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   group      = "group",
#'   data       = data,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' summary(model1)
#'
#' # -- 2. Fixed phi via survey design (n + deff) -------------------------------
#' model2 <- hbm_betalogitnorm(
#'   response   = "y",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   n          = "n",
#'   deff       = "deff",
#'   group      = "group",
#'   data       = data,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' summary(model2)
#'
#' # -- 3. Custom hyperprior on alpha, beta -------------------------------------
#' model3 <- hbm_betalogitnorm(
#'   response   = "y",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   group      = "group",
#'   data       = data,
#'   stanvars   = stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
#'                stanvar(scode = "beta  ~ gamma(2, 3);", block = "model"),
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#'
#' # -- 4. Spatial CAR model ----------------------------------------------------
#' data("adjacency_matrix_car")
#' model4 <- hbm_betalogitnorm(
#'   response   = "y",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   n          = "n",
#'   deff       = "deff",
#'   sre        = "sre",
#'   sre_type   = "car",
#'   M          = adjacency_matrix_car,
#'   data       = data,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' }
#'
#' @references
#' Liu, B. (2009). \emph{Hierarchical Bayes Estimation and Empirical Best
#' Prediction of Small-Area Proportions}. University of Maryland.
#'
#' Rao, J. N. K., & Molina, I. (2015). \emph{Small Area Estimation}, 2nd ed.
#' Wiley, p. 390.
#'
#' @seealso \code{\link{hbm_flex}}, \code{\link{hbm}}
#' @export
hbm_betalogitnorm <- function(response,
                              auxiliary      = NULL,
                              data,
                              n              = NULL,
                              deff           = NULL,
                              group          = NULL,
                              sre            = NULL,
                              sre_type       = NULL,
                              car_type       = NULL,
                              sar_type       = NULL,
                              M              = NULL,
                              link_phi       = "identity",
                              prior          = NULL,
                              stanvars       = NULL,
                              handle_missing = NULL,
                              m              = 5L,
                              control        = list(),
                              chains         = 4L,
                              iter           = 4000L,
                              warmup         = floor(iter / 2),
                              cores          = 1L,
                              sample_prior   = "no",
                              fixed_params   = NULL,    # v0.6.1: pinned dist parameters
                              predictors     = NULL,    # DEPRECATED in v0.6.1
                              ...) {

  # -- 0. Deprecated alias 'predictors' (v0.6.1) ----------------------------
  if (!is.null(predictors)) {
    if (!is.null(auxiliary))
      stop("Pass either `auxiliary` (preferred) or `predictors` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("predictors", "auxiliary", "v2.0.0")
    auxiliary <- predictors
  }
  if (is.null(auxiliary))
    stop("`auxiliary` (auxiliary variables) is required.", call. = FALSE)


  # -- 1. Input validation -----------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (!response %in% names(data))
    stop(sprintf("Response variable '%s' not found in `data`.", response),
         call. = FALSE)
  missing_aux <- setdiff(auxiliary, names(data))
  if (length(missing_aux) > 0L)
    stop("Predictor(s) not found in `data`: ",
         paste(missing_aux, collapse = ", "), call. = FALSE)

  # n + deff must be supplied together or not at all
  if (xor(is.null(n), is.null(deff)))
    stop("Both `n` and `deff` must be supplied together, or both NULL.",
         call. = FALSE)
  if (!is.null(n) && !(n    %in% names(data)))
    stop(sprintf("`n = \"%s\"` is not a column in `data`.", n),
         call. = FALSE)
  if (!is.null(deff) && !(deff %in% names(data)))
    stop(sprintf("`deff = \"%s\"` is not a column in `data`.", deff),
         call. = FALSE)
  if (!is.null(group) && !(group %in% names(data)))
    stop(sprintf("`group = \"%s\"` is not a column in `data`.", group),
         call. = FALSE)

  # Response support: strictly in (0, 1)
  y <- data[[response]]
  if (any(y <= 0 | y >= 1, na.rm = TRUE))
    stop("Response variable must lie strictly in (0, 1) for Beta likelihood.",
         call. = FALSE)

  # -- 2. Build fixed_params for phi (if survey-design mode) ------------------
  # Mode (1): user supplied n + deff -> compute phi_fixed and merge into
  # the (possibly already-populated) fixed_params list.  If user has also
  # supplied fixed_params$phi directly, that is a conflict.
  if (is.null(fixed_params)) fixed_params <- list()
  if (!is.list(fixed_params))
    stop("`fixed_params` must be a named list (or NULL).", call. = FALSE)

  if (!is.null(n) && !is.null(deff)) {
    if ("phi" %in% names(fixed_params))
      stop("Cannot supply both `n` + `deff` (which pin phi via survey ",
           "design) and `fixed_params$phi`.  Pick one.",
           call. = FALSE)
    nvec    <- data[[n]]
    deffvec <- data[[deff]]
    if (anyNA(nvec) || anyNA(deffvec))
      stop("Missing values detected in `n` or `deff`; cannot fix phi.",
           call. = FALSE)
    if (any(nvec <= 0 | deffvec <= 0))
      stop("`n` and `deff` must be strictly positive.", call. = FALSE)
    phi_vec <- nvec / deffvec - 1
    if (any(phi_vec <= 0))
      stop("Computed phi = n/deff - 1 contains non-positive values; ",
           "check `n` and `deff`.", call. = FALSE)
    fixed_params$phi <- phi_vec
  }

  # -- 3. Build stanvars for the alpha / beta hyperpriors (random mode only) --
  # In random mode, phi ~ gamma(alpha, beta) where alpha, beta are themselves
  # given gamma(1, 1) priors by default.  Users may override one or both
  # via the `stanvars` argument.
  if (length(fixed_params) == 0L) {
    # Declare alpha, beta as Stan parameters
    parameters_block <- brms::stanvar(
      scode = paste0(
        "  real<lower=1> alpha;\n",
        "  real<lower=0> beta;\n"
      ),
      block = "parameters"
    )
    # Detect which of alpha/beta the user has already specified a hyperprior for
    existing_vars <- .extract_model_block_vars(stanvars)
    to_add <- list()
    if (!"alpha" %in% existing_vars) {
      to_add[[length(to_add) + 1L]] <- brms::stanvar(
        scode = "  alpha ~ gamma(1, 1);", block = "model"
      )
    }
    if (!"beta" %in% existing_vars) {
      to_add[[length(to_add) + 1L]] <- brms::stanvar(
        scode = "  beta  ~ gamma(1, 1);", block = "model"
      )
    }
    default_model_block <- if (length(to_add) > 0L)
      Reduce(`+`, to_add)
    else
      NULL

    # Combine: declared parameters + (user stanvars OR default) + missing default
    combined <- parameters_block
    if (!is.null(stanvars))            combined <- combined + stanvars
    if (!is.null(default_model_block)) combined <- combined + default_model_block
    stanvars_final <- combined
  } else {
    # Fixed mode: alpha/beta hyperprior machinery is irrelevant.
    # If the user has supplied stanvars containing hyperpriors for alpha/beta,
    # those statements would refer to undeclared Stan parameters and fail at
    # compile time -- refuse early with a clear error.
    if (!is.null(stanvars)) {
      hyperprior_vars <- intersect(
        .extract_model_block_vars(stanvars),
        c("alpha", "beta")
      )
      if (length(hyperprior_vars) > 0L) {
        stop(
          "`stanvars` contains a hyperprior on `",
          paste(hyperprior_vars, collapse = "`, `"),
          "`, but `phi` is pinned via `n` + `deff` (or `fixed_params$phi`). ",
          "Hyperpriors on alpha/beta are only meaningful when `phi` is ",
          "sampled.  Remove the stanvars block, or remove `n`+`deff`/",
          "`fixed_params$phi` to let phi be sampled.",
          call. = FALSE
        )
      }
    }
    stanvars_final <- stanvars
  }

  # -- 4. Merge user-supplied prior with sensible Beta defaults ---------------
  adjusted_prior <- .merge_betalogitnorm_priors(
    user_prior   = prior,
    phi_is_fixed = length(fixed_params) > 0L
  )

  # -- 5. Build random-effect formula -----------------------------------------
  re_formula <- if (!is.null(group))
    stats::as.formula(paste0("~ (1 | ", group, ")"))
  else
    NULL

  # -- 6. Hand off to hbm() ----------------------------------------------------
  fixed_effects   <- paste(auxiliary, collapse = " + ")
  main_formula    <- brms::bf(stats::as.formula(
    paste0(response, " ~ ", fixed_effects)
  ))

  fit <- hbm(
    formula        = main_formula,
    hb_sampling    = "Beta",
    hb_link        = "logit",
    link_phi       = link_phi,
    re             = re_formula,
    sre            = sre,
    sre_type       = sre_type,
    car_type       = car_type,
    sar_type       = sar_type,
    M              = M,
    data           = data,
    prior          = adjusted_prior,
    fixed_params   = if (length(fixed_params) > 0L) fixed_params else NULL,
    handle_missing = handle_missing,
    m              = m,
    control        = control,
    chains         = chains,
    iter           = iter,
    warmup         = warmup,
    cores          = cores,
    sample_prior   = sample_prior,
    stanvars       = stanvars_final,
    ...
  )
  fit
}


# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

# Inspect a brmsstanvars object and return the names of variables that
# already have a sampling statement in the "model" block.  Used to detect
# whether the user has supplied a hyperprior for alpha / beta so that
# defaults are not silently added on top.
.extract_model_block_vars <- function(stanvars) {
  if (is.null(stanvars)) return(character(0))
  if (!inherits(stanvars, c("stanvars", "stanvar"))) return(character(0))
  sv_list <- if (inherits(stanvars, "stanvar")) list(stanvars) else unclass(stanvars)
  vars <- character(0)
  for (sv in sv_list) {
    if (!is.null(sv$block) && identical(sv$block, "model")) {
      m  <- regexec("\\b([A-Za-z_][A-Za-z0-9_]*)\\s*~", sv$scode)
      mm <- regmatches(sv$scode, m)[[1L]]
      if (length(mm) >= 2L) vars <- c(vars, mm[2L])
    }
  }
  unique(vars)
}

# Fill in default priors (Intercept, b, phi) for the Beta logit-normal
# model only when the user has not already specified them for the
# corresponding class.
.merge_betalogitnorm_priors <- function(user_prior, phi_is_fixed) {

  has_class <- function(p, cls) {
    if (is.null(p) || !inherits(p, "brmsprior")) return(FALSE)
    any(p$class == cls & (is.na(p$coef) | p$coef == ""))
  }

  defaults <- c(
    brms::set_prior("student_t(4, 0, 10)",  class = "Intercept"),
    brms::set_prior("student_t(4, 0, 2.5)", class = "b")
  )
  if (!phi_is_fixed) {
    defaults <- c(defaults,
                  brms::set_prior("gamma(alpha, beta)", class = "phi"))
  }

  if (is.null(user_prior)) return(defaults)

  # If user passed any prior on phi but phi is fixed, that's a contradiction
  if (phi_is_fixed && any(user_prior$class == "phi"))
    stop("Cannot supply a prior on `phi` when `n` + `deff` pin phi to ",
         "survey design.  Remove the prior or remove `n` + `deff`.",
         call. = FALSE)

  out <- user_prior
  if (!has_class(out, "Intercept"))
    out <- c(out, brms::set_prior("student_t(4, 0, 10)",  class = "Intercept"))
  if (!has_class(out, "b"))
    out <- c(out, brms::set_prior("student_t(4, 0, 2.5)", class = "b"))
  if (!phi_is_fixed && !has_class(out, "phi"))
    out <- c(out, brms::set_prior("gamma(alpha, beta)",   class = "phi"))
  out
}
