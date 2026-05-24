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
#       sampled with brms's own default prior:
#         phi ~ gamma(0.01, 0.01)        (weakly informative, lower bound 0)
#       As of v1.0.0 the pre-existing hierarchical construction
#         phi   ~ gamma(alpha, beta)
#         alpha ~ gamma(1, 1)
#         beta  ~ gamma(1, 1)
#       has been removed because the prior on alpha (real<lower=1>) was
#       on the boundary of its support, producing divergent transitions
#       on weakly-informative data.  Users who need that construction
#       can still build it manually via the `stanvars` + `prior`
#       arguments (see the migration note in ?hbm_betalogitnorm).
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
#' design effect (\code{n} + \code{deff}) or sampled with brms's default
#' weakly-informative prior \eqn{\phi \sim \mathrm{Gamma}(0.01, 0.01)}.
#'
#' @section Migration note (v1.0.0):
#' Earlier versions of this wrapper introduced a hierarchical
#' construction \eqn{\phi \sim \mathrm{Gamma}(\alpha, \beta),\,
#' \alpha \sim \mathrm{Gamma}(1,1),\, \beta \sim \mathrm{Gamma}(1,1)}
#' for the random-phi mode, declaring \code{alpha} and \code{beta} as
#' Stan parameters with hyperpriors injected via \code{stanvars}.
#' Starting v1.0.0, that construction has been removed in favour of
#' brms's own default prior, \eqn{\phi \sim \mathrm{Gamma}(0.01, 0.01)}
#' with lower bound 0 (mean 1, variance 100; weakly informative on
#' the precision scale).  Three reasons:
#' \itemize{
#'   \item \strong{Brittle priors on alpha/beta.}  The hyperprior
#'         \eqn{\mathrm{Gamma}(1,1)} on \eqn{\alpha} has prior mean 1,
#'         which is on the boundary of the parameter space declared
#'         as \code{real<lower=1>}.  This routinely produced divergent
#'         transitions when the data were not informative about phi.
#'   \item \strong{Parameter blow-up.}  Estimating two extra Stan
#'         parameters per area-level model with limited data inflated
#'         the effective posterior dimension and slowed convergence.
#'   \item \strong{Cleaner brms semantics.}  Letting brms apply its own
#'         default \eqn{\mathrm{Gamma}(0.01, 0.01)} means that passing
#'         \code{prior = NULL} now does exactly what the user expects:
#'         \dQuote{brms defaults}.  No surprises.
#' }
#' \strong{If you need to reproduce the old behaviour}, supply
#' \code{stanvars} yourself to declare \code{alpha}, \code{beta} and
#' their hyperpriors, and pass \code{prior = set_prior("gamma(alpha,
#' beta)", class = "phi")}.  Pre-v1.0.0 code that supplied
#' \code{stanvars} with hyperpriors on \code{alpha}/\code{beta}
#' will now raise an informative error.
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
#'   (treats \code{phi} as random with brms's default prior).
#' @param deff Character or \code{NULL}.  Name of the design-effect
#'   column.  Required when \code{n} is supplied (and vice versa).
#' @param area_var Character vector or \code{NULL}.  Name(s) of the
#'   area-grouping column(s).  Length 1 adds an IID random intercept
#'   \code{(1 | area_var)}; length \eqn{\geq} 2 supports hierarchical
#'   areas (e.g.\ \code{c("province", "regency")}) -- see
#'   \code{?hbm_flex} for the nested vs.\ crossed structures.
#' @param area_re_structure Either \code{"nested"} (default) or
#'   \code{"crossed"}; controls how multiple area columns combine.
#' @param spatial_var,spatial_model,car_type,sar_type,M Spatial
#'   random-effect arguments, forwarded to \code{\link{hbm}}.
#' @param group \strong{Deprecated.}  Use \code{area_var} instead.
#' @param sre \strong{Deprecated.}  Use \code{spatial_var} instead.
#' @param sre_type \strong{Deprecated.}  Use \code{spatial_model} instead.
#' @param link_phi Character or \code{NULL}.  Link function for
#'   \code{phi}.  \strong{Default \code{NULL} resolves automatically}:
#'   \code{"identity"} when \code{phi} is pinned via the survey design
#'   (the \code{n + deff} sugar or \code{fixed_params$phi}, both of
#'   which produce raw positive offsets) and \code{"log"} otherwise
#'   (the brms default for \code{phi} in the Beta family; keeps
#'   \eqn{\phi > 0} while letting NUTS sample on \eqn{\mathbb{R}}).
#'   Manually setting \code{"identity"} together with an estimated
#'   \code{phi} can let NUTS propose negative values and trigger
#'   divergent transitions; an informative warning is emitted in that
#'   case.
#' @param prior Optional \code{brmsprior} object.  If \code{NULL},
#'   sensible defaults are filled in:
#'   \itemize{
#'     \item \code{Intercept ~ student_t(4, 0, 10)}
#'     \item \code{b ~ student_t(4, 0, 2.5)}
#'     \item \code{phi} -- brms default \code{gamma(0.01, 0.01)} (in
#'       random mode; ignored in fixed mode).
#'   }
#'   The user may pass a partial prior: missing default classes are
#'   filled in automatically.  To put a custom prior on \code{phi}
#'   (random mode), set \code{prior = set_prior("gamma(2, 0.5)",
#'   class = "phi")} or similar.
#' @param stanvars Optional \code{brmsstanvars} object for power users
#'   who need to inject custom Stan code blocks (e.g.\ transformed
#'   data, generated quantities, model-block statements not expressible
#'   via the \code{prior} argument).  Passed through to brms verbatim.
#'   \strong{Note:} As of v1.0.0, this wrapper no longer declares
#'   \code{alpha} or \code{beta} as Stan parameters, so legacy
#'   \code{stanvars} blocks containing sampling statements on
#'   \code{alpha} / \code{beta} (left over from the pre-v1.0.0
#'   hierarchical-phi construction) will now raise an informative
#'   error.
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
#'   area_var   = "regency",
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
#'   area_var   = "regency",
#'   data       = data,
#'   chains = 1, iter = 500, warmup = 250, refresh = 0
#' )
#' summary(model2)
#'
#' # -- 3. Custom prior on phi via the standard `prior` argument ----------------
#' #
#' # Starting v1.0.0, hbm_betalogitnorm() no longer declares its own
#' # alpha / beta hyperparameters; phi uses brms's native default
#' # Gamma(0.01, 0.01).  To override that prior, pass a `brms::set_prior()`
#' # entry to the `prior` argument -- the legacy
#' # stanvar("alpha ~ gamma(...); beta ~ gamma(...)") pattern would now
#' # error because alpha/beta are no longer Stan parameters.
#' model3 <- hbm_betalogitnorm(
#'   response   = "y",
#'   auxiliary  = c("x1", "x2", "x3"),
#'   area_var   = "regency",
#'   data       = data,
#'   prior      = brms::set_prior("gamma(2, 0.5)", class = "phi"),
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
#'   spatial_var = "province",
#'   spatial_model   = "car",
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
                              area_var       = NULL,
                              area_re_structure = c("nested", "crossed"),
                              spatial_var    = NULL,
                              spatial_model  = NULL,
                              car_type       = NULL,
                              sar_type       = NULL,
                              M              = NULL,
                              link_phi       = NULL,
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
                              fixed_params   = NULL,    # pinned dist parameters
                              # -- DEPRECATED aliases (v1.0.0) ----
                              predictors     = NULL,
                              group          = NULL,
                              sre            = NULL,
                              sre_type       = NULL,
                              ...) {

  # -- 0. Deprecated alias 'predictors' (v1.0.0) ----------------------------
  if (!is.null(predictors)) {
    if (!is.null(auxiliary))
      stop("Pass either `auxiliary` (preferred) or `predictors` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("predictors", "auxiliary", "v2.0.0")
    auxiliary <- predictors
  }
  if (is.null(auxiliary))
    stop("`auxiliary` (auxiliary variables) is required.", call. = FALSE)

  # -- 0b. Deprecated aliases: group -> area_var,
  #         sre -> spatial_var, sre_type -> spatial_model
  if (!is.null(group)) {
    if (!is.null(area_var))
      stop("Pass either `area_var` (preferred) or `group` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("group", "area_var", "v2.0.0")
    area_var <- group
  }
  if (!is.null(sre)) {
    if (!is.null(spatial_var))
      stop("Pass either `spatial_var` (preferred) or `sre` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("sre", "spatial_var", "v2.0.0")
    spatial_var <- sre
  }
  if (!is.null(sre_type)) {
    if (!is.null(spatial_model))
      stop("Pass either `spatial_model` (preferred) or `sre_type` ",
           "(deprecated), but not both.", call. = FALSE)
    .deprecate_arg("sre_type", "spatial_model", "v2.0.0")
    spatial_model <- sre_type
  }


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
  area_re_structure <- match.arg(area_re_structure)
  .check_area_var_columns(area_var, data)

  # Validate spatial_var, if supplied, refers to an actual column.  Without
  # this guard, the error only surfaces inside brms with a less-friendly
  # message ("variables can neither be found in 'data' nor in 'data2'").
  if (!is.null(spatial_var) && !(spatial_var %in% names(data)))
    stop(sprintf("Spatial variable '%s' not found in `data`.", spatial_var),
         call. = FALSE)

  # Response support: strictly in (0, 1)
  y <- data[[response]]
  if (any(y <= 0 | y >= 1, na.rm = TRUE))
    stop("Response variable must lie strictly in (0, 1) for Beta likelihood.",
         call. = FALSE)

  # -- 2. Build fixed_params for phi (if survey-design mode) ------------------
  # Mode (1): user supplied n + deff -> compute phi_fixed and merge into
  # the (possibly already-populated) fixed_params list.  If user has also
  # supplied fixed_params$phi directly, that is a conflict.  Validation
  # and translation live in the centralised helper
  # `.translate_n_deff_to_phi()`.
  if (is.null(fixed_params)) fixed_params <- list()
  if (!is.list(fixed_params))
    stop("`fixed_params` must be a named list (or NULL).", call. = FALSE)

  fixed_params <- .translate_n_deff_to_phi(
    fixed_params = fixed_params,
    n            = n,
    deff         = deff,
    data         = data
  )

  # ----------------------------------------------------------------------
  # (v1.0.0): Resolve link_phi conditional on the mode.
  #
  # Two modes are supported:
  #
  #   (a) FIXED mode  -- phi is pinned via offset (sugar `n + deff`, or
  #                       direct `fixed_params$phi`).  The offset must
  #                       be on the SAME SCALE as the linear predictor
  #                       for phi.  Since the offset values are the raw
  #                       precision `n/deff - 1` (strictly positive,
  #                       no log involved), we need `link_phi = "identity"`.
  #
  #   (b) RANDOM mode -- phi is unconstrained and given a `gamma(alpha, beta)`
  #                       hyperprior.  brms's default link for `phi` is
  #                       "log", which keeps phi positive without manual
  #                       constraints.  Forcing "identity" here would
  #                       allow NUTS to propose negative `phi`, triggering
  #                       `log(0)` likelihood evaluations and divergent
  #                       transitions.  We therefore default to "log".
  #
  # The user can still override both via the explicit argument.
  # ----------------------------------------------------------------------
  if (is.null(link_phi)) {
    if ("phi" %in% names(fixed_params)) {
      link_phi <- "identity"   # fixed-design / Liu (2009) mode
    } else {
      link_phi <- "log"        # random / hyperprior mode -- brms default
    }
  } else {
    # User-supplied: warn if it looks unsafe for the active mode.
    if (link_phi == "identity" && !"phi" %in% names(fixed_params))
      warning(
        "`link_phi = \"identity\"` is being applied with `phi` ESTIMATED ",
        "(random mode).  The NUTS sampler may propose negative phi and ",
        "produce divergent transitions.  Consider `link_phi = \"log\"` ",
        "instead, or supply `n` + `deff` to pin phi via the survey ",
        "design.", call. = FALSE
      )
  }

  # -- 3. Stanvars (random mode + fixed mode share the same plumbing now) ----
  # In v1.0.0+, the random-phi mode uses brms's own default prior
  # `phi ~ gamma(0.01, 0.01)` instead of the earlier hierarchical
  # `phi ~ gamma(alpha, beta), alpha ~ gamma(1,1), beta ~ gamma(1,1)`
  # construction.  Consequently this wrapper no longer declares custom
  # Stan parameters (alpha, beta) or injects default hyperprior sampling
  # statements -- there is nothing for the wrapper to add.
  #
  # If the user supplies their own `stanvars` (for power-user Stan code
  # blocks, e.g. transformed data, generated quantities), we still pass
  # it through verbatim.  In fixed-phi mode we also guard against
  # legacy code that supplied alpha/beta hyperpriors: those statements
  # now refer to undeclared Stan parameters and would fail to compile.
  if (length(fixed_params) > 0L && !is.null(stanvars)) {
    legacy_hyperprior_vars <- intersect(
      .extract_model_block_vars(stanvars),
      c("alpha", "beta")
    )
    if (length(legacy_hyperprior_vars) > 0L) {
      stop(
        "`stanvars` contains a sampling statement targeting `",
        paste(legacy_hyperprior_vars, collapse = "`, `"),
        "`, but `phi` is pinned via `n` + `deff` (or `fixed_params$phi`). ",
        "These statements are also a leftover of the pre-v1.0.0 hierarchical ",
        "phi ~ gamma(alpha, beta) construction, which has been replaced by ",
        "brms's default `phi ~ gamma(0.01, 0.01)`.  Remove the stanvars ",
        "block, or remove `n`+`deff`/`fixed_params$phi`.",
        call. = FALSE
      )
    }
  } else if (length(fixed_params) == 0L && !is.null(stanvars)) {
    # Random-phi mode: warn (don't error) if user is still passing legacy
    # alpha/beta hyperpriors -- those Stan parameters no longer exist,
    # so the stanvars would not compile.  Better to fail loudly here.
    legacy_hyperprior_vars <- intersect(
      .extract_model_block_vars(stanvars),
      c("alpha", "beta")
    )
    if (length(legacy_hyperprior_vars) > 0L) {
      stop(
        "`stanvars` contains a sampling statement on `",
        paste(legacy_hyperprior_vars, collapse = "`, `"),
        "`, but this convenience wrapper no longer declares those as ",
        "Stan parameters.  As of v1.0.0, `phi` uses brms's default ",
        "prior `gamma(0.01, 0.01)`.  To use a different prior, supply ",
        "it via the `prior` argument, e.g. ",
        "`prior = brms::set_prior(\"gamma(2, 0.5)\", class = \"phi\")`.",
        call. = FALSE
      )
    }
  }
  stanvars_final <- stanvars

  # -- 4. Merge user-supplied prior with sensible Beta defaults ---------------
  adjusted_prior <- .merge_betalogitnorm_priors(
    user_prior   = prior,
    phi_is_fixed = length(fixed_params) > 0L
  )

  # -- 5. Build random-effect formula -----------------------------------------
  re_formula <- .build_area_re_formula(area_var, structure = area_re_structure)

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
    spatial_var    = spatial_var,
    spatial_model  = spatial_model,
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

  # Hbsaems-curated defaults for the regression coefficients.  We do NOT
  # add an explicit prior on `phi` in random mode any more; instead we
  # let brms apply its own default `gamma(0.01, 0.01)` (weakly informative,
  # mean 1, variance 100) via `brms::get_prior(family = Beta())`.  This
  # matches the user's reasonable expectation that "no explicit prior" =
  # "brms default".  The pre-v1.0.0 \eqn{\phi \sim \mathrm{Gamma}(\alpha,
  # \beta)} construction with hierarchical alpha/beta hyperpriors is gone:
  # see NEWS for the migration note.
  defaults <- c(
    brms::set_prior("student_t(4, 0, 10)",  class = "Intercept"),
    brms::set_prior("student_t(4, 0, 2.5)", class = "b")
  )

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
  # `phi`: rely on brms default unless the user supplied an explicit prior;
  # we don't add anything ourselves.
  out
}
