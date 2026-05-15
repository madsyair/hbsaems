# R/hbm-config.R
# =============================================================================
# Optional configuration helpers to reduce hbm() parameter clutter.
#
# These are entirely OPT-IN.  Users who prefer the flat signature
# (hbm(..., chains = 4, iter = 4000, ...)) continue to work exactly as
# before.  Users who want grouped configuration can build named lists
# with hbm_control() / hbm_priors() / hbm_nonlinear() and pass them
# directly through hbm()'s `...`:
#
#   ctrl <- hbm_control(chains = 4, iter = 4000, cores = 4)
#   fit  <- hbm(formula = bf(y ~ x), data = d, ctrl)
#
# hbm() detects objects of class "hbm_config" in `...` and splices their
# contents into the formal argument list before fitting -- the same
# pattern brms::brm() uses for its extra Stan arguments.
# =============================================================================


#' Sampler Configuration for HBSAE Models
#'
#' Bundles the MCMC sampler arguments of \code{\link{hbm}} (and the
#' \code{hbm_*} family wrappers) into a single named list.  Use this when
#' you want a reusable sampler profile or to reduce the size of long
#' \code{hbm()} calls.
#'
#' This is entirely \strong{opt-in}: the flat signatures of \code{hbm()},
#' \code{hbm_lnln()}, etc.\ continue to work exactly as before.  Pass the
#' result directly to \code{hbm()} -- it is auto-spliced via
#' \code{...}.
#'
#' @param chains      Integer.  Number of Markov chains (default \code{4L}).
#' @param iter        Integer.  Total iterations per chain (default \code{4000L}).
#' @param warmup      Integer.  Warm-up iterations per chain.  Default
#'   \code{floor(iter / 2)}.
#' @param thin        Integer.  Thinning interval (default \code{1L}).
#' @param cores       Integer.  Number of cores for parallel chains
#'   (default \code{1L}).
#' @param seed        Optional integer seed for reproducibility.
#' @param refresh     Integer.  Stan progress refresh frequency
#'   (default \code{NULL}: \pkg{brms} default).
#' @param adapt_delta Numeric in \eqn{(0, 1)}.  When supplied, included
#'   in the \code{control} list as \code{control = list(adapt_delta = ...)}.
#' @param max_treedepth Integer.  Max tree depth, included in
#'   \code{control} when supplied.
#' @param control     Optional \code{list} of additional NUTS control
#'   options.  Merged with any \code{adapt_delta} / \code{max_treedepth}
#'   above.
#'
#' @return A named list whose elements are valid arguments of
#'   \code{\link{hbm}}.
#'
#' @examples
#' # Build a reusable "high-quality" profile
#' hq <- hbm_control(chains = 4, iter = 8000, cores = 4,
#'                   adapt_delta = 0.99, seed = 1)
#' str(hq)
#'
#' # Quick draft profile
#' draft <- hbm_control(chains = 2, iter = 1000)
#'
#' @seealso \code{\link{hbm_priors}}, \code{\link{hbm_nonlinear}},
#'   \code{\link{hbm}}
#' @export
hbm_control <- function(chains       = 4L,
                        iter         = 4000L,
                        warmup       = NULL,
                        thin         = 1L,
                        cores        = 1L,
                        seed         = NULL,
                        refresh      = NULL,
                        adapt_delta  = NULL,
                        max_treedepth = NULL,
                        control      = NULL) {

  # -- Validation
  stopifnot(
    is.numeric(chains), length(chains) == 1L, chains >= 1,
    is.numeric(iter),   length(iter)   == 1L, iter   >= 1,
    is.numeric(thin),   length(thin)   == 1L, thin   >= 1,
    is.numeric(cores),  length(cores)  == 1L, cores  >= 1
  )
  if (is.null(warmup)) warmup <- floor(iter / 2)
  stopifnot(warmup >= 0, warmup < iter)
  if (!is.null(adapt_delta))
    stopifnot(adapt_delta > 0, adapt_delta < 1)
  if (!is.null(max_treedepth))
    stopifnot(max_treedepth >= 1)

  # -- Merge NUTS control sub-list
  ctrl_list <- if (is.null(control)) list() else control
  if (!is.null(adapt_delta))   ctrl_list$adapt_delta   <- adapt_delta
  if (!is.null(max_treedepth)) ctrl_list$max_treedepth <- max_treedepth

  out <- list(
    chains  = as.integer(chains),
    iter    = as.integer(iter),
    warmup  = as.integer(warmup),
    thin    = as.integer(thin),
    cores   = as.integer(cores)
  )
  if (!is.null(seed))      out$seed    <- seed
  if (!is.null(refresh))   out$refresh <- refresh
  if (length(ctrl_list) > 0L) out$control <- ctrl_list
  class(out) <- c("hbm_config_control", "hbm_config", "list")
  out
}


#' Prior Configuration for HBSAE Models
#'
#' Bundles the shrinkage-prior arguments of \code{\link{hbm}} into a
#' single named list.  Same opt-in pattern as \code{\link{hbm_control}}.
#'
#' @param prior_type Character.  One of \code{"default"}, \code{"horseshoe"},
#'   \code{"r2d2"}.
#' @param prior      Optional \code{brmsprior} for explicit priors that
#'   override the registry default.
#' @param hs_df,hs_df_global,hs_df_slab,hs_scale_global,hs_scale_slab,hs_par_ratio
#'   Horseshoe-prior hyperparameters; see \code{\link{hbm}}.
#' @param r2d2_mean_R2,r2d2_prec_R2,r2d2_cons_D2 R2D2-prior
#'   hyperparameters; see \code{\link{hbm}}.
#'
#' @return A named list of arguments for \code{\link{hbm}}.
#'
#' @examples
#' p_hs   <- hbm_priors(prior_type = "horseshoe", hs_df = 1, hs_df_slab = 4)
#' p_r2d2 <- hbm_priors(prior_type = "r2d2",      r2d2_mean_R2 = 0.5)
#'
#' @seealso \code{\link{hbm_control}}, \code{\link{hbm_nonlinear}},
#'   \code{\link{hbm}}
#' @export
hbm_priors <- function(prior_type      = c("default", "horseshoe", "r2d2"),
                       prior           = NULL,
                       hs_df           = 1,
                       hs_df_global    = 1,
                       hs_df_slab      = 4,
                       hs_scale_global = NULL,
                       hs_scale_slab   = 2,
                       hs_par_ratio    = NULL,
                       r2d2_mean_R2    = 0.5,
                       r2d2_prec_R2    = 2,
                       r2d2_cons_D2    = NULL) {
  prior_type <- match.arg(prior_type)
  out <- list(prior_type = prior_type)
  if (!is.null(prior))           out$prior           <- prior
  if (prior_type == "horseshoe") {
    out$hs_df           <- hs_df
    out$hs_df_global    <- hs_df_global
    out$hs_df_slab      <- hs_df_slab
    out$hs_scale_global <- hs_scale_global
    out$hs_scale_slab   <- hs_scale_slab
    out$hs_par_ratio    <- hs_par_ratio
  }
  if (prior_type == "r2d2") {
    out$r2d2_mean_R2 <- r2d2_mean_R2
    out$r2d2_prec_R2 <- r2d2_prec_R2
    out$r2d2_cons_D2 <- r2d2_cons_D2
  }
  class(out) <- c("hbm_config_priors", "hbm_config", "list")
  out
}


#' Nonlinear-Term Configuration for HBSAE Models
#'
#' Bundles the nonlinear-term arguments of \code{\link{hbm}} into a
#' single named list.  Same opt-in pattern as \code{\link{hbm_control}}.
#'
#' @param terms Character vector of predictor names to be wrapped in
#'   \code{s()} or \code{gp()}.
#' @param type  Character.  \code{"spline"} (default) or \code{"gp"}.
#' @param k     Integer.  Spline basis dimension.  Default \code{-1L}
#'   (let \pkg{mgcv} choose).
#' @param gp_scale Optional numeric.  GP length-scale (\code{c} argument
#'   of \code{\link[brms]{gp}}).
#'
#' @return A named list of arguments for \code{\link{hbm}}, mapping to
#'   \code{nonlinear}, \code{nonlinear_type}, \code{spline_k}, \code{gp_scale}.
#'
#' @examples
#' nl_spline <- hbm_nonlinear(c("x1", "x3"), type = "spline", k = 5)
#' nl_gp     <- hbm_nonlinear(c("x2"),       type = "gp",     gp_scale = 1.5)
#'
#' @seealso \code{\link{hbm_control}}, \code{\link{hbm_priors}},
#'   \code{\link{hbm}}
#' @export
hbm_nonlinear <- function(terms,
                          type     = c("spline", "gp"),
                          k        = -1L,
                          gp_scale = NULL) {
  type <- match.arg(type)
  if (!is.character(terms) || length(terms) == 0L)
    stop("'terms' must be a non-empty character vector.", call. = FALSE)
  out <- list(
    nonlinear      = terms,
    nonlinear_type = type,
    spline_k       = as.integer(k)
  )
  if (!is.null(gp_scale)) out$gp_scale <- gp_scale
  class(out) <- c("hbm_config_nonlinear", "hbm_config", "list")
  out
}
