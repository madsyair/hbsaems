# =============================================================================
# hbm() -- Hierarchical Bayesian Model for Small Area Estimation
# =============================================================================

#' Hierarchical Bayesian Model for Small Area Estimation
#'
#' @title hbm: Hierarchical Bayesian Small Area Models
#'
#' @description
#' Fits a hierarchical Bayesian model for Small Area Estimation (SAE) using
#' the \code{brms} package (Stan back-end).  The function supports fixed
#' effects, random effects, spatial random effects (CAR/SAR), user-defined
#' priors, and three strategies for handling missing data in auxiliary
#' (predictor) variables.
#'
#' @details
#' **Spatial Small Area Estimation Models**
#'
#' For spatially correlated areas, hbsaems extends the standard area-level
#' SAE model (Fay-Herriot 1979) by adding a spatially structured random
#' effect:
#' \deqn{y_i = x_i^\top \boldsymbol{\beta} + u_i + e_i,}
#' where \eqn{u_i} is the spatial random effect for area \eqn{i} and
#' \eqn{e_i} the sampling error.  Two families of spatial structures are
#' supported.
#'
#' \describe{
#'   \item{\strong{CAR (Conditional Autoregressive; Besag 1974)}}{
#'     Specified by \code{sre_type = "car"}.  The joint distribution of the
#'     spatial effects is
#'     \deqn{u \sim \mathcal{N}\bigl(0,\, \sigma_u^2 (D - \rho W)^{-1}\bigr),}
#'     where \eqn{W} is a binary adjacency matrix (1 if neighbour, 0
#'     otherwise) and \eqn{D = \mathrm{diag}(W \mathbf{1})}.  Sub-types via
#'     \code{car_type}: \code{"icar"} (intrinsic, \eqn{\rho = 1}; Besag 1991);
#'     \code{"escar"}, \code{"esicar"} (exact sparse formulations of
#'     Morris et al.\ 2019); \code{"bym2"} (BYM2 reparameterisation of
#'     Riebler et al.\ 2016, recommended for disconnected graphs).
#'   }
#'   \item{\strong{SAR (Simultaneous Autoregressive; Whittle 1954, Anselin 1988)}}{
#'     Specified by \code{sre_type = "sar"}.  The model is
#'     \deqn{u = \rho W u + \varepsilon,
#'        \quad \varepsilon \sim \mathcal{N}(0, \sigma_\varepsilon^2 I),}
#'     where \eqn{W} is row-standardised so that
#'     \eqn{\rho \in (-1, 1)} carries an interpretable correlation
#'     meaning.  Sub-types via \code{sar_type}: \code{"lag"} (spatial lag
#'     of the response, \eqn{y = \rho W y + X\boldsymbol{\beta} + \varepsilon});
#'     \code{"error"} (spatial error model).
#'   }
#' }
#'
#' Use \code{\link{check_spatial_weight}} to verify that \eqn{M} satisfies
#' the theoretical requirements (square, zero diagonal, symmetry for CAR,
#' style-appropriate for the model class).  Use \code{\link{build_spatial_weight}}
#' to construct \eqn{M} from a shapefile.
#'
#' **Missing Data Strategies**
#'
#' The three strategies differ in scope and statistical assumptions:
#'
#' \describe{
#'   \item{\code{"deleted"}}{
#'     Complete-case analysis.  Only rows where \strong{all} response
#'     variable(s) are observed are used for model fitting.  Auxiliary
#'     variables must be complete; otherwise an informative error is raised.
#'     Appropriate under MCAR (Missing Completely At Random).
#'   }
#'   \item{\code{"multiple"}}{
#'     Multiple imputation via \code{mice} \strong{for auxiliary (predictor)
#'     variables only}.  The response variable \eqn{Y} is \strong{never}
#'     imputed.  In a Bayesian model, missing outcomes are naturally
#'     marginalised through the posterior predictive distribution:
#'     \deqn{p(\theta \mid Y_{\text{obs}}, X) =
#'       \int p(\theta \mid Y_{\text{obs}}, Y_{\text{mis}}, X)\,
#'             p(Y_{\text{mis}} \mid Y_{\text{obs}}, X, \theta)\,
#'             \mathrm{d}Y_{\text{mis}}.}
#'     Imputing \eqn{Y} before fitting would replace this integral with a
#'     single point substitute, deflate posterior uncertainty, and potentially
#'     bias the estimates if the imputation model is misspecified.  If \eqn{Y}
#'     has missing values, those rows are excluded from model fitting (a
#'     \code{warning} is issued) but are retained in the returned object for
#'     subsequent prediction via \code{\link{hbsae}}.  Appropriate under MAR
#'     (Missing At Random).
#'   }
#'   \item{\code{"model"}}{
#'     Model-based imputation using \code{brms::mi()}.  Missing values in
#'     auxiliary variables are jointly estimated with the model parameters.
#'     The user must specify imputation sub-models explicitly in the
#'     \code{formula} argument, e.g.:
#'     \code{bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ x2)}.
#'     Only applicable to \strong{continuous} distributions.  Appropriate
#'     under MAR.
#'   }
#' }
#'
#' If data are Missing Not At Random (MNAR), none of the above strategies
#' applies directly; sensitivity analyses and explicit missingness models are
#' recommended.
#'
#' @name hbm
#'
#' @param formula A \code{\link[brms]{brmsformula}} or standard
#'   \code{formula} object specifying the model structure.  For multi-response
#'   or imputation sub-models use \code{\link[brms]{bf}}.  Examples:
#'   \code{formula(y ~ x1 + x2)}, \code{bf(y ~ x1 + x2)}, or
#'   \code{bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ x2)}.
#' @param hb_sampling Character string naming the distribution family of the
#'   response variable (default: \code{"gaussian"}).  Any family supported by
#'   \code{\link[brms]{brmsfamily}} is accepted.
#' @param hb_link Character string specifying the link function
#'   (default: \code{"identity"}).
#' @param link_phi Character string specifying the link function for the
#'   precision/phi parameter (default: \code{"log"}).  Only used for Beta and
#'   related families.
#' @param re An optional one-sided \code{formula} specifying group-level
#'   (random) effects, e.g. \code{~(1|area)}.  Must follow standard
#'   \code{lme4}-style syntax: \code{~ (1|group1) + (1|group2)}.  If
#'   \code{NULL} (default) \emph{and} \code{sre_type} is also \code{NULL},
#'   the function emits a warning recommending an area-level random
#'   effect, since a Fay-Herriot SAE model with neither IID nor spatial
#'   area effects degenerates to fixed-effects regression and does not
#'   borrow strength across areas.  The warning can be silenced with
#'   \code{suppressWarnings()} if a fixed-effects-only baseline is
#'   intentional.
#' @param sre Character.  Name of the grouping variable used for spatial
#'   random effects.  Must be supplied \emph{together with} \code{sre_type}
#'   and \code{M}; providing only one of them is an error.
#' @param sre_type Character.  Type of spatial model: \code{"car"}
#'   (Conditional Autoregressive) or \code{"sar"} (Simultaneous
#'   Autoregressive).  Must be supplied \emph{together with} \code{sre}
#'   and \code{M}; providing only one of them is an error.
#' @param car_type Character.  CAR subtype passed to \code{brms}: one of
#'   \code{"escar"} (exact sparse CAR), \code{"esicar"} (exact sparse
#'   intrinsic CAR), \code{"icar"} (intrinsic CAR), or \code{"bym2"}.
#'   Defaults to \code{"icar"} when \code{sre_type = "car"}.
#' @param sar_type Character.  SAR subtype: \code{"lag"} (SAR of the response
#'   values) or \code{"error"} (SAR of the residuals).  Defaults to
#'   \code{"lag"} when \code{sre_type = "sar"}.
#' @param M Spatial matrix supplied as \code{data2} to \code{brms}.  For CAR
#'   this must be a binary adjacency matrix; for SAR a spatial weight matrix.
#'   Row names must match the levels of the spatial grouping variable.
#' @param data A data frame containing all variables referenced in
#'   \code{formula}.
#' @param prior Priors specified via \code{\link[brms]{prior}} or a list
#'   thereof.  If \code{NULL} (default), \code{brms} default priors are used.
#'
#' @param fixed_params Named list pinning distributional parameters to
#'   known values instead of sampling them.  Each entry maps a parameter
#'   name to one of:
#'   \describe{
#'     \item{A column name (character)}{The named column in \code{data} is
#'       used as the fixed values.}
#'     \item{A scalar (numeric, length 1)}{Broadcast to all rows.}
#'     \item{A vector (numeric, length \code{nrow(data)})}{Used directly.}
#'     \item{A one-sided formula (e.g. \code{~ I(n / deff - 1)})}{Evaluated
#'       against \code{data} to produce the vector of fixed values.}
#'   }
#'   Internally, each pinned parameter \code{<par>} is attached to the
#'   data as a column \code{.hbsaems_<par>_fixed} and added to the brms
#'   formula as \code{<par> ~ 0 + offset(.hbsaems_<par>_fixed)}.  Using
#'   \code{fixed_params} on a parameter for which the user also supplies
#'   an explicit prior is an error.  Typical use cases include pinning
#'   \code{phi} for Beta regression from survey design effect
#'   (\code{phi = ~ I(n / deff - 1)}) and pinning \code{sigma} for
#'   Fay--Herriot-style models with known sampling variance.
#'
#' @param prior_type Character.  Global-local shrinkage prior applied to all
#'   regression coefficients (\code{class = "b"}).  One of:
#'   \describe{
#'     \item{\code{"default"}}{No shrinkage prior is added; the \code{prior}
#'       argument governs everything (default).}
#'     \item{\code{"horseshoe"}}{Regularised horseshoe prior
#'       (Piironen & Vehtari 2017).  Encourages exact sparsity while allowing
#'       large signals through.  Controlled by \code{hs_df},
#'       \code{hs_df_global}, \code{hs_df_slab}, \code{hs_scale_global},
#'       \code{hs_scale_slab}, and \code{hs_par_ratio}.}
#'     \item{\code{"r2d2"}}{R2D2 prior (Zhang et al. 2022).  Places a prior
#'       directly on the model \eqn{R^2} and distributes explained variance
#'       across predictors via a Dirichlet decomposition.  Controlled by
#'       \code{r2d2_mean_R2}, \code{r2d2_prec_R2}, and \code{r2d2_cons_D2}.}
#'   }
#'   If \code{prior} already contains a global \code{class = "b"} entry,
#'   \code{prior_type} is ignored and a warning is issued.
#' @param hs_df Numeric \eqn{> 0}.  Local half-\eqn{t} degrees of freedom for
#'   the horseshoe prior (default \code{1} = half-Cauchy).
#' @param hs_df_global Numeric \eqn{> 0}.  Global half-\eqn{t} degrees of
#'   freedom (default \code{1}).
#' @param hs_df_slab Numeric \eqn{> 0}.  Slab half-\eqn{t} degrees of
#'   freedom (default \code{4}).
#' @param hs_scale_global Numeric \eqn{> 0} or \code{NULL}.  Scale for the
#'   global half-\eqn{t} prior.  \code{NULL} (default) lets \code{brms}
#'   compute it automatically from the number of predictors.
#' @param hs_scale_slab Numeric \eqn{> 0}.  Scale for the slab component
#'   (default \code{2}).
#' @param hs_par_ratio Numeric \eqn{> 0} or \code{NULL}.  Expected ratio of
#'   non-zero to total coefficients.  \code{NULL} (default) treats all
#'   coefficients as potentially non-zero.
#' @param r2d2_mean_R2 Numeric in \eqn{(0, 1)}.  Prior mean of the model
#'   \eqn{R^2} (default \code{0.5}).
#' @param r2d2_prec_R2 Numeric \eqn{> 0}.  Prior precision of \eqn{R^2}
#'   (default \code{2}).  Higher values concentrate mass around
#'   \code{r2d2_mean_R2}.
#' @param r2d2_cons_D2 Numeric \eqn{> 0} or \code{NULL}.  Dirichlet
#'   concentration for the D2 component.  \code{NULL} (default) corresponds to
#'   \code{0.5}, yielding a uniform distribution over the simplex.
#'
#' @param nonlinear Character vector or \code{NULL}.  Names of predictor
#'   variables to model with a smooth nonlinear term.  Each listed variable
#'   is replaced in the formula RHS with \code{s(var)} (spline) or
#'   \code{gp(var)} (Gaussian process).  Variables not listed remain linear.
#'   Do \emph{not} also write \code{s(x)} in the formula when using this
#'   argument -- the modification is applied automatically.  Default \code{NULL}
#'   (all predictors remain linear).
#' @param nonlinear_type Character.  Smooth term family to use.  One of
#'   \code{"spline"} (default, thin-plate regression spline via
#'   \code{mgcv::s()}) or \code{"gp"} (Gaussian process via
#'   \code{brms::gp()}).
#' @param spline_k Integer.  Spline basis dimension (number of knots).
#'   \code{-1L} (default) lets \pkg{mgcv} choose automatically.  Values
#'   \eqn{\geq 3} are also accepted.  Ignored when
#'   \code{nonlinear_type = "gp"}.
#' @param gp_scale Numeric \eqn{> 0} or \code{NULL}.  Length-scale
#'   hyperparameter (\code{c} in \code{brms::gp()}).  \code{NULL} (default)
#'   uses the \code{brms} default.  Ignored when
#'   \code{nonlinear_type = "spline"}.
#'
#' @param handle_missing Character or \code{NULL}.  Strategy for missing data.
#'   One of \code{"deleted"}, \code{"multiple"}, or \code{"model"} (see
#'   \strong{Details}).  If \code{NULL} (default) and missing values exist in
#'   the data, an informative error is raised.
#' @param m Integer.  Number of imputations when
#'   \code{handle_missing = "multiple"} (default: \code{5}).  Ignored for
#'   other strategies.
#' @param mice_args A named list of additional arguments forwarded to
#'   \code{\link[mice]{mice}}, for example
#'   \code{list(method = "pmm", seed = 42)}.  Only used when
#'   \code{handle_missing = "multiple"}.
#' @param control A named list of sampler control parameters
#'   (default: \code{list()}).  Passed directly to \code{\link[brms]{brm}}.
#' @param chains Integer.  Number of MCMC chains (default: \code{4}).
#' @param iter Integer.  Total iterations per chain (default: \code{4000}).
#' @param warmup Integer.  Warm-up iterations per chain
#'   (default: \code{floor(iter / 2)}).
#' @param cores Integer.  Number of CPU cores for parallel sampling
#'   (default: \code{1}).
#' @param sample_prior Character.  Whether to draw from the prior
#'   distribution.  One of \code{"no"} (default), \code{"yes"}, or
#'   \code{"only"}.
#' @param stanvars An optional \code{\link[brms]{stanvar}} object (or a list
#'   of such objects) supplying additional Stan code, data, or parameters.
#'   Passed directly to \code{\link[brms]{brm}} and
#'   \code{\link[brms]{brm_multiple}}.  Intended for use by wrapper functions
#'   such as \code{\link{hbm_betalogitnorm}} that require custom Stan blocks;
#'   end users typically do not need to set this argument.
#'   Default: \code{NULL}.
#' @param ... Additional arguments forwarded to \code{\link[brms]{brm}} or
#'   \code{\link[brms]{brm_multiple}}.
#'
#' @return An object of class \code{hbmfit}, which is a named list containing:
#' \item{model}{The fitted \code{brmsfit} object (or \code{brmsfit_multiple}
#'   when \code{handle_missing = "multiple"} with missing predictors).}
#' \item{handle_missing}{The missing-data strategy used (\code{NULL} if the
#'   data were complete).}
#' \item{data}{The \strong{original} data frame passed to \code{hbm()} before
#'   any row deletion or imputation.  This is intentional: \code{\link{hbsae}}
#'   needs all rows -- including those with missing \eqn{Y} -- to generate
#'   predictions for every small area.}
#'
#' @importFrom brms bf brm brm_multiple brmsfamily save_pars
#' @importFrom stats update complete.cases as.formula
#' @importFrom mice mice complete
#'
#' @export
#'
#' @author Achmad Syahrul Choir, Saniyyah Sri Nurhayati, and Sofi Zamzanah
#'
#' @references
#' Rao, J. N. K., & Molina, I. (2015). \emph{Small Area Estimation}.
#' John Wiley & Sons.
#'
#' Burkner, P. C. (2017). brms: An R package for Bayesian multilevel models
#' using Stan. \emph{Journal of Statistical Software}, 80(1), 1--28.
#'
#' van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#' imputation by chained equations in R. \emph{Journal of Statistical
#' Software}, 45(3), 1--67.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' data <- data_fhnorm
#'
#' # Standard lightweight MCMC settings used throughout these examples.
#' # Use chains = 4, iter = 4000 for production runs.
#' FAST <- list(chains = 2, iter = 2000, warmup = 1000, cores = 1,
#'              seed = 123, refresh = 0)
#'
#' # -- Basic model --------------------------------------------------------------
#' model <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        hb_sampling = "gaussian",
#'        hb_link     = "identity",
#'        re          = ~(1 | group),
#'        data        = data),
#'   FAST
#' ))
#' summary(model)
#'
#' # -- Horseshoe prior (sparse coefficients) ------------------------------------
#' model_hs <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        re          = ~(1 | group),
#'        data        = data,
#'        prior_type  = "horseshoe",
#'        hs_df       = 1),
#'   FAST
#' ))
#' summary(model_hs)
#'
#' # -- R2D2 prior (prior on model R-squared) -------------------------------------
#' model_r2 <- do.call(hbm, c(
#'   list(formula      = bf(y ~ x1 + x2 + x3),
#'        re           = ~(1 | group),
#'        data         = data,
#'        prior_type   = "r2d2",
#'        r2d2_mean_R2 = 0.5,
#'        r2d2_prec_R2 = 2),
#'   FAST
#' ))
#' summary(model_r2)
#'
#' # -- Spline smooth for x1 (nonlinear) -----------------------------------------
#' # x1 is modelled with s(x1); x2 and x3 remain linear.
#' model_spline <- do.call(hbm, c(
#'   list(formula        = bf(y ~ x1 + x2 + x3),
#'        re             = ~(1 | group),
#'        data           = data,
#'        nonlinear      = "x1",
#'        nonlinear_type = "spline"),
#'   FAST
#' ))
#' summary(model_spline)
#'
#' # -- Gaussian process for x2 (nonlinear) --------------------------------------
#' model_gp <- do.call(hbm, c(
#'   list(formula        = bf(y ~ x1 + x2 + x3),
#'        re             = ~(1 | group),
#'        data           = data,
#'        nonlinear      = "x2",
#'        nonlinear_type = "gp"),
#'   FAST
#' ))
#' summary(model_gp)
#'
#' # -- Missing data: deletion (Y missing, X complete) ---------------------------
#' data_miss_y        <- data
#' data_miss_y$y[3:5] <- NA
#'
#' model_deleted <- do.call(hbm, c(
#'   list(formula        = bf(y ~ x1 + x2 + x3),
#'        re             = ~(1 | group),
#'        data           = data_miss_y,
#'        handle_missing = "deleted"),
#'   FAST
#' ))
#' summary(model_deleted)
#'
#' # -- Missing data: multiple imputation (X only -- Y is never imputed) ----------
#' data_miss_x          <- data
#' data_miss_x$x1[6:8]  <- NA
#'
#' model_multiple <- do.call(hbm, c(
#'   list(formula        = bf(y ~ x1 + x2 + x3),
#'        re             = ~(1 | group),
#'        data           = data_miss_x,
#'        handle_missing = "multiple",
#'        m              = 5),
#'   FAST
#' ))
#' summary(model_multiple)
#'
#' # -- Missing data: model-based imputation with mi() ---------------------------
#' data_miss_x2         <- data
#' data_miss_x2$x1[6:7] <- NA
#'
#' model_model <- do.call(hbm, c(
#'   list(formula        = bf(y | mi() ~ mi(x1) + x2 + x3) +
#'                         bf(x1 | mi() ~ x2 + x3),
#'        re             = ~(1 | group),
#'        data           = data_miss_x2,
#'        handle_missing = "model"),
#'   FAST
#' ))
#' summary(model_model)
#'
#' # -- Spatial: CAR (Conditional Autoregressive) --------------------------------
#' data("adjacency_matrix_car")
#' model_car <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        data        = data,
#'        sre         = "sre",
#'        sre_type    = "car",
#'        M           = adjacency_matrix_car),
#'   FAST
#' ))
#' summary(model_car)
#'
#' # -- Spatial: SAR (Simultaneous Autoregressive) -------------------------------
#' data("spatial_weight_sar")
#' model_sar <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        data        = data,
#'        sre         = "sre",
#'        sre_type    = "sar",
#'        M           = spatial_weight_sar),
#'   FAST
#' ))
#' summary(model_sar)
#' }
hbm <- function(formula,
                hb_sampling    = "gaussian",
                hb_link        = "identity",
                link_phi       = "log",
                re             = NULL,
                sre            = NULL,
                sre_type       = NULL,
                car_type       = NULL,
                sar_type       = NULL,
                M              = NULL,
                data,
                prior          = NULL,
                # -- v0.6.1: Fixed-value distributional parameters ------------
                fixed_params   = NULL,
                # -- Shrinkage priors (Feature 1) -----------------------------
                prior_type      = "default",
                hs_df           = 1,
                hs_df_global    = 1,
                hs_df_slab      = 4,
                hs_scale_global = NULL,
                hs_scale_slab   = 2,
                hs_par_ratio    = NULL,
                r2d2_mean_R2    = 0.5,
                r2d2_prec_R2    = 2,
                r2d2_cons_D2    = NULL,
                # -- Nonlinear smooth terms (Feature 2) -----------------------
                nonlinear       = NULL,
                nonlinear_type  = "spline",
                spline_k        = -1L,
                gp_scale        = NULL,
                # -- Missing data ---------------------------------------------
                handle_missing = NULL,
                m              = 5L,
                mice_args      = list(),
                control        = list(),
                chains         = 4L,
                iter           = 4000L,
                warmup         = floor(iter / 2),
                cores          = 1L,
                sample_prior   = "no",
                stanvars       = NULL,
                ...) {

  # -- v0.5.0: Intercept hbm_config bundles in ... ----------------------------
  # If the user passed any hbm_control()/hbm_priors()/hbm_nonlinear() bundles
  # via `...`, splice them into the explicit argument list and recurse.
  # This mirrors the brm() pattern where extra configuration travels in `...`.
  dots <- list(...)
  is_bundle <- vapply(dots, inherits, logical(1L), what = "hbm_config")
  if (any(is_bundle)) {
    # Capture the *named* explicit arguments as they were passed.
    explicit <- as.list(match.call(expand.dots = FALSE))[-1L]
    explicit$"..." <- NULL                    # drop the literal ...
    # Splice bundle contents AFTER explicit args (explicit wins on conflict).
    merged <- list()
    for (b in dots[is_bundle])
      merged <- utils::modifyList(merged, b)
    # Non-bundle ... arguments come through unchanged.
    extra <- dots[!is_bundle]
    # Final precedence: explicit args > extra ... > bundles.
    final_args <- utils::modifyList(merged,
                     utils::modifyList(extra,
                       lapply(explicit, eval, envir = parent.frame())))
    return(do.call(hbm, final_args))
  }

  # -- 0. Input validation and coercion ----------------------------------------
  # Delegated to internal helper for testability and clarity.
  data  <- .validate_hbm_data(data)
  n     <- nrow(data)
  data2 <- NULL

  # -- 0b. v0.6.1: Process fixed_params -----------------------------------------
  # Resolve user-specified pinned parameters (column name / scalar / vector /
  # formula) to numeric vectors and attach them as columns of `data` named
  # .hbsaems_<par>_fixed.  Conflict / type / NA checks fail loudly here.
  .fixed_params_processed <- .process_fixed_params(fixed_params, data)
  if (length(.fixed_params_processed$resolved) > 0L) {
    data <- .attach_fixed_columns(data, .fixed_params_processed)
  }

  # -- 1. Formula parsing -------------------------------------------------------
  # Delegated to .parse_hbm_formula() helper (v0.5.0).
  parsed <- .parse_hbm_formula(formula)
  main_formula   <- parsed$main_formula
  all_formulas   <- parsed$all_formulas
  response_var   <- parsed$response_var
  auxiliary_vars <- parsed$auxiliary_vars

  # -- 1b. Apply nonlinear smooth terms (Feature 2) -----------------------------
  # Replace listed predictor variables in the formula RHS with s(var) (spline)
  # or gp(var) (Gaussian process) terms.  This must happen before the
  # handle_missing block so that mi() wrappers added by .add_mi_to_lhs() operate
  # on a formula that already contains the correct smooth-term syntax.
  if (!is.null(nonlinear) && length(nonlinear) > 0L) {
    nonlinear_type <- match.arg(nonlinear_type, c("spline", "gp"))

    .validate_nonlinear(nonlinear, auxiliary_vars, data)

    if (nonlinear_type == "spline" && !requireNamespace("mgcv", quietly = TRUE))
      stop(
        "Package 'mgcv' is required for nonlinear_type = 'spline'. ",
        "Install it with: install.packages('mgcv')",
        call. = FALSE
      )

    all_formulas <- .apply_nonlinear_to_formula(
      formula        = all_formulas,
      nonlinear      = nonlinear,
      nonlinear_type = nonlinear_type,
      spline_k       = spline_k,
      gp_scale       = gp_scale
    )
  }

  # -- 2. Detect missing values -------------------------------------------------
  missing_info <- .detect_missing(data, response_var, auxiliary_vars)
  missing_y    <- missing_info$missing_y   # NULL or character vector
  missing_x    <- missing_info$missing_x   # NULL or character vector

  # -- 3. Discrete-family flag ---------------------------------------------------
  # v0.4.0: lookup the family registry instead of using a hardcoded list.
  # Falls back to a static list for the few brms families not (yet)
  # registered, so behaviour matches v0.3.0 exactly when the registry has
  # not been touched.
  is_discrete <- isTRUE(.model_is_discrete(hb_sampling)) ||
    hb_sampling %in% c("cumulative", "cratio", "sratio", "acat")

  # -- 4. Validate handle_missing -----------------------------------------------
  has_any_missing <- !is.null(missing_y) || !is.null(missing_x)

  if (has_any_missing) {
    if (is.null(handle_missing)) {
      stop(
        "`handle_missing` must be specified when the data contain missing ",
        "values. Choose one of: 'deleted', 'multiple', or 'model'.",
        call. = FALSE
      )
    }
    if (is_discrete && handle_missing == "model") {
      stop(
        "Discrete distributions do not support `handle_missing = 'model'` ",
        "because the mi() approach requires a continuous likelihood. ",
        "Use `handle_missing = 'multiple'` instead.",
        call. = FALSE
      )
    }
  }

  # -- 5. Initial state ---------------------------------------------------------
  # data_complete always stores the ORIGINAL data (all rows, all columns).
  # It is returned unchanged so that hbsae() can generate predictions for
  # every small area, including those with missing Y.
  data_complete <- data
  multiple      <- FALSE   # flag: use brm_multiple() instead of brm()

  # -- 6. Missing data handling -------------------------------------------------
  if (has_any_missing) {

    # -- 6a. DELETED -------------------------------------------------------------
    if (handle_missing == "deleted") {
      if (!is.null(missing_x)) {
        stop(
          "Option `handle_missing = 'deleted'` requires all auxiliary ",
          "(predictor) variables to be complete. Missing values were detected ",
          "in: ", paste(missing_x, collapse = ", "), ". ",
          "Consider `handle_missing = 'multiple'` to impute missing predictors.",
          call. = FALSE
        )
      }
      # Remove rows with missing response variable(s).
      n_before <- nrow(data)
      if (length(response_var) > 1L) {
        data <- data[stats::complete.cases(data[, response_var, drop = FALSE]),
                     , drop = FALSE]
      } else {
        data <- data[!is.na(data[[response_var]]), , drop = FALSE]
      }
      n <- nrow(data)
      message(
        "handle_missing = 'deleted': ",
        n_before - n,
        " row(s) with missing response variable removed from model fitting."
      )

    # -- 6b. MULTIPLE IMPUTATION --------------------------------------------------
    } else if (handle_missing == "multiple") {
      multiple <- TRUE

      # ----- KEY FIX -----------------------------------------------------------
      # mice MUST NOT impute the response variable Y.
      #
      # Statistical rationale (Bayesian SAE):
      #   In a Bayesian hierarchical model, missing responses Y_mis are
      #   integrated out analytically through the posterior predictive
      #   distribution p(Y_mis | Y_obs, X, theta).  Replacing this integral
      #   with a mice imputation:
      #     (a) introduces an additional imputation model whose assumptions are
      #         separate from -- and often inconsistent with -- the Bayesian
      #         likelihood;
      #     (b) artificially inflates the effective sample size, deflating
      #         posterior uncertainty;
      #     (c) biases the posterior if the imputation model is misspecified.
      #
      #   Correct behaviour: mice imputes ONLY auxiliary predictor variables.
      #   Rows with missing Y are excluded from parameter estimation (like
      #   "deleted" for those rows) but retained in data_complete so that
      #   hbsae() can predict all small areas.
      # -------------------------------------------------------------------------
      if (!is.null(missing_y)) {
        warning(
          "Missing values detected in response variable(s): ",
          paste(missing_y, collapse = ", "), ".\n",
          "Multiple imputation via `mice` applies ONLY to auxiliary predictor ",
          "variables (X). Response variable(s) are NEVER imputed inside a ",
          "Bayesian model -- missing Y rows will be excluded from model fitting ",
          "but are retained in the returned object for prediction via hbsae().\n",
          "If you wish to model missingness in Y jointly with the parameters, ",
          "consider `handle_missing = 'model'` (continuous outcomes only).",
          call. = FALSE
        )
      }

      if (!is.null(missing_x)) {
        message(
          "Missing predictor variable(s): ",
          paste(missing_x, collapse = ", "),
          ". Applying multiple imputation (mice) with m = ", m,
          " imputations."
        )
      } else {
        # Only Y is missing; X is complete.
        # mice cannot impute the response -- that would conflict with the
        # Bayesian likelihood.  Adopt the statistically correct strategy
        # depending on the distribution family.
        multiple <- FALSE

        if (is_discrete) {
          # Discrete families (binomial, Poisson, ...) do not support
          # model-based imputation via mi().  Fall back to complete-case
          # analysis: remove rows where Y is missing before fitting.
          message(
            "handle_missing = 'multiple' was specified but only the response ",
            "variable (Y) is missing and the distribution is discrete. ",
            "Discrete families do not support model-based imputation (mi()). ",
            "Automatically falling back to handle_missing = 'deleted': ",
            "rows with missing Y will be removed before model fitting."
          )
          handle_missing <- "deleted"
          data <- data[
            stats::complete.cases(data[, response_var, drop = FALSE]),
            , drop = FALSE
          ]
          n <- nrow(data)

        } else {
          # Continuous distribution: the correct Bayesian treatment is to
          # jointly estimate Y_mis with the model parameters via brms::mi().
          # Automatically convert to handle_missing = "model" and patch the
          # formula LHS.
          #
          # Note: re and sre terms are added to the formula LATER in this
          # function, so .add_mi_to_lhs() only needs to handle the base
          # formula structure -- it will not interfere with those terms.
          message(
            "handle_missing = 'multiple' was specified but only the response ",
            "variable (Y) is missing and X is complete. ",
            "mice imputes predictor variables only and cannot impute Y. ",
            "Automatically converting to handle_missing = 'model': ",
            "'| mi()' will be added to the formula LHS so that brms jointly ",
            "estimates missing Y with the model parameters (the correct ",
            "Bayesian approach). ",
            "To suppress this message, set handle_missing = 'model' ",
            "explicitly and write '",
            paste(response_var, collapse = ", "),
            " | mi()' in your formula."
          )
          handle_missing <- "model"
          all_formulas   <- .add_mi_to_lhs(all_formulas, response_var)
        }
      }

    # -- 6c. MODEL-BASED IMPUTATION -----------------------------------------------
    } else if (handle_missing == "model") {
      if (is.null(formula)) {
        stop(
          "No formula provided. `handle_missing = 'model'` requires explicit ",
          "mi() terms in the formula, for example:\n",
          "  bf(y | mi() ~ mi(x1) + x2) + bf(x1 | mi() ~ x2)",
          call. = FALSE
        )
      }

      # Validate that every missing variable has the correct mi() specification.
      all_missing_vars <- unique(c(missing_y, missing_x))

      formula_strs <- if (!is.null(formula$forms)) {
        sapply(
          all_formulas$forms,
          function(v) paste(deparse(v$formula), collapse = " ")
        )
      } else {
        paste(deparse(all_formulas$formula), collapse = " ")
      }

      # Response variables need "var | mi()" on the LHS.
      incomplete_responses <- all_missing_vars[!vapply(all_missing_vars, function(var) {
        any(grepl(paste0("\\b", var, " \\| mi\\("), formula_strs))
      }, logical(1L))]

      # Predictor variables need "mi(var)" on the RHS.
      incomplete_predictors <- if (!is.null(missing_x)) {
        missing_x[!vapply(missing_x, function(var) {
          any(grepl(paste0("mi\\(", var, "\\)"), formula_strs))
        }, logical(1L))]
      } else {
        character(0L)
      }

      if (length(incomplete_responses) > 0L || length(incomplete_predictors) > 0L) {
        stop(
          "Formula is incomplete for `handle_missing = 'model'`.\n",
          if (length(incomplete_responses) > 0L)
            paste0(
              "  Missing '| mi()' on the LHS for: ",
              paste(incomplete_responses, collapse = ", "), ".\n"
            ),
          if (length(incomplete_predictors) > 0L)
            paste0(
              "  Missing 'mi()' wrapper on the RHS for: ",
              paste(incomplete_predictors, collapse = ", "), "."
            ),
          call. = FALSE
        )
      }
      message(
        "handle_missing = 'model': using mi() specification for ",
        "joint model-based imputation."
      )
    }
  } # end if (has_any_missing)

  # -- 7. Prior validation -------------------------------------------------------
  if (!is.null(prior) && !inherits(prior, "brmsprior")) {
    stop(
      "Argument 'prior' must be a 'brmsprior' object created via ",
      "brms::prior() or brms::set_prior().",
      call. = FALSE
    )
  }

  # -- 7b. Build and merge shrinkage prior (Feature 1) -------------------------
  # When prior_type != "default", build a global-local shrinkage prior for
  # class = "b" and merge it with any user-supplied priors.
  # .merge_prior_type() warns and ignores the shrinkage prior if the user has
  # already supplied a global class = "b" prior.
  if (!is.null(prior_type) && prior_type != "default") {
    prior_type <- match.arg(prior_type, c("default", "horseshoe", "r2d2"))

    type_prior <- .build_prior_type(
      prior_type      = prior_type,
      hs_df           = hs_df,
      hs_df_global    = hs_df_global,
      hs_df_slab      = hs_df_slab,
      hs_scale_global = hs_scale_global,
      hs_scale_slab   = hs_scale_slab,
      hs_par_ratio    = hs_par_ratio,
      r2d2_mean_R2    = r2d2_mean_R2,
      r2d2_prec_R2    = r2d2_prec_R2,
      r2d2_cons_D2    = r2d2_cons_D2
    )

    prior <- .merge_prior_type(prior, type_prior)
  }

  # -- 8. Random effects --------------------------------------------------------
  if (!is.null(re)) {
    re_str       <- as.character(re)
    valid_re_pat <- paste0(
      "^\\(\\s*1\\s*\\|\\s*\\w+(\\s*\\+\\s*\\w+)*\\s*\\)",
      "(\\s*\\+\\s*\\(\\s*1\\s*\\|\\s*\\w+(\\s*\\+\\s*\\w+)*\\s*\\))*\\s*$"
    )
    if (!grepl(valid_re_pat, re_str[2L])) {
      stop(
        "Invalid `re` formula. Expected syntax: ~ (1|group1) + (1|group2)\n",
        "Received: ", re_str[2L],
        call. = FALSE
      )
    }
    re_term <- re_str[2L]
    if (!is.null(all_formulas$forms)) {
      all_formulas$forms <- lapply(all_formulas$forms, function(f) {
        f$formula <- stats::update(f$formula, paste(". ~ . +", re_term))
        f
      })
    } else {
      all_formulas$formula <- stats::update(
        all_formulas$formula, paste(". ~ . +", re_term)
      )
    }
  }

  # -- 8b. Consistency check: sre and sre_type must agree --------------------
  # Spatial random effects require BOTH `sre` (the area grouping column)
  # and `sre_type` ("car" or "sar") plus `M` (the weight matrix).
  # Providing only one of them is almost always a typo or an unfinished
  # call; we fail loudly with a precise suggestion so the user can fix the
  # issue immediately rather than getting a confusing downstream error
  # from brms.
  if (xor(is.null(sre), is.null(sre_type))) {
    if (is.null(sre_type)) {
      # User supplied sre but forgot sre_type
      stop(
        "`sre = \"", sre, "\"` was supplied but `sre_type` is NULL.\n",
        "  Spatial random effects require both `sre` (column name) ",
        "and `sre_type` (\"car\" or \"sar\") plus `M` (weight matrix).\n",
        "  Did you mean one of:\n",
        "    - SPATIAL CAR : sre = \"", sre, "\", sre_type = \"car\", M = W\n",
        "    - SPATIAL SAR : sre = \"", sre, "\", sre_type = \"sar\", M = W\n",
        "    - IID area RE: re = ~ (1 | ", sre, ")  (drop `sre`)\n",
        "    - No area RE : drop both `sre` and `sre_type`.",
        call. = FALSE
      )
    } else {
      # User supplied sre_type but forgot sre
      stop(
        "`sre_type = \"", sre_type, "\"` was supplied but `sre` is NULL.\n",
        "  Spatial random effects require `sre` to specify the column ",
        "in `data` that identifies the spatial areas.\n",
        "  Add `sre = \"<your_area_column>\"` (and `M = <weight_matrix>` ",
        "if not already supplied) to proceed.",
        call. = FALSE
      )
    }
  }

  # -- 8c. Soft warning: BYM-style decomposition recommended -----------------
  # When the user manually specifies BOTH an IID random effect on a column
  # AND a CAR spatial random effect on the SAME column, the resulting model
  # is the (parallel) BYM decomposition:
  #   y_i = X beta + u_i + s_i + e_i,  u_i ~ N(0, sigma_u^2),
  #                                    s_i ~ CAR(W, sigma_s^2).
  # This is mathematically valid and statistically common (heterogeneity +
  # clustering), but brms provides a more identifiable reparameterisation
  # via car_type = "bym2".  We emit a single informative message so the
  # user can switch to the recommended form if they wish.
  if (!is.null(re) && !is.null(sre) && !is.null(sre_type) &&
      identical(sre_type, "car")) {
    re_str_check <- paste(deparse(re), collapse = " ")
    # Match patterns like "(1 | g)" / "(1|g)" with optional spaces
    re_grp <- regmatches(
      re_str_check,
      regexpr("\\(\\s*1\\s*\\|\\s*[A-Za-z_][A-Za-z0-9_.]*\\s*\\)",
              re_str_check)
    )
    if (length(re_grp) > 0L) {
      # Extract the variable name inside (1 | xxx)
      re_var <- sub(".*\\|\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*\\).*",
                    "\\1", re_grp)
      if (identical(re_var, sre)) {
        message(
          "You have specified both `re = ~ (1 | ", sre, ")` and ",
          "`sre = \"", sre, "\"` with `sre_type = \"car\"`.\n",
          "  This fits the parallel BYM decomposition (IID + CAR on ",
          "the same area column), which is mathematically valid but can ",
          "have weakly identified variance components.\n",
          "  A more identifiable alternative is the BYM2 ",
          "reparameterisation: drop `re` and set ",
          "`sre_type = \"car\"`, `car_type = \"bym2\"`."
        )
      }
    }
  }

  # -- 9. Spatial random effects ------------------------------------------------
  if (!is.null(sre_type)) {
    M <- .validate_spatial_matrix(M, sre_type)
    data2 <- list(M = M)

    if (sre_type == "car") {
      car_t   <- if (is.null(car_type)) "icar" else car_type
      sre_str <- paste0("car(M, gr = ", sre, ", type = '", car_t, "')")
    } else if (sre_type == "sar") {
      sar_t   <- if (is.null(sar_type)) "lag" else sar_type
      sre_str <- paste0("sar(M, type = '", sar_t, "')")
    } else {
      stop("Invalid `sre_type`. Choose 'car' or 'sar'.", call. = FALSE)
    }

    if (!is.null(all_formulas$forms)) {
      all_formulas$forms <- lapply(all_formulas$forms, function(f) {
        f$formula <- stats::update(f$formula, paste(". ~ . +", sre_str))
        f
      })
    } else {
      all_formulas$formula <- stats::update(
        all_formulas$formula, paste(". ~ . +", sre_str)
      )
    }
  }

  # -- 9b. Sanity check: warn if NO area-level random structure is present ----
  # The Fay-Herriot SAE framework assumes u_i ~ N(0, sigma_u^2) per area.
  # Forgetting both `re` and `sre_type` reduces hbm() to a fixed-effects-only
  # regression, which is rarely what users want from a HBSAE package.  Emit
  # a single informative warning so the user can either confirm intent or
  # add the missing term.  Backward compatible: nothing is auto-injected
  # into the formula -- the user retains full control.
  if (is.null(re) && is.null(sre_type)) {
    warning(
      "Model fitted without any area-level random effects.\n",
      "  This is unusual for Small Area Estimation: the standard ",
      "Fay-Herriot model assumes u_i ~ N(0, sigma_u^2) per area, ",
      "so estimates from a purely fixed-effects model will not ",
      "borrow strength across areas.\n",
      "  Consider one of:\n",
      "    re = ~ (1 | area_id)                          # IID area RE\n",
      "    sre = 'area_id', sre_type = 'car', M = W       # CAR spatial RE\n",
      "    sre = 'area_id', sre_type = 'sar', M = W       # SAR spatial RE\n",
      "  If a fixed-effects-only baseline is intentional, you can ",
      "suppress this warning with `suppressWarnings()`.",
      call. = FALSE
    )
  }

  # -- 10. Attach distribution family -------------------------------------------
  # v0.6.0: hb_sampling can now be either:
  #   (a) a character key for a brms-native family (e.g. "gaussian", "Beta")
  #   (b) a character key for a registered brms::custom_family() in the
  #       hbsaems model registry (e.g. "loglogistic", "shifted_loglogistic")
  #   (c) a brms::customfamily object passed directly
  # We resolve the appropriate family object and merge any required
  # stanvars (only relevant for custom families).
  family_obj <- NULL

  if (inherits(hb_sampling, "customfamily")) {
    # Case (c): direct customfamily object
    family_obj <- hb_sampling
  } else if (is.character(hb_sampling)) {
    spec <- .get_model(hb_sampling)
    if (!is.null(spec) && .is_custom_family_spec(spec)) {
      # Case (b): registered custom family
      family_obj <- spec$custom_family
      # Merge custom stanvars with any user-supplied ones
      if (!is.null(spec$custom_stanvars)) {
        stanvars <- if (is.null(stanvars))
          spec$custom_stanvars
        else
          stanvars + spec$custom_stanvars
      }
    }
  }

  if (is.null(family_obj)) {
    # Case (a): fall back to brms-native family lookup
    family_obj <- brms::brmsfamily(hb_sampling, hb_link,
                                    link_phi = link_phi)
  }

  if (!is.null(all_formulas$forms)) {
    all_formulas$forms[[1L]]$family <- family_obj
  } else {
    all_formulas$family <- family_obj
  }

  # -- 10b. v1.0.0: Attach fixed-parameter pforms -------------------------------
  # For each pinned distributional parameter, append
  #   <par> ~ 0 + offset(.hbsaems_<par>_fixed)
  # to the brms formula.  This must happen AFTER the family is attached
  # (so brms knows which dpars are legal) and AFTER missing-data handling
  # (so multivariate / mi() formulas have been finalised).
  #
  # Conflict policy:  a parameter cannot be simultaneously pinned AND
  # given a prior or hyperprior, because the offset form
  # `<par> ~ 0 + offset(...)` removes the parameter from the sampler.
  # All such conflicts are caught here with informative errors.
  if (length(.fixed_params_processed$resolved) > 0L) {
    pinned_pars <- names(.fixed_params_processed$resolved)

    # 10b.i  No explicit prior on a pinned dpar.
    if (!is.null(prior) && inherits(prior, "brmsprior")) {
      conflict <- intersect(prior$class, pinned_pars)
      if (length(conflict) > 0L) {
        stop(
          "`prior` contains a prior on `", conflict[1L],
          "`, but this parameter is pinned via `fixed_params`. ",
          "A pinned parameter is removed from the sampler -- a prior on it ",
          "is ignored.  Remove the conflicting prior, or remove `",
          conflict[1L], "` from `fixed_params`.",
          call. = FALSE
        )
      }
    }

    # 10b.ii  No sampling statement in stanvars for a pinned dpar.
    # E.g. user pins phi via fixed_params and also writes
    # `stanvars = stanvar("phi ~ gamma(1,1);", block = "model")`.
    if (!is.null(stanvars)) {
      sv_vars  <- .extract_stanvar_model_targets(stanvars)
      conflict <- intersect(sv_vars, pinned_pars)
      if (length(conflict) > 0L) {
        stop(
          "`stanvars` contains a sampling statement targeting `",
          conflict[1L], "`, but this parameter is pinned via ",
          "`fixed_params` -- it is no longer a Stan parameter so any ",
          "sampling statement on it would fail at compile time.  ",
          "Remove the stanvars statement, or remove `", conflict[1L],
          "` from `fixed_params`.",
          call. = FALSE
        )
      }
    }

    all_formulas <- .add_fixed_pforms(all_formulas,
                                       .fixed_params_processed)
  }

  # -- 11. Check all required variables exist in data ---------------------------
  all_model_vars  <- setdiff(c(response_var, auxiliary_vars), "M")
  missing_in_data <- setdiff(all_model_vars, names(data))
  if (length(missing_in_data) > 0L) {
    stop(
      "Variable(s) referenced in formula not found in data: ",
      paste(missing_in_data, collapse = ", "),
      call. = FALSE
    )
  }

  # -- 12. Model fitting --------------------------------------------------------
  common_brm_args <- list(
    formula      = all_formulas,
    data2        = data2,
    prior        = prior,
    chains       = chains,
    iter         = iter,
    warmup       = warmup,
    cores        = cores,
    control      = control,
    sample_prior = sample_prior,
    save_pars    = brms::save_pars(all = TRUE),
    stanvars     = stanvars   # NULL by default; used by hbm_betalogitnorm()
  )

  if (multiple) {
    # -- 12a. Multiple imputation path ------------------------------------------
    # .mice_impute_x_only() guarantees Y is excluded from imputation and
    # returns the raw mids object from mice::mice().
    imp_result <- .mice_impute_x_only(
      data           = data,
      response_vars  = response_var,
      auxiliary_vars = auxiliary_vars,
      m              = m,
      mice_args      = mice_args
    )

    if (!imp_result$any_x_imputed) {
      # Edge case: all X were complete -- no imputation was needed.
      # Downgrade to a single brm() call on the training data.
      model <- do.call(
        brms::brm,
        c(list(data = imp_result$data_train), common_brm_args, list(...))
      )
    } else if (hb_sampling == "binomial" && length(response_var) >= 2L) {
      # Binomial post-processing: enforce y <= n across all imputed datasets.
      # Imputing predictors independently of the trials variable can violate
      # the constraint y <= n, so we clip y after extraction.
      # This requires a list of data frames; brm_multiple() will then treat
      # the list as m pre-imputed datasets.
      y_col        <- response_var[1L]
      n_col        <- response_var[2L]
      imputed_list <- mice::complete(imp_result$mids, action = "all")
      imputed_list <- lapply(imputed_list, function(d) {
        d[[y_col]] <- pmin(d[[y_col]], d[[n_col]])
        d
      })
      model <- do.call(
        brms::brm_multiple,
        c(list(data = imputed_list), common_brm_args, list(...))
      )
    } else {
      # Standard path: pass the mids object directly to brm_multiple().
      # brms::brm_multiple() detects the mids class and calls mice::complete()
      # internally, so we do not need to extract the imputed datasets here.
      # See: https://github.com/paul-buerkner/brms/blob/master/R/brm_multiple.R
      model <- do.call(
        brms::brm_multiple,
        c(list(data = imp_result$mids), common_brm_args, list(...))
      )
    }

  } else {
    # -- 12b. Single model path -------------------------------------------------
    model <- do.call(
      brms::brm,
      c(list(data = data), common_brm_args, list(...))
    )
  }

  # -- 13. Return hbmfit object -------------------------------------------------
  # $data is always the ORIGINAL data (before any deletion or imputation)
  # so that downstream functions (sae_predict, convergence_check, etc.) have
  # access to all rows -- including those with missing Y that were excluded
  # from model fitting but are still needed for area-level predictions.
  new_hbmfit(
    model          = model,
    missing_method = handle_missing,   # rename to canonical field name
    data           = data_complete
  )
}
