# =============================================================================

# Internal (v1.1.0): merge NUTS control defaults without clobbering the user.
# Hierarchical SAE funnels need a higher adapt_delta than brms's 0.8 default.
.merge_nuts_defaults <- function(control) {
  control <- if (is.null(control)) list() else control
  if (is.null(control$adapt_delta))   control$adapt_delta   <- 0.95
  if (is.null(control$max_treedepth)) control$max_treedepth <- 12L
  control
}

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
#'     Specified by \code{spatial_model = "car"}.  The joint distribution of the
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
#'     Specified by \code{spatial_model = "sar"}.  The model is
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
#' @param family The model family (primary argument, brms-consistent).
#'   Accepts a family name string (e.g.\ \code{"gaussian"}), a brms family
#'   object (e.g.\ \code{gaussian()}, \code{Gamma(link = "log")}), or a
#'   registered custom family object.  A link carried on the object is used
#'   when \code{hb_link} is left at its default.  Supply either \code{family}
#'   or its alias \code{hb_sampling}, not both.  \code{family} is the uniform
#'   way to choose the distribution across \code{hbm()} and
#'   \code{\link{hbm_flex}}; each function additionally keeps its historical
#'   alias (\code{hb_sampling} here, \code{family_key} in \code{hbm_flex}).
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
#'   \code{NULL} (default) \emph{and} \code{spatial_model} is also
#'   \code{NULL}, the function emits a warning recommending an area-level
#'   random effect, since a Fay-Herriot SAE model with neither IID nor
#'   spatial area effects degenerates to fixed-effects regression and does
#'   not borrow strength across areas.  The warning can be silenced with
#'   \code{suppressWarnings()} if a fixed-effects-only baseline is
#'   intentional.
#' @param spatial_var Character.  Name of the column in \code{data} that
#'   identifies the spatial areas (e.g. \code{"regency"} or
#'   \code{"province"}).  Must be supplied \emph{together with}
#'   \code{spatial_model} and \code{M}; providing only one of them is an
#'   error.  Distinct from \code{re}: \code{re} is a formula for IID
#'   random effects, whereas \code{spatial_var} is a column name (string)
#'   for the spatially-structured random effect.
#' @param spatial_model Character.  Type of spatial model: \code{"car"}
#'   (Conditional Autoregressive) or \code{"sar"} (Simultaneous
#'   Autoregressive).  Must be supplied \emph{together with}
#'   \code{spatial_var} and \code{M}; providing only one of them is an
#'   error.
#' @param car_type Character.  CAR subtype passed to \code{brms}: one of
#'   \code{"escar"} (exact sparse CAR), \code{"esicar"} (exact sparse
#'   intrinsic CAR), \code{"icar"} (intrinsic CAR), or \code{"bym2"}.
#'   Defaults to \code{"icar"} when \code{spatial_model = "car"}.
#' @param sar_type Character.  SAR subtype: \code{"lag"} (SAR of the response
#'   values) or \code{"error"} (SAR of the residuals).  Defaults to
#'   \code{"lag"} when \code{spatial_model = "sar"}.
#' @param M Spatial matrix supplied as \code{data2} to \code{brms}.  For CAR
#'   this must be a binary adjacency matrix; for SAR a spatial weight matrix.
#'   Row names must match the levels of \code{spatial_var}.
#' @param sre \strong{Deprecated.}  Use \code{spatial_var} instead.  Kept
#'   for backward compatibility; will be removed in v2.0.0.
#' @param sre_type \strong{Deprecated.}  Use \code{spatial_model} instead.
#'   Kept for backward compatibility; will be removed in v2.0.0.
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
#' @param sampling_variance Optional character.  Name of a column in
#'   \code{data} containing the \strong{known} sampling variance
#'   \eqn{D_i} for each area (the Fay-Herriot sugar).  When supplied,
#'   \eqn{\sigma_i = \sqrt{D_i}} is pinned via offset.  This is the
#'   canonical way to fit a Gaussian Fay-Herriot model: without it, the
#'   residual \eqn{\sigma} and the area-RE \eqn{\sigma_u} compete to
#'   explain the same variance and the model is unidentified, typically
#'   producing divergent transitions.  Equivalent to
#'   \code{fixed_params = list(sigma = sqrt(data[[<col>]]))}.
#'
#'   \strong{Family compatibility.} \code{sampling_variance} requires a
#'   continuous family whose response distribution has a residual scale
#'   parameter named \code{sigma}.  Supported: \code{gaussian} (the
#'   canonical Fay-Herriot case), \code{lognormal} (D must be on the
#'   log scale; see also \code{\link{hbm_lnln}}), \code{student},
#'   \code{skew_normal}, \code{exgaussian}, \code{asym_laplace}.  A
#'   helpful error is thrown when an incompatible family is supplied.
#'
#'   \strong{For non-Gaussian SAE families} the analogous pinning
#'   mechanism is family-specific:
#'   \itemize{
#'     \item \strong{Beta}: pin the precision \code{phi} via the survey
#'           design effect, e.g.\ \code{fixed_params = list(phi = ~ I(n/deff - 1))}
#'           (Liu 2009).  See \code{\link{hbm_betalogitnorm}}.
#'     \item \strong{Binomial}: sampling variability enters through the
#'           \code{trials} addition term, not through a separate
#'           \code{sigma}.  See \code{\link{hbm_binlogitnorm}}.
#'     \item \strong{Poisson, Gamma, Weibull}: variance is tied
#'           algebraically to the mean -- no separate scale parameter
#'           to pin.
#'   }
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
#'       \code{hs_scale_slab}, \code{hs_par_ratio}, and \code{hs_autoscale}.}
#'     \item{\code{"r2d2"}}{R2D2 prior (Zhang et al. 2022).  Places a prior
#'       directly on the model \eqn{R^2} and distributes explained variance
#'       across predictors via a Dirichlet decomposition.  Controlled by
#'       \code{r2d2_mean_R2}, \code{r2d2_prec_R2}, \code{r2d2_cons_D2},
#'       and \code{r2d2_autoscale}.}
#'   }
#'   If \code{prior} already contains a global \code{class = "b"} entry,
#'   \code{prior_type} is ignored and a warning is issued.
#'
#'   \strong{Cascading to smooth and GP terms.}  When the formula
#'   contains \code{s()} or \code{gp()} terms, the shrinkage prior is
#'   automatically extended to the corresponding parameter classes
#'   \code{"sds"} (spline SDs) and/or \code{"sdgp"} (GP SDs) using the
#'   brms-canonical \code{main = TRUE} pattern.  The resulting prior
#'   regularises ALL components jointly -- linear coefficients,
#'   nonlinear smooth wiggliness, and GP marginal variance -- which is
#'   the principled approach to global-local shrinkage in models that
#'   combine parametric and nonparametric components.
#' @param hs_df Numeric \eqn{> 0}.  Local half-\eqn{t} degrees of freedom for
#'   the horseshoe prior (default \code{1} = half-Cauchy).
#' @param hs_df_global Numeric \eqn{> 0}.  Global half-\eqn{t} degrees of
#'   freedom (default \code{1}).
#' @param hs_df_slab Numeric \eqn{> 0}.  Slab half-\eqn{t} degrees of
#'   freedom (default \code{4}).
#' @param hs_scale_global Numeric \eqn{> 0} or \code{NULL}.  Scale for the
#'   global half-\eqn{t} prior.  \code{NULL} (default) lets \code{brms}
#'   compute it automatically from the number of predictors via
#'   \code{hs_autoscale}.
#' @param hs_scale_slab Numeric \eqn{> 0}.  Scale for the slab component
#'   (default \code{2}).
#' @param hs_par_ratio Numeric \eqn{> 0} or \code{NULL}.  Expected ratio of
#'   non-zero to total coefficients.  \code{NULL} (default) treats all
#'   coefficients as potentially non-zero.
#' @param hs_autoscale Logical.  Whether \code{brms} should auto-scale the
#'   horseshoe prior using the residual SD \eqn{\sigma}.  Default
#'   \code{TRUE}; set to \code{FALSE} for non-continuous responses
#'   (binomial, Poisson, ...) where \eqn{\sigma} is not defined.
#' @param r2d2_mean_R2 Numeric in \eqn{(0, 1)}.  Prior mean of the model
#'   \eqn{R^2} (default \code{0.5}).
#' @param r2d2_prec_R2 Numeric \eqn{> 0}.  Prior precision of \eqn{R^2}
#'   (default \code{2}).  Higher values concentrate mass around
#'   \code{r2d2_mean_R2}.
#' @param r2d2_cons_D2 Numeric \eqn{> 0} or \code{NULL}.  Dirichlet
#'   concentration for the D2 component.  \code{NULL} (default) uses the
#'   \code{brms} default \code{0.5}.
#' @param r2d2_autoscale Logical.  Whether \code{brms} should auto-scale
#'   the R2D2 prior using \eqn{\sigma}.  Default \code{TRUE}; set to
#'   \code{FALSE} for non-continuous responses.
#'
#' @param nonlinear Character vector or \code{NULL}.  Names of predictor
#'   variables to model with a smooth nonlinear term.  Each listed variable
#'   is replaced in the formula RHS with \code{s(var)} (spline) or
#'   \code{gp(var)} (Gaussian process).  Variables not listed remain linear.
#'   Do \emph{not} also write \code{s(x)} in the formula when using this
#'   argument -- the modification is applied automatically.  Default \code{NULL}
#'   (all predictors remain linear).
#' @param nonlinear_type Character.  Smooth term family to use.  One of
#'   \code{"spline"} (default, penalised regression spline via
#'   \code{mgcv::s()}) or \code{"gp"} (Gaussian process via
#'   \code{brms::gp()}).
#' @param spline_k Integer.  Spline basis dimension (number of knots)
#'   passed to \code{mgcv::s(..., k = ...)}.  \code{-1L} (default) lets
#'   \pkg{mgcv} choose automatically.  For SAE typically \code{k = 8} to
#'   \code{15}.  Ignored when \code{nonlinear_type = "gp"}.
#' @param spline_bs Character.  Spline basis type passed to
#'   \code{mgcv::s(..., bs = ...)}.  Default \code{"tp"} (thin-plate
#'   regression spline, the \pkg{mgcv} default).  Common alternatives:
#'   \code{"cr"} (cubic regression spline; often more stable for SAE
#'   with correlated auxiliary variables), \code{"cs"} (cubic with
#'   shrinkage; allows variable selection), \code{"ps"} (P-splines).
#'   Ignored when \code{nonlinear_type = "gp"}.
#' @param gp_k Integer or \code{NA}.  Number of basis functions for the
#'   Hilbert-space approximate GP (Riutort-Mayol et al.\ 2020), passed
#'   to \code{brms::gp(..., k = ...)}.  \code{NA} (default) = exact GP
#'   which scales \eqn{O(n^3)} and is \strong{not recommended for}
#'   \eqn{n > 100} areas; an immediate warning is emitted in that case
#'   pointing to this argument.  Integer values \code{10}--\code{25}
#'   are typical for SAE and dramatically improve convergence and
#'   runtime.  Ignored when \code{nonlinear_type = "spline"}.
#' @param gp_cov Character.  GP covariance function passed to
#'   \code{brms::gp(..., cov = ...)}: \code{"exp_quad"} (squared
#'   exponential / RBF, default), \code{"matern15"} (Matern 3/2),
#'   \code{"matern25"} (Matern 5/2; often more numerically stable for
#'   SAE than RBF), or \code{"exponential"}.
#' @param gp_c Numeric \eqn{> 0} or \code{NULL}.  Hilbert-space GP
#'   boundary-scale factor passed to \code{brms::gp(..., c = ...)}.
#'   Default brms value is \eqn{5/4} (= 1.25); increase if the GP
#'   appears truncated at the domain boundaries.  Only relevant when
#'   \code{gp_k} is supplied.
#' @param gp_scale \strong{Deprecated.}  Use \code{gp_c} instead.  The
#'   old name suggested a length-scale interpretation but actually
#'   mapped to the HSGP boundary-scale factor.  Will be removed in v2.0.0.
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
#' @param measurement_error Optional named list specifying which
#'   auxiliary variables are measured with error and where to find
#'   their standard errors.  The list maps variable names to columns
#'   in \code{data} containing the SE, e.g.
#'   \code{measurement_error = list(x1 = "se_x1", x2 = "se_x2")}.
#'   Listed variables are wrapped on-the-fly with \code{mi(var, se_col)}
#'   in the brmsformula so that brms treats them as latent variables
#'   with a Gaussian measurement-error structure, following Ybarra and
#'   Lohr (2008).  Standard errors must be non-negative and have no
#'   missing values; \code{measurement_error} variables must be a
#'   subset of the model's auxiliary (linear) predictors.  When the
#'   user has already written \code{mi(...)} explicitly in the
#'   formula, the corresponding entries in \code{measurement_error}
#'   are detected and not duplicated.  Note that ME inflates the
#'   parameter space (each \emph{x_i} becomes latent), which slows
#'   sampling and increases the risk of divergent transitions,
#'   especially when combined with smooth terms (\code{nonlinear}).
#'   See the "Measurement error" section of the SAE vignette.
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
#' @section How to pin distributional parameters per family:
#' \pkg{hbsaems} exposes three layers for pinning distributional
#' parameters to known values, in increasing order of generality:
#' \enumerate{
#'   \item \strong{Family-specific sugar} (least typing, most readable).
#'         Each wrapper exposes a convenience argument that maps to a
#'         well-defined Fay-Herriot-style transformation of survey
#'         design quantities:
#'         \tabular{lll}{
#'           \strong{Wrapper} \tab \strong{Sugar argument} \tab \strong{Pinned dpar} \cr
#'           \code{hbm(..., hb_sampling = "gaussian")} \tab \code{sampling_variance = "D"}
#'             \tab \eqn{\sigma_i = \sqrt{D_i}} \cr
#'           \code{hbm_lnln()}                        \tab \code{sampling_variance = "psi"}
#'             \tab \eqn{\sigma_i = \sqrt{\psi_i}}  (on log scale) \cr
#'           \code{hbm_betalogitnorm()}               \tab \code{n = "n", deff = "deff"}
#'             \tab \eqn{\phi_i = n_i / \mathrm{deff}_i - 1}  (Liu 2009) \cr
#'           \code{hbm_binlogitnorm()}                \tab \code{trials = "n"}
#'             \tab (sampling variance built into the family) \cr
#'         }
#'   \item \strong{Universal \code{fixed_params}} (works everywhere).
#'         A named list pinning any distributional parameter -- accepts
#'         a column name (character), a scalar (numeric of length 1),
#'         a vector of length \code{nrow(data)}, or a one-sided
#'         formula (evaluated in \code{data}'s environment).  Examples:
#'         \itemize{
#'           \item \code{fixed_params = list(sigma = "D")} -- pin sigma to a column.
#'           \item \code{fixed_params = list(phi   = ~ I(n / deff - 1))} -- pin phi via formula.
#'           \item \code{fixed_params = list(nu    = 4)} -- pin Student-t df to a scalar.
#'         }
#'   \item \strong{Raw \code{stanvars}} (for power users authoring
#'         custom Stan code).  Direct injection of Stan code blocks --
#'         see \code{\link[brms]{stanvar}}.
#' }
#'
#' Sugar arguments are simply thin wrappers around layer 2: they
#' validate the survey-design inputs (no NAs, strictly positive) and
#' translate to \code{fixed_params} before delegating to the universal
#' machinery.  Hence the conflict policy below applies uniformly to
#' both sugar and explicit \code{fixed_params}.
#'
#' @section Conflict resolution between prior, prior_type, fixed_params, and stanvars:
#' \code{hbm()} provides four orthogonal mechanisms to influence the
#' prior / parameter specification of the underlying \pkg{brms} model:
#' \enumerate{
#'   \item \code{prior}        -- explicit \code{brmsprior} object(s).
#'   \item \code{prior_type}   -- global-local shrinkage prior on the
#'                                 regression coefficients (cascades to
#'                                 \code{"sds"} / \code{"sdgp"} when
#'                                 splines or GPs are present).
#'   \item \code{fixed_params} -- pin distributional parameters to known
#'                                 values via the offset trick.
#'   \item \code{stanvars}     -- inject custom Stan code blocks.
#' }
#'
#' Plus two family-specific \emph{sugar} arguments that translate to
#' \code{fixed_params} internally:
#' \itemize{
#'   \item \code{sampling_variance} (continuous families): pins
#'         \eqn{\sigma_i = \sqrt{D_i}}, equivalent to
#'         \code{fixed_params$sigma = sqrt(data$D)}.
#'   \item \code{n + deff} (\code{hbm_betalogitnorm}): pins
#'         \eqn{\phi_i = n_i / \mathrm{deff}_i - 1}, equivalent to
#'         \code{fixed_params$phi = n / deff - 1}.
#' }
#'
#' Combining these without rules in mind can produce unidentified models
#' or compile-time errors.  \code{hbm()} therefore enforces the
#' following \strong{conflict matrix}, where each cell describes what
#' happens when the row and column inputs both target the same
#' distributional parameter (e.g.\ both pin \code{sigma}):
#'
#' \tabular{lllll}{
#'                                  \tab \strong{fixed_params} \tab \strong{prior} \tab \strong{prior_type} \tab \strong{stanvars} \cr
#'   \strong{sampling_variance}    \tab error                 \tab error (transitive) \tab no overlap        \tab error (transitive) \cr
#'   \strong{n + deff}              \tab error                 \tab error (transitive) \tab no overlap        \tab error (transitive) \cr
#'   \strong{fixed_params}          \tab --                    \tab error (10b.i)      \tab no overlap        \tab error (10b.ii)     \cr
#'   \strong{prior}                 \tab error (10b.i)         \tab --                 \tab warning, user wins\tab no check needed     \cr
#'   \strong{prior_type}            \tab no overlap            \tab warning, user wins \tab --                 \tab no check needed     \cr
#'   \strong{stanvars}              \tab error (10b.ii)        \tab no check needed    \tab no check needed   \tab --                  \cr
#' }
#'
#' Resolution semantics in detail:
#' \itemize{
#'   \item \strong{\code{prior} vs \code{prior_type}.}  If the user
#'         supplies a \emph{global} (no \code{coef =}) prior on
#'         \code{class = "b"}, \code{"sds"}, or \code{"sdgp"},
#'         \code{prior_type} is silently dropped for that class and a
#'         warning is emitted.  Coefficient-specific user priors
#'         (\code{coef = "x1"}) are kept alongside the shrinkage prior
#'         without warning.
#'   \item \strong{\code{fixed_params} vs \code{prior}.}  A pinned
#'         parameter is removed from the sampler; supplying a prior on
#'         that same parameter therefore has no effect and is treated
#'         as a user error -- an informative \code{stop()} is issued.
#'   \item \strong{\code{fixed_params} vs \code{stanvars}.}  Same logic
#'         as above: a sampling statement in \code{stanvars} that
#'         targets a pinned parameter would fail at Stan compile time;
#'         \code{hbm()} catches this and stops with a clear message.
#'   \item \strong{Sugar vs \code{fixed_params} on the same parameter.}
#'         The sugar translators (\code{.translate_sampling_variance()},
#'         \code{.translate_n_deff_to_phi()}) error out if the user has
#'         also pre-populated \code{fixed_params} for the target
#'         parameter -- there should never be two pin sources for the
#'         same dpar.
#'   \item \strong{Sugar vs \code{prior} / \code{stanvars} transitively.}
#'         After the sugar -> \code{fixed_params} translation, the
#'         downstream \code{fixed_params}-vs-prior / -stanvars checks
#'         fire automatically.  E.g.\ \code{sampling_variance = "D"}
#'         plus \code{prior = set_prior("normal(0, 1)", class = "sigma")}
#'         errors via the \code{fixed_params} vs \code{prior} rule.
#' }
#' The intent is to fail fast and explicitly rather than silently
#' producing an unidentified or mis-specified model.
#'
#' @section Convergence advice for SAE practitioners:
#' Common convergence pathologies in hierarchical Bayesian SAE models
#' and how to address them.  Run \code{\link{convergence_check}()} after
#' fitting to inspect \eqn{\hat R}, effective sample size (ESS), and
#' divergent transitions.
#'
#' \strong{1. Default sampler settings (recommended starting point).}
#' \itemize{
#'   \item \code{chains = 4}, \code{iter = 4000}, \code{warmup = 2000}
#'   \item \code{control = list(adapt_delta = 0.95, max_treedepth = 12)}
#'   \item \code{cores = parallel::detectCores() - 1}
#' }
#'
#' \strong{2. Divergent transitions.}  Most common cause is the
#' \emph{funnel} geometry of hierarchical variance parameters.
#' \itemize{
#'   \item First-line: increase \code{adapt_delta} to \code{0.99}.
#'   \item If still diverging, increase \code{warmup} and consider a
#'         tighter prior on the area-level standard deviation
#'         (\code{sd} class), e.g.\ \code{set_prior("normal(0, 0.5)",
#'         class = "sd")}.
#'   \item For Beta/Binomial logit-normal models, prior on the random
#'         intercept SD should also be on the logit scale.
#'   \item \strong{Gaussian (Fay-Herriot) only.}  Always supply the
#'         known sampling variance via \code{sampling_variance =
#'         "<col>"}.  Without this, the residual \eqn{\sigma} and the
#'         area-RE \eqn{\sigma_u} compete to explain the same variance
#'         component, producing weak identifiability and divergent
#'         transitions almost regardless of \code{adapt_delta}.  This
#'         is the single most common cause of divergences in
#'         Fay-Herriot fits and should be checked \emph{first} before
#'         any sampler-tuning.
#' }
#'
#' \strong{3. Low effective sample size (ESS < 1000).}
#' \itemize{
#'   \item Increase \code{iter} (e.g.\ to \code{6000}); this is the
#'         single most reliable fix.
#'   \item Centre and scale the auxiliary variables before fitting.
#'   \item Check for prior--data conflict via \pkg{priorsense}; see
#'         \code{?prior_check}.
#' }
#'
#' \strong{4. Gaussian processes.}
#' \itemize{
#'   \item \strong{Exact GP scales \eqn{O(n^3)}.}  For \eqn{n > 100}
#'         areas, set \code{gp_k} to use the Hilbert-space approximate
#'         GP (Riutort-Mayol et al.\ 2020).  A heuristic is
#'         \code{gp_k = ceiling(min(n / 5, 25))}.
#'   \item Try \code{gp_cov = "matern25"} (Matern 5/2) if the default
#'         squared-exponential covariance is numerically unstable.
#'   \item The boundary-scale factor \code{gp_c} (brms default 1.25)
#'         may need increasing if the posterior GP is truncated at
#'         the domain edges.
#' }
#'
#' \strong{5. Splines.}
#' \itemize{
#'   \item Start with \code{spline_k = -1} (auto).  Increase only if
#'         the residual diagnostics suggest under-smoothing.
#'   \item For strongly correlated auxiliary variables, try
#'         \code{spline_bs = "cr"} (cubic regression spline) for
#'         better numerical stability than the default thin-plate.
#'   \item For variable selection, use \code{spline_bs = "cs"} (cubic
#'         with shrinkage); coefficients on irrelevant smooths shrink
#'         toward zero.
#' }
#'
#' \strong{6. Spatial models.}
#' \itemize{
#'   \item For CAR, \code{car_type = "bym2"} (Riebler et al.\ 2016) is
#'         the modern recommendation; it stabilises the
#'         spatial/IID decomposition via a single mixing parameter.
#'   \item Verify the weight matrix with
#'         \code{\link{check_spatial_weight}()}; isolated areas or
#'         multiple disconnected components cause non-identifiability.
#' }
#'
#' \strong{7. Prior predictive check first.}  Always call
#' \code{\link{prior_check}()} (\code{sample_prior = "only"}) before
#' the full posterior run.  Implausible prior predictives are the
#' single most common cause of slow / divergent sampling.
#'
#' @references
#' Rao, J. N. K., & Molina, I. (2015). \emph{Small Area Estimation}.
#' John Wiley & Sons.
#'
#' Burkner, P. C. (2017). brms: An R package for Bayesian multilevel models
#' using Stan. \emph{Journal of Statistical Software}, 80(1), 1--28.
#'
#' Riutort-Mayol, G., Burkner, P.-C., Andersen, M. R., Solin, A., &
#' Vehtari, A. (2023).  Practical Hilbert space approximate Bayesian
#' Gaussian processes for probabilistic programming.
#' \emph{Statistics and Computing}, 33, 17.
#' \doi{10.1007/s11222-022-10167-2}
#'
#' Riebler, A., Sorbye, S. H., Simpson, D., & Rue, H. (2016).  An
#' intuitive Bayesian spatial model for disease mapping that accounts
#' for scaling.  \emph{Statistical Methods in Medical Research},
#' 25(4), 1145--1165.
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
#' # Standard brms-default MCMC settings used throughout these
#' # examples (chains = 4, iter = 2000, warmup = 1000).  For tougher
#' # posteriors (funnel geometry, weakly identified priors), bump to
#' # iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99).
#' FAST <- list(chains = 4, iter = 2000, warmup = 1000, cores = 1,
#'              seed = 123, refresh = 0)
#'
#' # -- Basic model --------------------------------------------------------------
#' model <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        hb_sampling = "gaussian",
#'        hb_link     = "identity",
#'        re          = ~(1 | regency),
#'        data        = data),
#'   FAST
#' ))
#' summary(model)
#'
#' # -- Horseshoe prior (sparse coefficients) ------------------------------------
#' model_hs <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        re          = ~(1 | regency),
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
#'        re           = ~(1 | regency),
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
#'        re             = ~(1 | regency),
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
#'        re             = ~(1 | regency),
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
#'        re             = ~(1 | regency),
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
#'        re             = ~(1 | regency),
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
#'        re             = ~(1 | regency),
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
#'        spatial_var = "province",
#'        spatial_model    = "car",
#'        M           = adjacency_matrix_car),
#'   FAST
#' ))
#' summary(model_car)
#'
#' # -- Spatial: SAR (Simultaneous Autoregressive) -------------------------------
#' # spatial_weight_sar is a 100x100 row-standardised matrix with row-
#' # names regency_001..regency_100, so it pairs with the fine-grained
#' # "regency" column (100 levels) -- NOT with "province" (5 levels).
#' data("spatial_weight_sar")
#' model_sar <- do.call(hbm, c(
#'   list(formula     = bf(y ~ x1 + x2 + x3),
#'        data        = data,
#'        spatial_var = "regency",
#'        spatial_model    = "sar",
#'        M           = spatial_weight_sar),
#'   FAST
#' ))
#' summary(model_sar)
#' }
hbm <- function(formula,
                family         = NULL,
                hb_sampling    = "gaussian",
                hb_link        = "identity",
                link_phi       = "log",
                re             = NULL,
                spatial_var    = NULL,
                spatial_model  = NULL,
                car_type       = NULL,
                sar_type       = NULL,
                M              = NULL,
                data,
                prior          = NULL,
                # -- Fixed-value distributional parameters ------------
                fixed_params   = NULL,
                # -- Fay-Herriot sampling variance (Gaussian) ---------
                sampling_variance = NULL,
                # -- Shrinkage priors (Feature 1) -----------------------------
                prior_type      = "default",
                hs_df           = 1,
                hs_df_global    = 1,
                hs_df_slab      = 4,
                hs_scale_global = NULL,
                hs_scale_slab   = 2,
                hs_par_ratio    = NULL,
                hs_autoscale    = TRUE,
                # R2D2-prior arguments
                r2d2_mean_R2    = 0.5,
                r2d2_prec_R2    = 2,
                r2d2_cons_D2    = NULL,
                r2d2_autoscale  = TRUE,
                # -- Nonlinear smooth terms (Feature 2) -----------------------
                nonlinear       = NULL,
                nonlinear_type  = "spline",
                spline_k        = -1L,
                spline_bs       = "tp",
                gp_k            = NA_integer_,
                gp_cov          = "exp_quad",
                gp_c            = NULL,
                gp_scale        = NULL,   # deprecated alias for gp_c
                # -- Missing data ---------------------------------------------
                handle_missing = NULL,
                m              = 5L,
                mice_args      = list(),
                # -- Measurement error on auxiliary variables (v1.0.0) --
                measurement_error = NULL,
                control        = list(),
                chains         = 4L,
                iter           = 4000L,
                warmup         = floor(iter / 2),
                cores          = 1L,
                sample_prior   = "no",
                # -- Deprecated argument aliases (v1.0.0) ---------------------
                sre            = NULL,
                sre_type       = NULL,
                stanvars       = NULL,
                ...) {

  # -- Intercept hbm_config bundles in ... ----------------------------
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
  # family alias for hb_sampling (v1.1.0): resolve here, AFTER the bundle
  # recursion above, so it runs exactly once on the final call.  `family` is
  # the primary, brms-consistent selector; `hb_sampling` is the kept alias.
  #   * string (e.g. "gaussian")            -> used as hb_sampling
  #   * brms family object (e.g. gaussian()) -> name + link extracted
  #   * registered/custom family object      -> passed through (hb_sampling
  #                                             already accepts customfamily)
  if (!is.null(family)) {
    if (!identical(hb_sampling, "gaussian"))
      stop("Supply either `family` or `hb_sampling`, not both.", call. = FALSE)
    if (is.character(family) && length(family) == 1L) {
      hb_sampling <- family
    } else if (inherits(family, "customfamily")) {
      hb_sampling <- family
    } else if (inherits(family, "brmsfamily") || inherits(family, "family")) {
      fam_name <- family$family %||% NA_character_
      if (is.na(fam_name) || !nzchar(fam_name))
        stop("Could not determine the family name from the supplied ",
             "`family` object.", call. = FALSE)
      hb_sampling <- fam_name
      if (!is.null(family$link) && identical(hb_link, "identity"))
        hb_link <- family$link
    } else {
      stop("`family` must be a family name (string) or a brms family object ",
           "such as gaussian(), Gamma(), binomial(), or a registered custom ",
           "family.", call. = FALSE)
    }
  }

  # Delegated to internal helper for testability and clarity.
  data  <- .validate_hbm_data(data)
  n     <- nrow(data)
  data2 <- NULL

  # -- 0a. Argument deprecation (v1.0.0): sre -> spatial_var, sre_type -> spatial_model
  # Old names continue to work for one release cycle; users see a single
  # informative warning per call.  Removal scheduled for v2.0.0.
  if (!is.null(sre)) {
    if (!is.null(spatial_var)) {
      stop(
        "Both `spatial_var` and the deprecated alias `sre` were supplied.\n",
        "  Use `spatial_var` only.",
        call. = FALSE
      )
    }
    warning(
      "Argument `sre` is deprecated as of hbsaems 1.0.0 -- use ",
      "`spatial_var` instead.  Scheduled for removal in v2.0.0.",
      call. = FALSE
    )
    spatial_var <- sre
  }
  if (!is.null(sre_type)) {
    if (!is.null(spatial_model)) {
      stop(
        "Both `spatial_model` and the deprecated alias `sre_type` were ",
        "supplied.\n  Use `spatial_model` only.",
        call. = FALSE
      )
    }
    warning(
      "Argument `sre_type` is deprecated as of hbsaems 1.0.0 -- use ",
      "`spatial_model` instead.  Scheduled for removal in v2.0.0.",
      call. = FALSE
    )
    spatial_model <- sre_type
  }

  # -- 0c. Translate sampling_variance to fixed_params$sigma -------------------
  # `sampling_variance = "<col>"` is the Fay-Herriot sugar that pins
  # sigma_i to sqrt(D_i) for each area, recovering the canonical
  # Fay-Herriot identifiability.  Only meaningful for continuous
  # families whose response distribution has a residual SCALE parameter
  # named `sigma` (e.g. gaussian, lognormal, student, skew_normal,
  # exgaussian, asym_laplace).  For Beta / Binomial / Poisson families
  # the variance is tied to the mean and cannot be pinned this way; for
  # those, use `fixed_params$phi` (Beta, via design effect) or the
  # `trials` argument (Binomial) instead.
  if (!is.null(sampling_variance)) {
    # Family compatibility check (happens BEFORE the translation so we
    # produce an informative error rather than a cryptic Stan error).
    families_with_sigma <- c(
      "gaussian", "lognormal", "student", "skew_normal",
      "exgaussian", "asym_laplace"
    )
    if (!hb_sampling %in% families_with_sigma) {
      stop(
        "`sampling_variance` requires a family with a residual SD parameter ",
        "named `sigma`.\n",
        "  Supplied family: '", hb_sampling, "'\n",
        "  Compatible:      ", paste(shQuote(families_with_sigma),
                                       collapse = ", "), "\n",
        "  For Beta:        pin precision via `fixed_params = list(phi = ~ I(n/deff - 1))` (Liu 2009).\n",
        "  For Binomial:    sampling variance enters via the `trials = ` argument.\n",
        "  For Poisson:     variance is tied to mean -- no offset needed.",
        call. = FALSE
      )
    }

    # Translate via the centralised helper.  Validation lives there too.
    fixed_params <- .translate_sampling_variance(
      fixed_params      = fixed_params,
      sampling_variance = sampling_variance,
      data              = data
    )
  }

  # -- 0b. Process fixed_params -----------------------------------------
  # Resolve user-specified pinned parameters (column name / scalar / vector /
  # formula) to numeric vectors and attach them as columns of `data` named
  # .hbsaems_<par>_fixed.  Conflict / type / NA checks fail loudly here.
  .fixed_params_processed <- .process_fixed_params(fixed_params, data)
  if (length(.fixed_params_processed$resolved) > 0L) {
    data <- .attach_fixed_columns(data, .fixed_params_processed)
  }

  # -- 1. Formula parsing -------------------------------------------------------
  # Delegated to .parse_hbm_formula() helper (v1.0.0).
  parsed <- .parse_hbm_formula(formula)
  main_formula   <- parsed$main_formula
  all_formulas   <- parsed$all_formulas
  response_var   <- parsed$response_var
  auxiliary_vars <- parsed$auxiliary_vars

  # -- 1a. Detect explicit mi() / me() usage in the user-supplied formula -------
  # (v1.0.0) when the user writes `mi(x, sdx)` or `me(x, sdx)` directly
  # in the brmsformula they are explicitly opting in to brms's measurement-
  # error / joint-modelling framework.  Two consequences:
  #   * NA in `x` is intentional (brms handles imputation internally); we
  #     must NOT force the user to set handle_missing or drop rows.
  #   * We should silently route to `handle_missing = "model"` so that
  #     downstream code preserves the original data.
  user_has_mi <- .formula_has_mi(formula)

  # -- 1b. measurement_error sugar (v1.0.0) -------------------------------
  # Validate and apply.  When the user passes `measurement_error = list(x = "se_x")`,
  # we rewrite the RHS so that the listed variables are wrapped with mi(x, se_x).
  # The auxiliary-variable names referenced here must be PRESENT in the
  # current formula RHS as bare names; if they already appear inside mi() the
  # transformation is skipped (the user's explicit syntax wins).
  if (!is.null(measurement_error)) {
    .validate_measurement_error(measurement_error, auxiliary_vars, data)
    main_formula <- .apply_measurement_error(main_formula, measurement_error)
    formula      <- .apply_measurement_error(formula,      measurement_error)
    # Refresh parsed metadata after the rewrite
    parsed         <- .parse_hbm_formula(formula)
    main_formula   <- parsed$main_formula
    all_formulas   <- parsed$all_formulas
    response_var   <- parsed$response_var
    auxiliary_vars <- parsed$auxiliary_vars
    # Re-detect mi() since we just inserted it
    user_has_mi    <- TRUE
  }

  # -- 1b. Apply nonlinear smooth terms (Feature 2) -----------------------------
  # Replace listed predictor variables in the formula RHS with s(var) (spline)
  # or gp(var) (Gaussian process) terms.  This must happen before the
  # handle_missing block so that mi() wrappers added by .add_mi_to_lhs() operate
  # on a formula that already contains the correct smooth-term syntax.

  # Deprecated `gp_scale` alias (v1.0.0) -- handle before the nonlinear block
  # so the value is available regardless of whether nonlinear terms are used.
  if (!is.null(gp_scale)) {
    if (!is.null(gp_c))
      stop("Pass either `gp_c` (preferred) or `gp_scale` (deprecated), ",
           "but not both.", call. = FALSE)
    .deprecate_arg("gp_scale", "gp_c", "v2.0.0")
    gp_c <- gp_scale
  }

  if (!is.null(nonlinear) && length(nonlinear) > 0L) {
    nonlinear_type <- match.arg(nonlinear_type, c("spline", "gp"))

    .validate_nonlinear(nonlinear, auxiliary_vars, data)

    if (nonlinear_type == "spline" && !requireNamespace("mgcv", quietly = TRUE))
      stop(
        "Package 'mgcv' is required for nonlinear_type = 'spline'. ",
        "Install it with: install.packages('mgcv')",
        call. = FALSE
      )

    # Warn against exact GP for n > 100 (common SAE pitfall)
    n_obs <- nrow(data)
    if (nonlinear_type == "gp" && is.na(gp_k) && n_obs > 100L) {
      warning(
        "Exact GP requested for n = ", n_obs, " observations. ",
        "Exact GP scales O(n^3) and typically diverges or hits ",
        "max_treedepth above ~100 areas. Consider setting `gp_k` to ",
        "use the Hilbert-space approximate GP (Riutort-Mayol et al. 2020), ",
        "e.g. gp_k = ", min(25L, ceiling(n_obs / 5)), ".",
        call. = FALSE, immediate. = TRUE
      )
    }

    all_formulas <- .apply_nonlinear_to_formula(
      formula        = all_formulas,
      nonlinear      = nonlinear,
      nonlinear_type = nonlinear_type,
      spline_k       = spline_k,
      spline_bs      = spline_bs,
      gp_k           = gp_k,
      gp_cov         = gp_cov,
      gp_c           = gp_c
    )
  }

  # -- 2. Detect missing values -------------------------------------------------
  missing_info <- .detect_missing(data, response_var, auxiliary_vars)
  missing_y    <- missing_info$missing_y   # NULL or character vector
  missing_x    <- missing_info$missing_x   # NULL or character vector

  # -- 3. Discrete-family flag ---------------------------------------------------
  # lookup the family registry instead of using a hardcoded list.
  # Falls back to a static list for the few brms families not (yet)
  # registered, so behaviour matches v1.0.0 exactly when the registry has
  # not been touched.
  is_discrete <- isTRUE(.model_is_discrete(hb_sampling)) ||
    hb_sampling %in% c("cumulative", "cratio", "sratio", "acat")

  # -- 4. Validate handle_missing -----------------------------------------------
  has_any_missing <- !is.null(missing_y) || !is.null(missing_x)

  # (v1.0.0): explicit mi()/me() in user's formula => silently engage model-based
  # imputation mode and skip the eager error.  The user knows what they want.
  if (user_has_mi && is.null(handle_missing)) {
    handle_missing <- "model"
  }

  if (has_any_missing && !user_has_mi) {
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
          # Note: re and spatial terms are added to the formula LATER in
          # this function, so .add_mi_to_lhs() only needs to handle the
          # base formula structure -- it will not interfere with those
          # terms.
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

    # Use the full-cascade helper which detects splines / GP terms in the
    # formula and adds matching priors on classes "sds" and "sdgp" with
    # the brms-canonical `main = TRUE` pattern (Buerkner 2024).
    type_prior <- .build_shrinkage_priors_full(
      formula         = all_formulas,
      prior_type      = prior_type,
      hs_df           = hs_df,
      hs_df_global    = hs_df_global,
      hs_df_slab      = hs_df_slab,
      hs_scale_global = hs_scale_global,
      hs_scale_slab   = hs_scale_slab,
      hs_par_ratio    = hs_par_ratio,
      hs_autoscale    = hs_autoscale,
      r2d2_mean_R2    = r2d2_mean_R2,
      r2d2_prec_R2    = r2d2_prec_R2,
      r2d2_cons_D2    = r2d2_cons_D2,
      r2d2_autoscale  = r2d2_autoscale
    )

    prior <- .merge_prior_type(prior, type_prior)
  }

  # -- 8. Random effects --------------------------------------------------------
  if (!is.null(re)) {
    re_str       <- as.character(re)
    # Accepts brms/lme4 random-effect syntaxes:
    #   (1 | g)            single grouping factor
    #   (1 | g1 + g2)      multiple grouping factors (crossed within a term)
    #   (1 | g1 / g2)      nested grouping factors  -> brms sugar for
    #                      (1 | g1) + (1 | g1:g2)
    # plus any number of additional `(1 | ...)` terms joined by `+`.
    valid_re_pat <- paste0(
      "^\\(\\s*1\\s*\\|\\s*\\w+(\\s*[+/]\\s*\\w+)*\\s*\\)",
      "(\\s*\\+\\s*\\(\\s*1\\s*\\|\\s*\\w+(\\s*[+/]\\s*\\w+)*\\s*\\))*\\s*$"
    )
    if (!grepl(valid_re_pat, re_str[2L])) {
      stop(
        "Invalid `re` formula. Expected one of:\n",
        "  ~ (1|group)\n",
        "  ~ (1|group1) + (1|group2)\n",
        "  ~ (1|group1/group2)           # nested\n",
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

  # -- 8b. Consistency check: spatial_var and spatial_model must agree --------
  # Spatial random effects require BOTH `spatial_var` (the area grouping
  # column) and `spatial_model` ("car" or "sar") plus `M` (the weight matrix).
  # Providing only one of them is almost always a typo or an unfinished
  # call; we fail loudly with a precise suggestion so the user can fix the
  # issue immediately rather than getting a confusing downstream error
  # from brms.
  if (xor(is.null(spatial_var), is.null(spatial_model))) {
    if (is.null(spatial_model)) {
      # User supplied spatial_var but forgot spatial_model
      stop(
        "`spatial_var = \"", spatial_var, "\"` was supplied but ",
        "`spatial_model` is NULL.\n",
        "  Spatial random effects require both `spatial_var` (column name) ",
        "and `spatial_model` (\"car\" or \"sar\") plus `M` (weight matrix).\n",
        "  Did you mean one of:\n",
        "    - SPATIAL CAR : spatial_var = \"", spatial_var,
        "\", spatial_model = \"car\", M = W\n",
        "    - SPATIAL SAR : spatial_var = \"", spatial_var,
        "\", spatial_model = \"sar\", M = W\n",
        "    - IID area RE: re = ~ (1 | ", spatial_var,
        ")  (drop `spatial_var`)\n",
        "    - No area RE : drop both `spatial_var` and `spatial_model`.",
        call. = FALSE
      )
    } else {
      # User supplied spatial_model but forgot spatial_var
      stop(
        "`spatial_model = \"", spatial_model, "\"` was supplied but ",
        "`spatial_var` is NULL.\n",
        "  Spatial random effects require `spatial_var` to specify the ",
        "column in `data` that identifies the spatial areas.\n",
        "  Add `spatial_var = \"<your_area_column>\"` (and ",
        "`M = <weight_matrix>` if not already supplied) to proceed.",
        call. = FALSE
      )
    }
  }

  # -- 8b-ii. Consistency check: car_type / sar_type need matching spatial_model
  # `car_type` is meaningful only with `spatial_model = "car"`; `sar_type`
  # only with `spatial_model = "sar"`.  Silently ignoring these would mask a
  # typo (e.g. user wants BYM2 but forgot `spatial_model = "car"`).
  if (!is.null(car_type) && !identical(spatial_model, "car")) {
    stop(
      "`car_type = \"", car_type, "\"` was supplied but ",
      if (is.null(spatial_model)) "`spatial_model` is NULL" else
        paste0("`spatial_model = \"", spatial_model, "\"`"),
      ".\n",
      "  `car_type` (\"icar\", \"escar\", \"esicar\", \"bym2\") is only valid ",
      "with `spatial_model = \"car\"`.\n",
      "  Did you mean `spatial_model = \"car\"`?",
      call. = FALSE
    )
  }
  if (!is.null(sar_type) && !identical(spatial_model, "sar")) {
    stop(
      "`sar_type = \"", sar_type, "\"` was supplied but ",
      if (is.null(spatial_model)) "`spatial_model` is NULL" else
        paste0("`spatial_model = \"", spatial_model, "\"`"),
      ".\n",
      "  `sar_type` (\"lag\", \"error\") is only valid with ",
      "`spatial_model = \"sar\"`.\n",
      "  Did you mean `spatial_model = \"sar\"`?",
      call. = FALSE
    )
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
  if (!is.null(re) && !is.null(spatial_var) && !is.null(spatial_model) &&
      identical(spatial_model, "car")) {
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
      if (identical(re_var, spatial_var)) {
        message(
          "You have specified both `re = ~ (1 | ", spatial_var, ")` and ",
          "`spatial_var = \"", spatial_var, "\"` with ",
          "`spatial_model = \"car\"`.\n",
          "  This fits the parallel BYM decomposition (IID + CAR on ",
          "the same area column), which is mathematically valid but can ",
          "have weakly identified variance components.\n",
          "  A more identifiable alternative is the BYM2 ",
          "reparameterisation: drop `re` and set ",
          "`spatial_model = \"car\"`, `car_type = \"bym2\"`."
        )
      }
    }
  }

  # -- 9. Spatial random effects ------------------------------------------------
  if (!is.null(spatial_model)) {
    # For CAR, give the validator the grouping levels so a rowname/level
    # mismatch is caught here rather than during brms Stan-data prep.
    grp_levels <- if (identical(spatial_model, "car") &&
                      !is.null(spatial_var) &&
                      spatial_var %in% names(data))
      levels(factor(data[[spatial_var]])) else NULL
    M <- .validate_spatial_matrix(M, spatial_model,
                                  grouping_levels = grp_levels)
    data2 <- list(M = M)

    if (spatial_model == "car") {
      car_t        <- if (is.null(car_type)) "icar" else car_type
      spatial_term <- paste0("car(M, gr = ", spatial_var,
                              ", type = '", car_t, "')")
    } else if (spatial_model == "sar") {
      sar_t        <- if (is.null(sar_type)) "lag" else sar_type
      spatial_term <- paste0("sar(M, type = '", sar_t, "')")
    } else {
      stop("Invalid `spatial_model`. Choose 'car' or 'sar'.", call. = FALSE)
    }

    if (!is.null(all_formulas$forms)) {
      all_formulas$forms <- lapply(all_formulas$forms, function(f) {
        f$formula <- stats::update(f$formula, paste(". ~ . +", spatial_term))
        f
      })
    } else {
      all_formulas$formula <- stats::update(
        all_formulas$formula, paste(". ~ . +", spatial_term)
      )
    }
  }

  # -- 9b. Sanity check: warn if NO area-level random structure is present ----
  # The Fay-Herriot SAE framework assumes u_i ~ N(0, sigma_u^2) per area.
  # Forgetting both `re` and `spatial_model` reduces hbm() to a
  # fixed-effects-only regression, which is rarely what users want from a
  # HBSAE package.  Emit a single informative warning so the user can either
  # confirm intent or add the missing term.  Backward compatible: nothing is
  # auto-injected into the formula -- the user retains full control.
  if (is.null(re) && is.null(spatial_model)) {
    warning(
      "Model fitted without any area-level random effects.\n",
      "  This is unusual for Small Area Estimation: the standard ",
      "Fay-Herriot model assumes u_i ~ N(0, sigma_u^2) per area, ",
      "so estimates from a purely fixed-effects model will not ",
      "borrow strength across areas.\n",
      "  Consider one of:\n",
      "    re = ~ (1 | area_id)                                     # IID area RE\n",
      "    spatial_var = 'area_id', spatial_model = 'car', M = W    # CAR spatial RE\n",
      "    spatial_var = 'area_id', spatial_model = 'sar', M = W    # SAR spatial RE\n",
      "  If a fixed-effects-only baseline is intentional, you can ",
      "suppress this warning with `suppressWarnings()`.",
      call. = FALSE
    )
  }

  # -- 10. Attach distribution family -------------------------------------------
  # hb_sampling can now be either:
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

  # -- 10a. Override default links for pinned distributional parameters --
  # Critical: brms applies the dpar's LINK FUNCTION to the linear predictor
  # before plugging it into the likelihood.  For Gaussian / Lognormal /
  # Student / etc., brms's default `link_sigma = "log"` means the model
  # block computes `sigma = exp(offset_sigma)`.  Since our offset already
  # carries the SCALE value sqrt(D) on the natural (untransformed) scale,
  # the exponentiation would corrupt sigma -- e.g. sqrt(D)=2.0 would
  # become sigma=exp(2.0)=7.39.  To prevent this, we force the relevant
  # link to "identity" for every pinned dpar.
  #
  # The mapping below covers all dpars hbsaems can pin via sugar
  # (sigma via `sampling_variance`, phi via `n + deff`) and via the
  # generic `fixed_params` mechanism.  We only intervene when:
  #   * the user did not already supply a `link_<par>` (we respect their
  #     explicit choice -- they might be doing something exotic), and
  #   * the family actually exposes `link_<par>` (brms is silent on
  #     unknown link args, but we check defensively).
  if (length(.fixed_params_processed$resolved) > 0L) {
    pinned_dpars <- names(.fixed_params_processed$resolved)
    for (dpar in pinned_dpars) {
      link_key <- paste0("link_", dpar)
      # Only override if family currently uses a non-identity link for this dpar.
      # The family_obj is a brmsfamily object with a `link_<par>` element.
      current_link <- family_obj[[link_key]]
      if (!is.null(current_link) && current_link != "identity") {
        # phi was already correctly resolved in hbm_betalogitnorm() (which
        # sets link_phi = "identity" when fixed_params$phi is set), but
        # for sigma in hbm() we need to do it here.  Re-build the family
        # with the identity link substituted in.
        family_obj[[link_key]] <- "identity"
      }
    }
  }

  if (!is.null(all_formulas$forms)) {
    all_formulas$forms[[1L]]$family <- family_obj
  } else {
    all_formulas$family <- family_obj
  }

  # -- 10b. Attach fixed-parameter pforms -------------------------------
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
  # Build the named arg list to pass to brm()/brm_multiple().
  # We separate explicit hbsaems arguments from user-supplied ...
  # to handle collisions cleanly:
  #
  #   * `data2` may be set internally (CAR/SAR spatial weight matrix)
  #     AND/OR passed via ... by the user.  Merge the two lists --
  #     hbsaems-set keys win for the spatial matrix slot M, but user
  #     additions for other keys (e.g. their own auxiliary matrices)
  #     are preserved.
  #   * Other args (`formula`, `prior`, `chains`, etc.) cannot be
  #     overridden via ... -- they are derived from explicit hbsaems
  #     arguments; passing them via ... would be a user error.
  #
  # `do.call(..., c(list_a, list_b))` errors loudly on duplicate names,
  # so we deduplicate up front.
  dots_brm <- list(...)
  user_data2 <- dots_brm$data2
  dots_brm$data2 <- NULL                      # remove to avoid duplicate
  if (!is.null(user_data2)) {
    if (!is.list(user_data2))
      stop("`data2` supplied via `...` must be a named list.",
           call. = FALSE)
    # Merge: hbsaems wins for keys we set ourselves (currently `M`).
    data2 <- utils::modifyList(user_data2, as.list(data2 %||% list()))
  }

  # -- NUTS sampler defaults (v1.1.0) ----------------------------------------
  # Hierarchical SAE posteriors typically exhibit a funnel geometry, for
  # which brms's stock adapt_delta = 0.8 routinely produces avoidable
  # divergent transitions.  We raise the defaults to the values long
  # documented in ?hbm ("recommended starting point").  User-supplied
  # values are respected: we only fill in keys the user left unset.
  control <- .merge_nuts_defaults(control)

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

  # Warn loudly if user tries to override any internally-managed arg via ...
  managed_args <- intersect(names(common_brm_args), names(dots_brm))
  if (length(managed_args) > 0L)
    stop(
      "The following argument(s) supplied via `...` conflict with values ",
      "hbm() sets internally: ",
      paste(shQuote(managed_args), collapse = ", "), ". ",
      "Remove them from the call (or pass them through their dedicated ",
      "hbsaems argument, if any).",
      call. = FALSE
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
        c(list(data = imp_result$data_train), common_brm_args, dots_brm)
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
        c(list(data = imputed_list), common_brm_args, dots_brm)
      )
    } else {
      # Standard path: pass the mids object directly to brm_multiple().
      # brms::brm_multiple() detects the mids class and calls mice::complete()
      # internally, so we do not need to extract the imputed datasets here.
      # See: https://github.com/paul-buerkner/brms/blob/master/R/brm_multiple.R
      model <- do.call(
        brms::brm_multiple,
        c(list(data = imp_result$mids), common_brm_args, dots_brm)
      )
    }

  } else {
    # -- 12b. Single model path -------------------------------------------------
    model <- do.call(
      brms::brm,
      c(list(data = data), common_brm_args, dots_brm)
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
