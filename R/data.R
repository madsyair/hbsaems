# R/data.R
# =============================================================================
# Documentation stubs for the simulated example datasets bundled with the
# package.  Each dataset is provided as a lazy-loaded .rda in /data.
# =============================================================================


#' Simulated Fay-Herriot Normal Data
#'
#' A simulated dataset for 100 areas under the Fay-Herriot Normal small-area
#' model.  Used as the running example throughout the package
#' documentation and vignettes.
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{\code{y}}{Direct (survey) estimator of the area mean.}
#'   \item{\code{D}}{Sampling variance of the direct estimator.}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates at the area
#'     level.}
#'   \item{\code{theta_true}}{True area-level latent value.}
#'   \item{\code{u}}{True area-level random effect.}
#'   \item{\code{group}}{Area identifier (1-100) for random-effect grouping.}
#'   \item{\code{sre}}{Spatial-random-effect grouping variable.}
#' }
#'
#' @source Simulated.
"data_fhnorm"


#' Simulated Beta Logit-Normal Data
#'
#' A simulated dataset for 100 areas under a Beta logit-normal model.  The
#' response \code{y} is a proportion in \eqn{(0, 1)}.
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{\code{y}}{Direct estimator of the area-level proportion.}
#'   \item{\code{theta}}{True area-level proportion.}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates.}
#'   \item{\code{n}}{Area sample size.}
#'   \item{\code{deff}}{Design effect.}
#'   \item{\code{group}}{Area identifier (1-100).}
#'   \item{\code{sre}}{Spatial-random-effect grouping variable.}
#' }
#'
#' @source Simulated.
"data_betalogitnorm"


#' Simulated Binomial Logit-Normal Data
#'
#' A simulated dataset for 100 areas under a Binomial logit-normal model.
#' Each area provides a number of successes (\code{y}) out of \code{n}
#' trials.
#'
#' @format A data frame with 100 rows and 14 variables:
#' \describe{
#'   \item{\code{n}}{Number of trials in the area.}
#'   \item{\code{y}}{Number of successes.}
#'   \item{\code{p}}{Direct proportion (\code{y / n}).}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates.}
#'   \item{\code{u_true}}{True area-level random effect (logit scale).}
#'   \item{\code{eta_true}}{True linear predictor (logit scale).}
#'   \item{\code{p_true}}{True success probability.}
#'   \item{\code{psi_i}}{Sampling variance.}
#'   \item{\code{y_obs}, \code{p_obs}}{Observed (direct) values.}
#'   \item{\code{group}}{Area identifier (1-100).}
#'   \item{\code{sre}}{Spatial-random-effect grouping variable.}
#' }
#'
#' @source Simulated.
"data_binlogitnorm"


#' Simulated Lognormal-Lognormal Data
#'
#' A simulated dataset for 100 areas under a Lognormal-Lognormal model.
#' Suitable for strictly positive, right-skewed outcomes.
#'
#' @format A data frame with 100 rows and 13 variables:
#' \describe{
#'   \item{\code{group}}{Area identifier (1-100).}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates.}
#'   \item{\code{u_true}}{True area-level random effect (log scale).}
#'   \item{\code{teta_true}}{True linear predictor (log scale).}
#'   \item{\code{mu_orig_true}}{True mean on the original scale.}
#'   \item{\code{n}}{Area sample size.}
#'   \item{\code{y_obs}}{Observed direct estimator.}
#'   \item{\code{lambda_dir}}{Direct estimator scale parameter.}
#'   \item{\code{y_log_obs}}{Observed value on the log scale.}
#'   \item{\code{psi_i}}{Sampling variance on the log scale.}
#'   \item{\code{sre}}{Spatial-random-effect grouping variable.}
#' }
#'
#' @source Simulated.
"data_lnln"


#' Adjacency Matrix for Conditional Autoregressive Models
#'
#' A small example adjacency matrix used for fitting Conditional
#' Autoregressive (CAR) random effects.
#'
#' @format A binary symmetric \eqn{5 \times 5} matrix with \code{1} for
#'   adjacent pairs of areas and \code{0} otherwise.
#'
#' @source Simulated.
"adjacency_matrix_car"


#' Spatial Weight Matrix for Simultaneous Autoregressive Models
#'
#' A row-standardised spatial weight matrix for 100 areas, used for fitting
#' Simultaneous Autoregressive (SAR) random effects.
#'
#' @format A \eqn{100 \times 100} numeric matrix with row sums equal to one
#'   (when at least one neighbour is present).
#'
#' @source Simulated.
"spatial_weight_sar"
