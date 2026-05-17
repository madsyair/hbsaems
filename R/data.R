# R/data.R
# =============================================================================
# Documentation stubs for the simulated example datasets bundled with the
# package.  Each dataset is provided as a lazy-loaded .rda in /data.
#
# Naming convention (v1.0.0 onward): instead of generic "group" and "sre"
# columns, datasets use concrete Indonesian administrative levels to
# illustrate realistic SAE workflows:
#
#   * Regency-level data  (data_fhnorm, data_betalogitnorm):
#       100 regencies (kabupaten/kota) nested within 5 provinces.
#   * District-level data (data_binlogitnorm, data_lnln):
#       100 districts (kecamatan) nested within 5 regencies.
# =============================================================================


#' Simulated Fay-Herriot Normal Data
#'
#' A simulated dataset for 100 regencies under the Fay-Herriot Normal
#' small-area model.  Used as the running example throughout the package
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
#'   \item{\code{regency}}{Regency identifier (\code{"regency_001"} ..
#'     \code{"regency_100"}) used as the IID random-effect grouping
#'     variable.  Use with \code{re = ~ (1 | regency)} or as
#'     \code{area_var = "regency"} in the wrapper functions.}
#'   \item{\code{province}}{Province identifier (\code{"province_01"} ..
#'     \code{"province_05"}) used as the spatial random-effect grouping
#'     variable for CAR or SAR models.  Use with
#'     \code{spatial_var = "province"} and \code{M = adjacency_matrix_car}.}
#' }
#'
#' @source Simulated.
"data_fhnorm"


#' Simulated Beta Logit-Normal Data
#'
#' A simulated dataset for 100 regencies under a Beta logit-normal model.
#' The response \code{y} is a proportion in \eqn{(0, 1)}.
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{\code{y}}{Direct estimator of the area-level proportion.}
#'   \item{\code{theta}}{True area-level proportion.}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates.}
#'   \item{\code{n}}{Area sample size.}
#'   \item{\code{deff}}{Design effect.}
#'   \item{\code{regency}}{Regency identifier (\code{"regency_001"} ..
#'     \code{"regency_100"}).}
#'   \item{\code{province}}{Province identifier (\code{"province_01"} ..
#'     \code{"province_05"}) -- spatial cluster level for CAR / SAR.}
#' }
#'
#' @source Simulated.
"data_betalogitnorm"


#' Simulated Binomial Logit-Normal Data
#'
#' A simulated dataset for 100 districts under a Binomial logit-normal
#' model.  Each district provides a number of successes (\code{y}) out of
#' \code{n} trials.
#'
#' @format A data frame with 100 rows and 14 variables:
#' \describe{
#'   \item{\code{n}}{Number of trials in the district.}
#'   \item{\code{y}}{Number of successes.}
#'   \item{\code{p}}{Direct proportion (\code{y / n}).}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates.}
#'   \item{\code{u_true}}{True area-level random effect (logit scale).}
#'   \item{\code{eta_true}}{True linear predictor (logit scale).}
#'   \item{\code{p_true}}{True success probability.}
#'   \item{\code{psi_i}}{Sampling variance.}
#'   \item{\code{y_obs}, \code{p_obs}}{Observed (direct) values.}
#'   \item{\code{district}}{District identifier (\code{"district_001"} ..
#'     \code{"district_100"}).}
#'   \item{\code{regency}}{Regency identifier (\code{"regency_01"} ..
#'     \code{"regency_05"}) -- spatial cluster level.  Pair with
#'     \code{adjacency_matrix_car_regency}.}
#' }
#'
#' @source Simulated.
"data_binlogitnorm"


#' Simulated Lognormal-Lognormal Data
#'
#' A simulated dataset for 100 districts under a Lognormal-Lognormal
#' model.  Suitable for strictly positive, right-skewed outcomes.
#'
#' @format A data frame with 100 rows and 13 variables:
#' \describe{
#'   \item{\code{district}}{District identifier (\code{"district_001"} ..
#'     \code{"district_100"}).}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates.}
#'   \item{\code{u_true}}{True area-level random effect (log scale).}
#'   \item{\code{teta_true}}{True linear predictor (log scale).}
#'   \item{\code{mu_orig_true}}{True mean on the original scale.}
#'   \item{\code{n}}{Area sample size.}
#'   \item{\code{y_obs}}{Observed direct estimator.}
#'   \item{\code{lambda_dir}}{Direct estimator scale parameter.}
#'   \item{\code{y_log_obs}}{Observed value on the log scale.}
#'   \item{\code{psi_i}}{Sampling variance on the log scale.}
#'   \item{\code{regency}}{Regency identifier (\code{"regency_01"} ..
#'     \code{"regency_05"}) -- spatial cluster level.  Pair with
#'     \code{adjacency_matrix_car_regency}.}
#' }
#'
#' @source Simulated.
"data_lnln"


#' Province-level Adjacency Matrix
#'
#' A small example adjacency matrix used for fitting Conditional
#' Autoregressive (CAR) random effects on the **province** level.
#' Pairs with \code{data_fhnorm} and \code{data_betalogitnorm}, whose
#' \code{province} column has matching labels.
#'
#' @format A binary symmetric \eqn{5 \times 5} matrix with row- and
#'   column-names \code{province_01} .. \code{province_05}; entries are
#'   \code{1} for adjacent province pairs and \code{0} otherwise.
#'
#' @source Simulated.
"adjacency_matrix_car"


#' Regency-level Adjacency Matrix
#'
#' A small example adjacency matrix used for fitting Conditional
#' Autoregressive (CAR) random effects on the **regency** level.
#' Pairs with \code{data_binlogitnorm} and \code{data_lnln}, whose
#' \code{regency} column has matching labels.
#'
#' @format A binary symmetric \eqn{5 \times 5} matrix with row- and
#'   column-names \code{regency_01} .. \code{regency_05}; entries are
#'   \code{1} for adjacent regency pairs and \code{0} otherwise.
#'
#' @source Simulated.
"adjacency_matrix_car_regency"


#' Spatial Weight Matrix for Simultaneous Autoregressive Models
#'
#' A row-standardised spatial weight matrix for 100 regencies, used for
#' fitting Simultaneous Autoregressive (SAR) random effects.  Pairs with
#' \code{data_fhnorm}'s \code{regency} column.
#'
#' @format A \eqn{100 \times 100} numeric matrix with row- and
#'   column-names \code{regency_001} .. \code{regency_100} and row sums
#'   equal to one (when at least one neighbour is present).
#'
#' @source Simulated.
"spatial_weight_sar"
