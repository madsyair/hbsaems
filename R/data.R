# R/data.R
# =============================================================================
# Documentation stubs for the simulated example datasets bundled with the
# package.  Each dataset is provided as a lazy-loaded .rda in /data.
#
# Naming convention (v1.0.0 onward): datasets use concrete administrative-
# level labels in English (loosely modeled on the BPS Indonesia hierarchy
# of provinsi > kabupaten/kota > kecamatan > kelurahan/desa) to illustrate
# realistic SAE workflows.  The labels are CONSISTENT across datasets:
# the same word ALWAYS refers to the same administrative level.
#
#   province  -- Level 1, provinsi          (always 5 in our examples)
#   regency   -- Level 2, kabupaten/kota    (analogue: US county)
#   district  -- Level 3, kecamatan         (analogue: US census tract)
#
# What DIFFERS between datasets is the analysis resolution -- i.e.\ at
# which level the 100 small areas live:
#
#   * Regency-level analysis (data_fhnorm, data_betalogitnorm):
#       100 regencies nested within 5 provinces.
#       Use spatial_var = "regency"  (fine, 100 areas) with spatial_weight_sar.
#       Use spatial_var = "province" (coarse, 5 areas) with adjacency_matrix_car.
#
#   * District-level analysis (data_binlogitnorm, data_lnln):
#       100 districts nested within 5 regencies.  ("province" omitted at
#       this resolution -- it would be a single value above all 5 regencies
#       and contribute no variation.)
#       Use spatial_var = "district" (fine, 100 areas) for IID random effects.
#       Use spatial_var = "regency"  (coarse, 5 areas) with adjacency_matrix_car_regency.
#
# Label format conventions (helps tell datasets apart at a glance):
#   * 3-digit suffix (regency_001..100, district_001..100) -- the 100-level
#     analysis areas.
#   * 2-digit suffix (province_01..05, regency_01..05)     -- the 5-level
#     coarse spatial clusters.
# =============================================================================


#' Simulated Fay-Herriot Normal Data
#'
#' A simulated dataset for 100 regencies under the Fay-Herriot Normal
#' small-area model.  Used as the running example throughout the package
#' documentation and vignettes.  The simulation is engineered so that the
#' canonical Fay-Herriot fit
#' (\code{hbm(..., sampling_variance = "D")}) converges with default
#' brms / Stan settings -- no divergent transitions, no manual
#' tuning required.
#'
#' \strong{Generative model.}  For each regency \eqn{i = 1, \ldots, 100},
#' \deqn{
#'   y_i = \theta_i + \varepsilon_i, \quad
#'   \varepsilon_i \sim \mathcal{N}(0, D_i)
#' }
#' \deqn{
#'   \theta_i = 10 + 0.8 \, x_{1i} - 0.5 \, x_{2i} + 0.3 \, x_{3i} + u_i,
#'   \quad u_i \sim \mathcal{N}(0, \sigma_u^2)
#' }
#' with auxiliary covariates \eqn{x_j \sim \mathcal{N}(0, 1)} (already
#' standardised), area RE SD \eqn{\sigma_u = 1}, and \strong{known}
#' sampling variances \eqn{D_i \sim \mathrm{Gamma}(\mathrm{shape} = 4,
#' \mathrm{rate} = 4)} -- a realistic spread (\eqn{\approx [0.2, 3.0]})
#' that mirrors varying sample sizes across regencies.
#'
#' \strong{Important: pass \code{D} as the sampling variance.}  In any
#' fit on this dataset, supply \code{sampling_variance = "D"}; otherwise
#' the residual \eqn{\sigma} and the area-RE \eqn{\sigma_u} compete to
#' explain the same variance, producing weak identifiability and
#' divergent transitions.
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{\code{y}}{Direct (survey) estimator of the area mean.}
#'   \item{\code{D}}{Sampling variance of the direct estimator (KNOWN
#'     from the survey design; treat as input, not as a parameter).}
#'   \item{\code{x1}, \code{x2}, \code{x3}}{Auxiliary covariates at the
#'     area level, simulated from \eqn{\mathcal{N}(0, 1)}.}
#'   \item{\code{theta_true}}{True area-level latent value
#'     \eqn{\theta_i}.}
#'   \item{\code{u}}{True area-level random effect \eqn{u_i}.}
#'   \item{\code{regency}}{Regency identifier (\code{"regency_001"}
#'     through \code{"regency_100"}) used as the IID random-effect
#'     grouping variable.  Use with \code{re = ~ (1 | regency)} or
#'     \code{area_var = "regency"}.}
#'   \item{\code{province}}{Province identifier (\code{"province_01"}
#'     through \code{"province_05"}) -- 20 regencies per province.
#'     Used as the spatial random-effect grouping variable for CAR /
#'     SAR / BYM2 examples; also serves as the higher level in the
#'     hierarchical-area example
#'     \code{area_var = c("province", "regency")}.}
#' }
#'
#' @source Simulated.  Reproducible script in \code{data-raw/data_fhnorm.R}.
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
#'     \code{"district_100"}) -- the 100 small areas to estimate.}
#'   \item{\code{regency}}{Regency identifier (\code{"regency_01"} ..
#'     \code{"regency_05"}) -- coarse spatial-cluster level (5 regencies
#'     each containing 20 districts).  Pair with
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
#'     \code{"district_100"}) -- the 100 small areas to estimate.}
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
#'     \code{"regency_05"}) -- coarse spatial-cluster level (5 regencies
#'     each containing 20 districts).  Pair with
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


#' Regency-level Adjacency Matrix (Coarse Spatial Cluster)
#'
#' A small example adjacency matrix used for fitting Conditional
#' Autoregressive (CAR) random effects on the **regency** (coarse
#' spatial-cluster, kabupaten) level.  Pairs with \code{data_binlogitnorm}
#' and \code{data_lnln}, whose \code{regency} column has matching labels.
#'
#' @format A binary symmetric \eqn{5 \times 5} matrix with row- and
#'   column-names \code{regency_01} .. \code{regency_05}; entries are
#'   \code{1} for adjacent regency pairs and \code{0} otherwise.
#'
#' @note The naming \code{regency_01..05} (two-digit suffix) is reserved
#'   in this package for the COARSE 5-level cluster used by
#'   \code{data_binlogitnorm} and \code{data_lnln}.  The 100-level FINE
#'   regency column in \code{data_fhnorm} and \code{data_betalogitnorm}
#'   uses three-digit suffixes (\code{regency_001..100}) and pairs with
#'   the larger \code{spatial_weight_sar} matrix.  See
#'   \code{vignette("hbsaems-spatial")} for the naming convention.
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
