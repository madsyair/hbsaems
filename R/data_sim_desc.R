#' Simulated Fay-Herriot Normal Data (Area-Level)
#'
#' The `data_fhnorm` dataset contains simulated data for n areas based on a 
#' Fay-Herriot Normal model. It includes area-level covariates, true latent values, 
#' direct estimators, sampling variances, and area random effects. This dataset is 
#' intended for evaluating small area estimation models under normality assumptions.
#'
#' @format A data frame with n rows and 8 variables:
#' \describe{
#' \item{y}{Observed direct estimator per area.}
#' \item{D}{Sampling variance for the direct estimator.}
#' \item{x1, x2, x3}{Auxiliary area-level covariates.}
#' \item{theta_true}{True latent mean parameter per area.}
#' \item{u}{Area-level random effect.}
#' \item{group}{Area ID (1–n) specifying the grouping structure.}
#' \item{sre}{Optional grouping factor mapping observations to spatial locations (e.g. regions).}
#' }
#' @source Simulated data based on the Fay-Herriot Normal model.
#' @examples
#' data(data_fhnorm)
#' head(data_fhnorm)
"data_fhnorm"

#' Simulated Binomial–Logit-Normal data (area-level)
#'
#' The `data_binlogitnorm` dataset contains simulated data for 50 areas based on a 
#' Binomial–Logit-Normal model. It includes area-level covariates, true 
#' probability parameters, sample sizes, observed counts, direct estimators, 
#' sampling variances, and true latent values.
#'
#' This dataset is intended for evaluating small area estimation models 
#' under Binomial–Logit-Normal assumptions.
#'
#' @format A data frame with 50 rows and 13 variables:
#' \describe{
#' \item{n}{Sample size per area}
#' \item{y}{Observed success count per area}
#' \item{p}{Direct estimator of proportion}
#' \item{x1, x2, x3}{Auxiliary area-level covariates}
#' \item{u_true}{True area-level random effect}
#' \item{eta_true}{True linear predictor (logit scale)}
#' \item{p_true}{True probability per area}
#' \item{psi_i}{Sampling variance of logit-transformed direct estimator}
#' \item{y_obs}{Simulated noisy version of eta (logit scale)}
#' \item{p_obs}{Estimated proportion via inverse logit of y_obs}
#' \item{group}{Area ID (1–100) for random effects formula specifying the grouping structure in the data. }
#' \item{sre}{An optional grouping factor mapping observations to spatial locations.}
#' }
#' @source Simulated data based on a Binomial–Logit-Normal model
"data_binlogitnorm"

#' Simulation Data for Beta Logit Normal Model
#'
#' The `data_betalogitnorm` is a simulation data created specifically to demonstrate the implementation of 
#' _Hierarchical Bayesian Small Area Estimation_ (HB SAE) with Beta distribution. 
#' This data is suitable for testing Beta regression models with a hierarchical structure 
#' between areas. This data is also equipped with variables that apply spatial effects.
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#' \item{y}{Response variable - The proportion of simulation results, has a value between 0 and 1, follows a Beta distribution.}
#' \item{theta}{True latent mean parameter on the logit scale, representing the underlying probability of success in each area.}
#' \item{x1, x2, x3}{Predictors Variables}
#' \item{n}{The number of sample units for each region used in the survey}
#' \item{deff}{Design Effect}
#' \item{group}{Area ID (1–100) Random effects formula specifying the grouping structure in the data. }
#' \item{sre}{An optional grouping factor mapping observations to spatial locations.}
#' }
#' @source Simulated data based on a Beta-Logit-Normal model.
#' @examples
#' data(data_betalogitnorm)
#' head(data_betalogitnorm)
"data_betalogitnorm"


#' Simulation Data for Lognormal-Lognormal Model
#'
#' This dataset is a simulated data created for demonstrating the implementation of 
#' _Hierarchical Bayesian Small Area Estimation_ (HB SAE) using a **lognormal-lognormal model**. 
#' It includes area-level covariates, random effects, direct estimates, and spatial components,
#' for testing SAE models with lognormal assumptions and spatial correlation.
#'
#' @format A data frame with 100 rows and 13 variables:
#' \describe{
#' \item{group}{Area ID (1–100) for random effects formula specifying the grouping structure in the data. }
#' \item{x1, x2, x3}{Auxiliary area-level covariates}
#' \item{u_true}{True unstructured area-level random effect on the log scale.}
#' \item{teta_true}{True linear predictor on the log scale (meanlog for lognormal distribution).}
#' \item{mu_orig_true}{True mean on the original scale, calculated from \code{eta_true} and \code{sigma_e}.}
#' \item{n}{Sample size per area.}
#' \item{y_obs}{Simulated observed mean per area, generated from a lognormal distribution.}
#' \item{lambda_dir}{Direct estimator of the mean per area (same as \code{y_obs}).}
#' \item{y_log_obs}{Log-transformed direct estimator.}
#' \item{psi_i}{Approximate sampling variance of \code{y_obs}.}
#' \item{sre}{An optional grouping factor mapping observations to spatial locations.}
#' }
#' @source Simulated data based on a Lognormal–Lognormal model.
"data_lnln"


#' Adjacency Matrix for Conditional Autoregressive (CAR)
#'
#' The `adjacency_matrix_car` contains a symmetric adjacency matrix used for demonstrating 
#' _Hierarchical Bayesian Small Area Estimation_ (HB SAE) with spatial structure under the 
#' Conditional Autoregressive (CAR) model. 
#' The matrix consists of 0s and 1s to represent neighborhood relationships between areas, 
#' where a value of 1 indicates direct adjacency. This structure is commonly used in CAR models 
#' to specify spatial dependence based on local neighborhood connections.
#'
#' @format A 5×5 numeric matrix with row and column names `"1"` to `"5"`.
#' @source Simulated example created for illustrating spatial structure in CAR models.
"adjacency_matrix_car"


#' Spatial Weight for Simultaneous Autoregressive (SAR)
#'
#' The `spatial_weight_sar` dataset contains a standardized spatial weight matrix designed 
#' for use in _Hierarchical Bayesian Small Area Estimation_ (HB SAE) under the 
#' Simultaneous Autoregressive (SAR) model. 
#' The matrix represents spatial influence between areas with continuous, non-negative weights, 
#' and is symmetric with zeros on the diagonal. 
#' This structure allows modeling of spatial autocorrelation with varying intensity across regions.
#'
#' @format A 100×100 numeric matrix.
#' @source Simulated example created for illustrating spatial structure in SAR models.
"spatial_weight_sar"
