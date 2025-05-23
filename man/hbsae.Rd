% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hbsae.R
\name{hbsae}
\alias{hbsae}
\title{hbsae : Hierarchical Bayesian Small Area Estimation}
\usage{
hbsae(model, scale = "response", newdata = NULL)
}
\arguments{
\item{model}{A \code{brmsfit} or \code{hbmfit} object, a fitted model from the \code{brms} package and \code{hbsaems} package.}

\item{scale}{A character string indicating the scale of the predictions. Must be one of \code{"response"} (default) or \code{"linear"}:
\code{"response"}: predictions are returned on the response scale. \code{"linear"}: predictions are returned on the scale of the linear predictor, \strong{without} applying the inverse link function or other transformations.
\code{"linear_inverse"}: predictions are returned on the scale of the linear predictor, \strong{with} applying the inverse link function or other transformations.}

\item{newdata}{A dataset for making predictions.}
}
\value{
An object of class \code{"hbsae"}, which is a list containing:
\item{rse_model}{A numeric value indicating the overall relative standard error (RSE) of the model.}
\item{result_table}{A \code{data.frame} containing predictions and associated statistics for each small area.}
}
\description{
This function performs Hierarchical Bayesian Small Area Estimation (HBSAE).
It estimates predictions and computes the Relative Standard Error (RSE) based on the posterior predictive sample from the fitted Bayesian model.
}
\details{
Hierarchical Bayesian Small Area Estimation
}
\examples{
\donttest{
# Prepare dataset
data("BostonHousing")
data <- BostonHousing

# Fit the Basic Model
model <- hbm(
formula = bf(medv ~ crim + indus + rm + dis + rad + tax),  # Formula model
hb_sampling = "gaussian",      # Gaussian family for continuous outcomes
hb_link = "identity",          # Identity link function (no transformation)
data = data,                   # Dataset
chains = 4,                    # Number of MCMC chains
iter = 4000,                   # Total MCMC iterations
warmup = 2000,                 # Number of warmup iterations
cores = 2                      # Paralelisasi
)
summary(model)

# Small Area Estimates
hbsae(model)
}
}
