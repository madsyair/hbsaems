% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hbm_lognormal.R
\name{hbm_lognormal}
\alias{hbm_lognormal}
\title{Small Area Estimation using Hierarchical Bayesian under Lognormal Distribution}
\usage{
hbm_lognormal(
  response,
  predictors,
  re = NULL,
  sre = NULL,
  sre_type = NULL,
  car_type = NULL,
  sar_type = NULL,
  M = NULL,
  data,
  handle_missing = NULL,
  m = 5,
  prior = NULL,
  control = list(),
  chains = 4,
  iter = 4000,
  warmup = floor(iter/2),
  cores = 1,
  sample_prior = "no",
  ...
)
}
\arguments{
\item{response}{A non-negative (x>0) dependent (outcome) variable assumed to follow a lognormal distribution.
Internally, the model applies a log transformation to fit a normal model.}

\item{predictors}{A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.}

\item{re}{Specifies the grouping variable for hierarchical (random effects) models. Used when modeling data with grouped structures (e.g., individuals within clusters).}

\item{sre}{An optional grouping factor mapping observations to spatial locations. If not specified, each observation is treated as a separate location. It is recommended to always specify a grouping factor to allow for handling of new data in postprocessing methods.}

\item{sre_type}{Determines the type of spatial random effect used in the model. The function currently supports "sar" and "car"}

\item{car_type}{Type of the CAR structure. Currently implemented are "escar" (exact sparse CAR), "esicar" (exact sparse intrinsic CAR), "icar" (intrinsic CAR), and "bym2".}

\item{sar_type}{Type of the SAR structure. Either "lag" (for SAR of the response values) or "error" (for SAR of the residuals).}

\item{M}{The M matrix in SAR is a spatial weighting matrix that shows the spatial relationship between locations with certain weights, while in CAR, the M matrix is an adjacency matrix that only contains 0 and 1 to show the proximity between locations. SAR is more focused on spatial influences with different intensities, while CAR is more on direct adjacency relationships. If sre is specified, the row names of M have to match the levels of the grouping factor}

\item{data}{Dataset used for model fitting}

\item{handle_missing}{Mechanism to handle missing data (NA values) to ensure model stability and prevent errors during estimation.}

\item{m}{Number of multiple imputations}

\item{prior}{Priors for the model parameters (default: \code{NULL})}

\item{control}{A list of control parameters for the sampler (default: \code{list()})}

\item{chains}{Number of Markov chains (default: 4)}

\item{iter}{Total number of iterations per chain (default: 2000)}

\item{warmup}{Number of warm-up iterations per chain (default: floor(iter/2))}

\item{cores}{Number of CPU cores to use (default: 1)}

\item{sample_prior}{(default: "no")}

\item{...}{}
}
\value{
A \code{hbmfit} object
}
\description{
This function implements a \strong{Hierarchical Bayesian Small Area Estimation (HBSAE)} model
under a \strong{Lognormal distribution} using \strong{Bayesian inference} with the \code{brms} package.

The response variable \eqn{y} is assumed to follow a lognormal distribution, meaning that
\eqn{\log(y)} follows a normal distribution:

\deqn{\log(y) \sim N(\mu, \sigma^2)}

where \eqn{\mu} represents the mean structure and \eqn{\sigma} the standard deviation in log-space.

The function utilizes the \strong{Bayesian regression modeling framework} provided by \code{brms},
which interfaces with \strong{Stan} for efficient Markov Chain Monte Carlo (MCMC) sampling.
The \code{brm()} function from \code{brms} is used to estimate posterior distributions based on user-defined
hierarchical and spatial structures.
}
\examples{
\donttest{
library(ggplot2)
library(hbsaems)

# Prepare dataset
data("midwest")
data_midwest <- midwest[order(midwest$poptotal), ][2:31, ]
data_midwest$popbelowpoverty <- round(data_midwest$percbelowpoverty/100 * data_midwest$poptotal, 0)

# 1. Basic Lognormal Model 
model <- hbm_lognormal(
  response = "popbelowpoverty",
  predictors = c("popdensity", "perchsd"),
  data = data_midwest
)
summary(model1)

# 2. Model With Missing Data
data_midwest_missing <- data_midwest
data_midwest_missing$popbelowpoverty[sample(1:30, 3)] <- NA  # 3 missing values in response

# (a) Handling missing data by deleted (Only if missing in response)
model_deleted <- hbm_lognormal(
  response = "popbelowpoverty",
  predictors = c("popdensity", "perchsd"),
  data = data_midwest_missing,
  handle_missing = "deleted"
)
summary(model_deleted)

# (b) Handling missing data using model-based approach
model_model <- hbm_lognormal(
  response = "popbelowpoverty",
  predictors = c("popdensity", "perchsd"),
  data = data_midwest_missing,
  handle_missing = "model"
)
summary(model_model)

# (c) Handling missing data using multiple imputation (m=5)
model_multiple <- hbm_lognormal(
  response = "popbelowpoverty",
  predictors = c("popdensity", "perchsd"),
  data = data_midwest_missing,
  handle_missing = "multiple",
  m = 5
)
summary(model_multiple)

# 3. Model With Random Effect
model_re <- hbm_lognormal(
  response = "popbelowpoverty",
  predictors = c("popdensity", "perchsd"),
  re = "category",
  data = data_midwest
)
summary(model_re)

# 4. Model With Spatial Effect
# Dummy spatial weight matrix
M <- matrix(0, 4, 4)
for (i in 1:(4 - 1)) {
   M[i, i + 1] <- 1
   M[i + 1, i] <- 1
}
# Set row and column names to match the group levels
rownames(M) <- unique(data_midwest$state)
colnames(M) <- unique(data_midwest$state)

model_spatial <- hbm_lognormal(
  response = "popbelowpoverty",
  predictors = c("popdensity", "perchsd"),
  sre = "state",
  sre_type = "car",
  car_type = "icar",
  M = M,
  data = data_midwest
)
summary(model_spatial)

} 

}
