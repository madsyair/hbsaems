% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hbm_lnln.R
\name{hbm_lnln}
\alias{hbm_lnln}
\title{Small Area Estimation using Hierarchical Bayesian under Lognormal Distribution}
\source{
Fabrizi, E., Ferrante, M. R., & Trivisano, C. (2018). Bayesian small area estimation for skewed business survey variables. Journal of the Royal Statistical Society. Series C (Applied Statistics), 67(4), 863–864. https://www.jstor.org/stable/26800576;
Gelman, A. (2006). Prior Distributions for Variance Parameters in Hierarchical Models (Comment on Article by Browne and Draper). Bayesian Analysis, 1(3), 527–528;
Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A Weakly Informative Default Prior Distribution for Logistic and Other Regression Models.
}
\usage{
hbm_lnln(
  response,
  predictors,
  group = NULL,
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
\item{response}{A non-negative (x>0) dependent (outcome) variable assumed to follow a lognormal distribution.}

\item{predictors}{A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.}

\item{group}{The name of the grouping variable (e.g., area, cluster, region)
used to define the hierarchical structure for random effects. This variable should
correspond to a column in the input data and is typically used to model area-level
variation through random intercepts.}

\item{sre}{An optional grouping factor mapping observations to spatial locations.
If not specified, each observation is treated as a separate location.
It is recommended to always specify a grouping factor to allow for handling of new data in postprocessing methods.}

\item{sre_type}{Determines the type of spatial random effect used in the model. The function currently supports "sar" and "car"}

\item{car_type}{Type of the CAR structure. Currently implemented are "escar" (exact sparse CAR), "esicar" (exact sparse intrinsic CAR),
"icar" (intrinsic CAR), and "bym2".}

\item{sar_type}{Type of the SAR structure. Either "lag" (for SAR of the response values) or
"error" (for SAR of the residuals).}

\item{M}{The M matrix in SAR is a spatial weighting matrix that shows the spatial relationship between locations with certain
weights, while in CAR, the M matrix is an adjacency matrix that only contains 0 and 1 to show the proximity between locations.
SAR is more focused on spatial influences with different intensities, while CAR is more on direct adjacency relationships.
If sre is specified, the row names of M have to match the levels of the grouping factor}

\item{data}{Dataset used for model fitting}

\item{handle_missing}{Mechanism to handle missing data (NA values) to ensure model stability and avoid estimation errors.
Three approaches are supported.
The \code{"deleted"} approach performs complete case analysis by removing all rows with any missing values before model fitting.
This is done using a simple filter such as \code{complete.cases(data)}.
It is recommended when the missingness mechanism is Missing Completely At Random (MCAR).
The \code{"multiple"} approach applies multiple imputation before model fitting.
Several imputed datasets are created (e.g., using the \code{mice} package or the \code{brm_multiple()} function in \code{brms}),
the model is fitted separately to each dataset, and the results are combined.
This method is suitable when data are Missing At Random (MAR).
The \code{"model"} approach uses model-based imputation within the Bayesian model itself.
Missing values are incorporated using the \code{mi()} function in the model formula (e.g., \code{y ~ mi(x1) + mi(x2)}),
allowing the missing values to be jointly estimated with the model parameters.
This method also assumes a MAR mechanism and is applicable only for continuous variables.
If data are suspected to be Missing Not At Random (MNAR), none of the above approaches directly apply.
Further exploration, such as explicitly modeling the missingness process or conducting sensitivity analyses, is recommended.}

\item{m}{Number of imputations to perform when using the \code{"multiple"} approach for handling missing data (default: 5).
This parameter is only used if \code{handle_missing = "multiple"}.
It determines how many imputed datasets will be generated.
Each imputed dataset is analyzed separately, and the posterior draws are then combined to account for both within-imputation and between-imputation variability,
following Rubin’s rules. A typical choice is between 5 and 10 imputations, but more may be needed for higher missingness rates.}

\item{prior}{Priors for the model parameters (default: \code{NULL}).
Should be specified using the \code{brms::prior()} function or a list of such objects.
For example, \code{prior = prior(normal(0, 1), class = "b")} sets a Normal(0,1) prior on the regression coefficients.
Multiple priors can be combined using \code{c()}, e.g.,
\code{prior = c(prior(normal(0, 1), class = "b"), prior(exponential(1), class = "sd"))}.
If \code{NULL}, default priors from \code{brms} will be used.}

\item{control}{A list of control parameters for the sampler (default: \code{list()})}

\item{chains}{Number of Markov chains (default: 4)}

\item{iter}{Total number of iterations per chain (default: 2000)}

\item{warmup}{Number of warm-up iterations per chain (default: floor(iter/2))}

\item{cores}{Number of CPU cores to use (default: 1)}

\item{sample_prior}{(default: "no")}

\item{...}{Additional arguments}
}
\value{
A \code{hbmfit} object
}
\description{
This function implements a \strong{Hierarchical Bayesian Small Area Estimation (HBSAE)} model
under a \strong{Lognormal distribution} using \strong{Bayesian inference} with the \code{brms} package.

The response variable \eqn{y_i} in area \eqn{i} is assumed to follow a lognormal distribution:

\deqn{y_i \mid \theta_i, \psi_i \sim \mathrm{Lognormal}(\theta_i, \psi_i)}

which implies:

\deqn{\log(y_i) \sim \mathcal{N}(\theta_i, \psi_i)}

The function utilizes the \strong{Bayesian regression modeling framework} provided by \code{brms},
which interfaces with 'Stan' for efficient Markov Chain Monte Carlo (MCMC) sampling.
The \code{brm()} function from \code{brms} is used to estimate posterior distributions based on user-defined
hierarchical and spatial structures.
}
\examples{
\donttest{
# Load necessary libraries
library(hbsaems)

# Load custom dataset
data <- data_lnln
head(data)

# --- 1. Prior Predictive Check ---
model.check_prior <- hbm_lnln(
response = "y_obs",
predictors = c("x1", "x2", "x3"),
group = "group",
data = data,
prior = c(
prior(normal(0.1, 0.1), class = "b"),
prior(normal(1, 1), class = "Intercept")
),
sample_prior = "only",
iter = 4000,
warmup = 2000,
chains = 2,
seed = 123
)
hbpc(model.check_prior, response_var = "y_obs")

# --- 2. Fit the Model with Data ---
model <- hbm_lnln(
response = "y_obs",
predictors = c("x1", "x2", "x3"),
group = "group",
data = data,
prior = c(
prior(normal(0.1, 0.1), class = "b"),
prior(normal(1, 1), class = "Intercept")
),
iter = 10000,
warmup = 5000,
chains = 1,
seed = 123
)
summary(model)

# --- 3. Fit Model with Spatial Effect (CAR) ---
M <- adjacency_matrix_car

model.spatial <- hbm_lnln(
response = "y_obs",
predictors = c("x1", "x2", "x3"),
group = "group",
sre = "sre", # Spatial grouping variable (must match rows/cols of M)
sre_type = "car", # Spatial random effect type
car_type = "icar", # CAR model type
M = M, # Adjacency matrix (must be symmetric)
data = data,
prior = c(
prior(normal(0.1, 0.1), class = "b"),
prior(normal(1, 1), class = "Intercept")
),
iter = 10000,
warmup = 5000,
chains = 1,
seed = 123
)
summary(model.spatial)
}
}
\author{
Arsyka Laila Oktalia Siregar
}
