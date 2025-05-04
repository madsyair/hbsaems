#' @title Small Area Estimation using Hierarchical Bayesian under Logit-Normal Distribution
#'
#' @description
#' This function implements a **Hierarchical Bayesian Small Area Estimation (HBSAE)** model
#' under a **logit-normal distribution** using **Bayesian inference** with the `brms` package.
#'
#' The model accounts for **fixed effects**, **random effects**, **spatial random effects (CAR/SAR models)**,
#' and **measurement error correction**, allowing for robust small area estimation.
#'
#' The function utilizes the **Bayesian regression modeling framework** provided by `brms`,
#' which interfaces with **Stan** for efficient Markov Chain Monte Carlo (MCMC) sampling.
#' The `brm()` function from `brms` is used to estimate posterior distributions based on user-defined
#' hierarchical and spatial structures.
#'
#' @name hbm_logitnormal
#'
#' @param response The dependent (outcome) variable in the model. This variable represents the count of successes in a Binomial distribution.
#' @param trials Specifies the number of trials in a binomial model. This is required for binomial family models where the response variable is specified as a proportion.
#' @param predictors A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.
#' @param re Specifies the grouping variable for hierarchical (random effects) models. Used when modeling data with grouped structures (e.g., individuals within clusters).
#' @param sre An optional grouping factor mapping observations to spatial locations. If not specified, each observation is treated as a separate location. It is recommended to always specify a grouping factor to allow for handling of new data in postprocessing methods.
#' @param sre_type Determines the type of spatial random effect used in the model. The function currently supports "sar" and "car"
#' @param car_type Type of the CAR structure. Currently implemented are "escar" (exact sparse CAR), "esicar" (exact sparse intrinsic CAR), "icar" (intrinsic CAR), and "bym2". 
#' @param sar_type Type of the SAR structure. Either "lag" (for SAR of the response values) or "error" (for SAR of the residuals). 
#' @param M The M matrix in SAR is a spatial weighting matrix that shows the spatial relationship between locations with certain weights, while in CAR, the M matrix is an adjacency matrix that only contains 0 and 1 to show the proximity between locations. SAR is more focused on spatial influences with different intensities, while CAR is more on direct adjacency relationships. If sre is specified, the row names of M have to match the levels of the grouping factor
#' @param data Dataset used for model fitting
#' @param handle_missing Mechanism to handle missing data (NA values) to ensure model stability and prevent errors during estimation.
#' @param m Number of multiple imputations
#' @param prior Priors for the model parameters (default: `NULL`)
#' @param control A list of control parameters for the sampler (default: `list()`)
#' @param chains Number of Markov chains (default: 4)
#' @param iter Total number of iterations per chain (default: 2000)
#' @param warmup Number of warm-up iterations per chain (default: floor(iter/2))
#' @param cores Number of CPU cores to use (default: 1)
#' @param sample_prior (default: "no")
#' @param ...
#'
#' @return A `hbmfit` object
#' @export
#'
#' @examples
#' \donttest{
#' 
#' # Prepare dataset
#' data("esoph")
#' esoph$n <- esoph$ncases + esoph$ncontrols
#'
#'
#' # Fit Logit-Normal Model
#' model1 <- hbm_logitnormal(
#'   response = "ncases",
#'   trials = "n",
#'   predictors = c("agegp", "tobgp"),
#'   data = esoph
#' )
#' summary(model1)
#' 
#' 
#' # Fit Logit-Normal Model With Missing Data
#' esoph[5:7, "ncases"] <- NA
#' 
#' model2 <- hbm_logitnormal(
#'   response = "ncases",
#'   trials = "n",
#'   predictors = c("agegp", "tobgp"),
#'   data = esoph,
#'   handle_missing = "deleted"
#' )
#' summary(model2)
#' 
#' model3 <- hbm_logitnormal(
#'   response = "ncases",
#'   trials = "n",
#'   predictors = c("agegp", "tobgp"),
#'   data = esoph,
#'   handle_missing = "multiple"
#' )
#' summary(model3)
#' 
#' 
#' # Fit Logit-Normal Model With Spatial Effect
#' # Create a Adjacency Matrix
#' esoph$area <- as.numeric(interaction(esoph$agegp, esoph$tobgp, esoph$alcgp, drop = TRUE))
#' n_areas <- length(unique(esoph$area))
#' adjacency_matrix <- bandSparse(n_areas, k = c(-1, 1), diag = list(rep(1, n_areas - 1), rep(1, n_areas - 1)))
#' area_names <- as.character(sort(unique(esoph$area)))
#' rownames(adjacency_matrix) <- area_names
#' colnames(adjacency_matrix) <- area_names
#' 
#' model4 <- hbm_logitnormal(
#'   response = "ncases",              # Response variable (number of successes)
#'   trials = "n",                     # Total trials (ncases + ncontrols)
#'   predictors = c("agegp", "tobgp", "alcgp"),  # Predictor variables
#'   sre = "area",                     # Spatial random effect based on the 'area' variable
#'   sre_type = "car",                 # Use Conditional Autoregressive (CAR) model for spatial effects
#'   M = adjacency_matrix,             # Adjacency matrix defining spatial relationships
#'   data = esoph                     # Input dataset
#' )
#' summary(model4)
#' 
#' } 
#' 
hbm_logitnormal <- function(response,
                            trials,
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
                            warmup = floor(iter / 2),
                            cores = 1,
                            sample_prior = "no",
                            ...){
  
  # Ensure response and predictors exist in the data
  if (!(response %in% names(data))) {
    stop("Response variable not found in 'data'.")
  }
  
  if (!all(predictors %in% names(data))) {
    stop("One or more predictor variables not found in 'data'.")
  }
  
  if (!all(trials %in% names(data))) {
    stop("Trials not found in 'data'.")
  }
  
  if(!is.null(re)){
    if (!(re %in% names(data))) {
      stop(sprintf("Variable '%s' not found in the data.", re))
    }
  }
  
  if(!is.null(sre)){
    if (!(sre %in% names(data))) {
      stop(sprintf("Variable '%s' not found in the data.", sre))
    }
  }
  
  if (any(is.na(data[[trials]]))) {
    stop("Trials contains NA values. The model cannot proceed.")
  }
  
  if (any(data[[trials]] <= 0 | data[[trials]] != floor(data[[trials]]), na.rm = TRUE)) {
    stop("Number of trials must be a positive integer.")
  }
  
  if (any(data[[response]] < 0 | data[[response]] != floor(data[[response]]), na.rm = TRUE)) {
    stop("Response must be a non-negative integer.")
  }
  
  if (any(data[[response]] > data[[trials]], na.rm = TRUE)) {
    stop("Response cannot be greater than the number of trials.")
  }
  
  # Add fixed effect formula
  fixed_effects <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste(response, "| trials(", trials, ") ~", fixed_effects))

  # Define default prior
  if (is.null(prior)) {
    prior <- c(
      prior("", class = "b")  # Flat prior for β
    )
  }
  
  # Check handle missing for continuous distribution
  if (is.null(handle_missing)) {
    if (anyNA(data[[response]]) || anyNA(data[predictors])) {
      handle_missing <- "multiple"
    } 
  }

  # Add handle missing
  if (!is.null(handle_missing) && handle_missing == "model") {
    stop("Error: This distribution does not support `handle_missing = 'model'`. ",
         "Please use `'multiple'` instead.")
  }
  
  all_formulas <- brms::bf(formula)
  
  # Add random effect
  if (!is.null(re)) {
    formula_re <- as.formula(paste("~", paste("(1 |", re, ")", collapse = " + ")))
  } else {
    formula_re <- NULL
  }

  # Model fitting
  model <- hbm(formula = all_formulas,
               hb_sampling = "binomial",
               hb_link = "logit",
               re = formula_re,
               sre = sre,
               sre_type = sre_type,
               car_type = car_type,
               sar_type = sar_type,
               M = M,
               handle_missing = handle_missing,
               m = m,
               data = data,
               prior = prior,
               control = list(),
               chains = chains,
               iter = iter,
               warmup = warmup,
               cores = cores,
               sample_prior = sample_prior,
               ...)
  return(model)
}
