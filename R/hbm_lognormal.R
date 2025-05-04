#' @title Small Area Estimation using Hierarchical Bayesian under Lognormal Distribution
#'
#' @description 
#' This function implements a **Hierarchical Bayesian Small Area Estimation (HBSAE)** model
#' under a **Lognormal distribution** using **Bayesian inference** with the `brms` package.
#'
#' The response variable \eqn{y} is assumed to follow a lognormal distribution, meaning that 
#' \eqn{\log(y)} follows a normal distribution:
#'
#' \deqn{\log(y) \sim N(\mu, \sigma^2)}
#'
#' where \eqn{\mu} represents the mean structure and \eqn{\sigma} the standard deviation in log-space.

#'
#' The function utilizes the **Bayesian regression modeling framework** provided by `brms`,
#' which interfaces with **Stan** for efficient Markov Chain Monte Carlo (MCMC) sampling.
#' The `brm()` function from `brms` is used to estimate posterior distributions based on user-defined
#' hierarchical and spatial structures.
#'
#' @name hbm_lognormal
#' @param response A non-negative (x>0) dependent (outcome) variable assumed to follow a lognormal distribution.
#'        Internally, the model applies a log transformation to fit a normal model.
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
#' library(ggplot2)
#' library(hbsaems)
#' 
#' # Prepare dataset
#' data("midwest")
#' data_midwest <- midwest[order(midwest$poptotal), ][2:31, ]
#' data_midwest$popbelowpoverty <- round(data_midwest$percbelowpoverty/100 * data_midwest$poptotal, 0)
#' 
#' # 1. Basic Lognormal Model 
#' model <- hbm_lognormal(
#'   response = "popbelowpoverty",
#'   predictors = c("popdensity", "perchsd"),
#'   data = data_midwest
#' )
#' summary(model1)
#' 
#' # 2. Model With Missing Data
#' data_midwest_missing <- data_midwest
#' data_midwest_missing$popbelowpoverty[sample(1:30, 3)] <- NA  # 3 missing values in response
#' 
#' # (a) Handling missing data by deleted (Only if missing in response)
#' model_deleted <- hbm_lognormal(
#'   response = "popbelowpoverty",
#'   predictors = c("popdensity", "perchsd"),
#'   data = data_midwest_missing,
#'   handle_missing = "deleted"
#' )
#' summary(model_deleted)
#' 
#' # (b) Handling missing data using model-based approach
#' model_model <- hbm_lognormal(
#'   response = "popbelowpoverty",
#'   predictors = c("popdensity", "perchsd"),
#'   data = data_midwest_missing,
#'   handle_missing = "model"
#' )
#' summary(model_model)
#' 
#' # (c) Handling missing data using multiple imputation (m=5)
#' model_multiple <- hbm_lognormal(
#'   response = "popbelowpoverty",
#'   predictors = c("popdensity", "perchsd"),
#'   data = data_midwest_missing,
#'   handle_missing = "multiple",
#'   m = 5
#' )
#' summary(model_multiple)
#' 
#' # 3. Model With Random Effect
#' model_re <- hbm_lognormal(
#'   response = "popbelowpoverty",
#'   predictors = c("popdensity", "perchsd"),
#'   re = "category",
#'   data = data_midwest
#' )
#' summary(model_re)
#' 
#' # 4. Model With Spatial Effect
#' # Dummy spatial weight matrix
#' M <- matrix(0, 4, 4)
#' for (i in 1:(4 - 1)) {
#'    M[i, i + 1] <- 1
#'    M[i + 1, i] <- 1
#' }
#' # Set row and column names to match the group levels
#' rownames(M) <- unique(data_midwest$state)
#' colnames(M) <- unique(data_midwest$state)
#' 
#' model_spatial <- hbm_lognormal(
#'   response = "popbelowpoverty",
#'   predictors = c("popdensity", "perchsd"),
#'   sre = "state",
#'   sre_type = "car",
#'   car_type = "icar",
#'   M = M,
#'   data = data_midwest
#' )
#' summary(model_spatial)
#' 
#' } 
#' 


hbm_lognormal <- function(response,
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
  
  # Check for non-positive or zero response values
  if (any(data[[response]] <= 0, na.rm = TRUE)) {
    stop("The response variable 'response' contains zero or negative values. Lognormal distribution requires strictly positive response values.")
  }

  # Build fixed effect formula
  fixed_effects <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste(response, "~", fixed_effects))

  # # Define default prior
  # if (is.null(prior)) {
  #   prior <- c(
  #     prior("", class = "b") # Flat prior untuk β (koefisien regresi)
  #   )
  # }

  # Check handle missing for continuous distribution
  if (is.null(handle_missing)) {
    if (anyNA(data[[response]]) || anyNA(data[predictors])) {
      handle_missing <- "model"
    } 
  }

  # Add handle missing
  if (!is.null(handle_missing) && handle_missing == "model") {
    vars <- unique(c(response, predictors))
    missing_vars <- vars[sapply(data[vars], function(x) any(is.na(x)))]

    # Create the main formula for the response variable
    response_with_mi <- ifelse(response %in% missing_vars,
      paste0(response, " | mi()"),
      response
    )

    # Create a formula for predictors with missing values
    predictor_with_mi <- sapply(predictors, function(x) {
      if (x %in% missing_vars) {
        # If a predictor has missing values, create an imputation formula for that predictor
        return(paste0("mi(", x, ")"))
      } else {
        # If a predictor has no missing values, use the regular predictor name
        return(x)
      }
    })

    # The main formula that combines the response and predictors
    main_formula <- brms::bf(as.formula(
      paste0(response_with_mi, " ~ ", paste(predictor_with_mi, collapse = " + "))
    ))

    # Create additional formulas for variables that have missing values
    auxiliary_formulas <- lapply(missing_vars, function(var) {
      if (var %in% response) {
        return(NULL)
      } # Do not create an additional model for the response

      # Only use variables from the initial formula as predictors
      predictors_for_var <- setdiff(predictors, var)
      predictors_for_var <- predictors_for_var[!sapply(data[predictors_for_var], function(x) any(is.na(x)))] # Avoid predictors that are also missing

      # If there are no valid predictors, use the intercept (1)
      if (length(predictors_for_var) > 0) {
        return(brms::bf(as.formula(paste0(var, " | mi() ~ ", paste(predictors_for_var, collapse = " + ")))))
      } else {
        return(brms::bf(as.formula(paste0(var, " | mi() ~ 1")))) # Use the intercept (1) if there are no valid predictors
      }
    })
    
    auxiliary_formulas <- Filter(Negate(is.null), auxiliary_formulas)
    if (length(auxiliary_formulas) > 0) {
      all_formulas <- Reduce("+", c(list(main_formula), auxiliary_formulas))
    } else {
      all_formulas <- main_formula
    }
  } else {
    all_formulas <- brms::bf(formula)
  }

  # Add random effect
  if (!is.null(re)) {
    formula_re <- as.formula(paste("~", paste("(1 | ", re, ")", collapse = " + ")))
  } else {
    formula_re <- NULL
  }

  # Model fitting
  model <- hbm(formula = all_formulas,
               hb_sampling = "lognormal",
               hb_link = "identity",
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
