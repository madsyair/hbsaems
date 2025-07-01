#' Hierarchical Bayesian Small Area Estimation
#' 
#' @title hbsae : Hierarchical Bayesian Small Area Estimation
#' 
#' @description This function performs Hierarchical Bayesian Small Area Estimation (HBSAE). 
#' It estimates predictions and computes the Relative Standard Error (RSE) based on the posterior predictive sample from the fitted Bayesian model.
#' 
#' @name hbsae
#' 
#' @param model A `brmsfit` or `hbmfit` object, a fitted model from the `brms` package and `hbsaems` package.
#' @param newdata A dataset for making predictions.
#' 
#' @return An object of class `hbsae_results`, which is a list containing:
#'   \item{rse_model}{A numeric value indicating the overall relative standard error (RSE) of the model.}
#'   \item{mse_model}{A numeric value indicating the overall mean squared error (MSE) of the model estimates, representing the average estimation error across areas.}
#'   \item{result_table}{A \code{data.frame} containing predictions and associated statistics for each small area.}
#'   
#' @importFrom brms posterior_predict
#' 
#' @export
#' 
#' @author Achmad Syahrul Choir and Saniyyah Sri Nurhayati
#'
#' @references 
#' Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
#'
#' @examples
#' \donttest{
#' 
#' library(hbsaems)
#' data("data_fhnorm")
#'
#' # Prepare the dataset
#' data <- data_fhnorm
#' 
#' # Fit the Basic Model
#' model <- hbm(
#'   formula = bf(y ~ x1 + x2 + x3),  # Formula model
#'   hb_sampling = "gaussian",       # Gaussian family for continuous outcomes
#'   hb_link = "identity",           # Identity link function (no transformation)
#'   data = data,                    # Dataset
#'   chains = 4,                     # Number of MCMC chains
#'   iter = 4000,                    # Total MCMC iterations
#'   warmup = 2000,                  # Number of warmup iterations
#'   cores = 2                       # Parallel processing
#' )
#' summary(model)
#' 
#' # Small Area Estimates
#' hbsae(model)
#' }
hbsae <- function(model,
                  newdata = NULL
                  ) {
  # Check if model is a brmsfit or hbmfit object
  
  handle_missing <- NULL
  data <- NULL
  
  if (inherits(model, "hbmfit")) {
    handle_missing <- model$handle_missing
    data <- model$data
    model <- model$model  
  }
  
  if (!inherits(model, "brmsfit")) {
    stop("Input model must be a brmsfit or hbmfit object.")
  }
  
  # Extract model formula 
  model_formula <- model$formula
  if (!is.null(model_formula$forms)) {
    main_formula <- as.formula(model_formula$forms[[1]])
  } else {
    main_formula <- as.formula(model_formula$formula)
  }
  
  # Extract response variable and trials variable
  model_formula_str <- as.character(main_formula)
  formula_parts <- strsplit(model_formula_str, " ~")[[2]]
  response_var <- sub(" \\|.*", "", formula_parts)
  response_var <- trimws(response_var)
  trials_var <- sub(".*trials\\(([^)]+)\\).*", "\\1", formula_parts)
  trials_var <- trimws(trials_var)
  
  # Generate predictions
  if (!is.null(newdata)) {   
    # Prediction using newdata
    newdata <- data.frame(newdata)
    n <- nrow(newdata)
    newdata$group <- as.factor(seq(n))


      preds <- brms::posterior_epred(model, newdata = newdata, transform = TRUE)

    
    y_true <- NULL
  } else {
    # Prediction using model data
    if (!is.null(handle_missing) && handle_missing == "deleted"){ #if handle_missing = deleted was selected

        preds <- brms::posterior_epred(model, newdata = data, allow_new_levels = TRUE, transform = TRUE)
      
        y_true <- data[[response_var]]
    } else{ 
        preds <- brms::posterior_epred(model, transform = TRUE)
      
        y_true <- model$data[[response_var]]  
    }
  }
  
  # Binomial model case
  is_binomial <- FALSE
  if (!is.null(model$family) && !is.null(model$family$family)) {
    is_binomial <- grepl("binomial", model$family$family, ignore.case = TRUE)
  }
  
  if (is_binomial) {
    # Extract the trials variable from newdata or model$data
    if (!is.null(newdata)) {
      trials <- newdata[[trials_var]]
    } else {
      if (!is.null(handle_missing) && handle_missing == "deleted"){
        trials <- data[[trials_var]]
      } else{
      trials <- model$data[[trials_var]]
      }
    }
    
    # Check that the number of trials matches the number of predicted observations
    if (length(trials) != ncol(preds)) {
      stop("Number of trials does not match the number of predicted columns.")
    }
    
    # Convert predicted number of successes to predicted probabilities
    preds <- sweep(preds, 2, trials, FUN = "/")
  }
  
  # Create Predictions
  pred <- apply(preds, 2, mean)
 
  # Relative Standard Error (RSE)
  rse <- apply(preds, 2, function(x) stats::sd(x) / abs(mean(x))) * 100
  mse <- apply(preds, 2, function(x) stats::sd(x)^2)
  rse_model <- mean(rse)
  mse_model <- mean(mse)
  
  if (is.null(rse_model) || is.na(rse_model)) {
    rse_model <- NA
  }
  if (is.null(mse_model) || is.na(mse_model)) {
    mse_model <- NA
  }
  if (!is.null(y_true) & !anyNA(y_true)) {
    # Create a summary table
    result_table <- data.frame(
      Prediction = pred,
      Actual = y_true,
      RSE_percent = rse,
      MSE = mse
    )
  } else {
    # Create a summary table
    result_table <- data.frame(
      Prediction = pred,
      RSE_percent = rse,
      MSE = mse
    )
  }
  
  # Return an object of class 'hbsae'
  return(structure(
    list(result_table = result_table, rse_model = rse_model, mse_model=mse_model, pred=pred),
    class = "hbsae_results"
  ))
}
