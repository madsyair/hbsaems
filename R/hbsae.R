#' Hierarchical Bayesian Small Area Estimation
#' @title hbsae : Hierarchical Bayesian Small Area Estimation
#' @description This function performs Hierarchical Bayesian Small Area Estimation (HBSAE). 
#' It estimates predictions and computes the Relative Standard Error (RSE) based on the posterior predictive sample from the fitted Bayesian model.
#' @name hbsae
#' 
#' @param model A `brmsfit` or `hbmfit` object, a fitted model from the `brms` package and `hbsaems` package.
#' @param scale A character string indicating the scale of the predictions. Must be one of \code{"response"} (default) or \code{"linear"}:
#' \code{"response"}: predictions are returned on the response scale. \code{"linear"}: predictions are returned on the scale of the linear predictor, **without** applying the inverse link function or other transformations.
#' \code{"linear_inverse"}: predictions are returned on the scale of the linear predictor, **with** applying the inverse link function or other transformations.
#' @param newdata A dataset for making predictions.
#' 
#' @return An object of class \code{"hbsae"}, which is a list containing:
#'   \item{rse_model}{A numeric value indicating the overall relative standard error (RSE) of the model.}
#'   \item{result_table}{A \code{data.frame} containing predictions and associated statistics for each small area.}
#'   
#' @importFrom stats sd
#' @importFrom brms posterior_predict
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # Prepare dataset
#' data("BostonHousing")
#' data <- BostonHousing
#' 
#' # Fit the Basic Model
#' model <- hbm(
#' formula = bf(medv ~ crim + indus + rm + dis + rad + tax),  # Formula model
#' hb_sampling = "gaussian",      # Gaussian family for continuous outcomes
#' hb_link = "identity",          # Identity link function (no transformation)
#' data = data,                   # Dataset
#' chains = 4,                    # Number of MCMC chains
#' iter = 4000,                   # Total MCMC iterations
#' warmup = 2000,                 # Number of warmup iterations
#' cores = 2                      # Paralelisasi
#' )
#' summary(model)
#' 
#' # Small Area Estimates
#' hbsae(model)
#' }

hbsae <- function(model,
                  scale = "response",
                  newdata = NULL
                  ) {
  
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
  
  if (!scale %in% c("response", "linear", "linear_inverse")) {
    stop('Prediction` must be either "response", "linear", or "linear_inverse.')
  }
  
  # Extract model formula 
  model_formula <- model$formula
  if (!is.null(model_formula$forms)) {
    main_formula <- as.formula(model_formula$forms[[1]])
  } else {
    main_formula <- as.formula(model_formula$formula)
  }
  
  # Extract response variable
  model_formula_str <- as.character(main_formula)
  formula_parts <- strsplit(model_formula_str, " ~")[[2]]
  response_var <- (formula_parts[1])
  response_var <- sub(" \\|.*", "", response_var)
  response_var <- trimws(response_var)
  
  # Generate predictions
  if (!is.null(newdata)) {   
    # Prediction using newdata
    newdata <- data.frame(newdata)
    n <- nrow(newdata)
    newdata$group <- as.factor(seq(n))

    if (scale == "response") {
      preds <- brms::posterior_predict(model, newdata = newdata, allow_new_levels = TRUE)
    } else if (scale == "linear") {
      preds <- brms::posterior_linpred(model, newdata = newdata, transform = FALSE)
    } else if (scale == "linear_inverse") {
      preds <- brms::posterior_linpred(model, newdata = newdata, transform = TRUE)
    }
    
    y_true <- NULL
  } else {
    # Prediction using model data
    if (!is.null(handle_missing) && handle_missing == "deleted"){ #if handle_missing = deleted was selected
      if (scale == "response") {
        preds <- brms::posterior_predict(model, newdata = data, allow_new_levels = TRUE)
      } else if (scale == "linear") {
        preds <- brms::posterior_linpred(model, newdata = data, allow_new_levels = TRUE, transform = FALSE)
      } else if (scale == "linear_inverse") {
        preds <- brms::posterior_linpred(model, newdata = data, allow_new_levels = TRUE, transform = TRUE)
      }
      
      if (response_var %in% colnames(data)) {
        y_true <- data[[response_var]]
      } else {
        y_true <- NULL
        message("Response variable not found in data")
      }
    } else{ 
      if (scale == "response") {
        preds <- brms::posterior_predict(model)
      } else if (scale == "linear") {
        preds <- brms::posterior_linpred(model, transform = FALSE)
      } else if (scale == "linear_inverse") {
        preds <- brms::posterior_linpred(model, transform = TRUE)
      }
      
      y_true <- model$data[[response_var]]  
    }
  }
  
  # Create Predictions
  pred <- apply(preds, 2, mean)
  if (scale == "linear") {
    warning("Prediction is made on the linear scale (without transformation)")
  } else if (scale == "linear_inverse") {
    warning("Prediction is made on the linear scale (with transformation)")
  }
  
  # Relative Standard Error (RSE)
  rse <- apply(preds, 2, function(x) sd(x) / mean(x)) * 100
  rse_model <- mean(rse)
  
  if (!is.null(y_true) & !anyNA(y_true)) {
    # Create a summary table
    result_table <- data.frame(
      Prediction = pred,
      Actual = y_true,
      RSE_percent = rse
    )
  } else {
    # Create a summary table
    result_table <- data.frame(
      Prediction = pred,
      RSE_percent = rse
    )
  }
  
  # Return an object of class 'hbsae'
  return(structure(
    list(result_table = result_table, rse_model = rse_model, pred=pred),
    class = "hbsae"
  ))
}
