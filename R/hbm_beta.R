#' @title Small Area Estimation using Hierarchical Bayesian under Beta Distribution
#'  
#' @description This function is implemented a **Hierarchical Bayesian Small Area Estimation (HBSAE)** model  
#' under a **beta distribution** using **Bayesian inference** with the `brms` package. 
#' 
#' The range of the variable data \eqn{(y)} that is intended as a beta distribution must be \eqn{0<y<1}.
#' The data proportion is supposed to be implemented with this function.
#' 
#' The function utilizes the **Bayesian regression modeling framework** provided by `brms`,  
#' which interfaces with **Stan** for efficient Markov Chain Monte Carlo (MCMC) sampling.  
#' The `brm()` function from `brms` is used to estimate posterior distributions based on user-defined  
#' hierarchical and spatial structures.
#' 
#' @name hbm_beta
#' 
#' @usage hbm_beta(response, 
#'                 predictors, 
#'                 n = NULL, 
#'                 deff = NULL, 
#'                 data, 
#'                 prior = NULL)
#' 
#' @param response The dependent (outcome) variable in the model. This variable represents the main response being predicted or analyzed.
#' @param predictors A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.
#' @param n The number of sample units for each region used in the survey
#' @param deff Design Effect
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
#' @param iter Total number of iterations per chain (default: 4000)
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
#' # Proses pengerjaan ya guys, belum menemukan data yang cocok bgt bisa n deff dan spatial 
#' }

hbm_beta <- function(response,
                     predictors,
                     n = NULL,
                     deff = NULL,
                     link_phi = "identity",
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
                     stanvars = NULL,
                     ...){
  
  # Ensure response and predictors exist in the data
  if (!(response %in% names(data))) {
    stop(sprintf("Variable '%s' not found in the data.", response))
  }
  
  if (!all(predictors %in% names(data))) {
    missing_predictors <- predictors[!predictors %in% names(data)]
    stop(sprintf("Variable '%s' not found in the data.", paste(missing_predictors, collapse = ", ")))
  }
  
  if (!is.null(n)){
    if (!(n %in% names(data))) {
      stop(sprintf("Variable '%s' not found in the 'data'.", n))
    }
  }
  
  if(!is.null(deff)){
    if (!(deff %in% names(data))) {
      stop(sprintf("Variable '%s' not found in the data.", deff))
    }
  }
  
  # Validate n and deff
  if (xor(is.null(n), is.null(deff))) {
    stop("Both variables 'n' and 'deff' must be specified or undefined simultaneously.")
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

  # Check for response values
  if (any(data[[response]] <= 0, na.rm = TRUE) | any(data[[response]] >= 1, na.rm = TRUE)) {
    stop("Response variable must be between 0 and 1.")
  }

  # Add fixed effect formula
  fixed_effects <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste(response, "~", fixed_effects))
  
  # Define phi
  if (!is.null(n) && !is.null(deff)) {
    # Check for missing values in n or deff
    if (anyNA(data[[n]]) || anyNA(data[[deff]])) {
      stop("Missing values detected in either 'n' or 'deff'.")
    }
    
    # Check if n and deff are non-positive
    if (any(data[[n]] <= 0) || any(data[[deff]] <= 0)) {
      stop("Both 'n' and 'deff' must be strictly positive values.")
    }
    
    phi_fixed <- ((data[[n]] / data[[deff]]) - 1)
    
    if (any(phi_fixed <= 0)) {
      stop("The phi value should be positive, but we found phi <=0. Check your n and deff values again.")
    }
    
    data$phi_fixed <- phi_fixed
    
    prior <- c(
      set_prior("", class = "b")              
    )
    
    formula <- brms::bf(formula, phi ~ 0 + offset(phi_fixed))
  } else {
    # Define default prior
    if (is.null(prior)) {
      
      stanvars <- 
        stanvar(scode = "
          real<lower=0> alpha;
          real<lower=0> beta;
        ", block = "parameters") +
        stanvar(scode = "
          alpha ~ gamma(1, 1);
          beta ~ gamma(1, 1);
        ", block = "model")
      
      prior <- c(
        set_prior("", class = "b"),              # Flat prior untuk koefisien β
        set_prior("gamma(alpha, beta)", class = "phi") # Prior gamma untuk phi
      )
    }
    
    formula <- brms::bf(formula)
  }

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
                               response)
    
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
    
    # After adding mi(), the formula has become a standard formula. Adding additional components...
    if (!is.null(formula$pforms) && length(formula$pforms) > 0) {
      main_formula$pforms <- formula$pforms
    } 
    
    # Create additional formulas for variables that have missing values
    auxiliary_formulas <- lapply(missing_vars, function(var) {
      if (var %in% response) return(NULL)  # Do not create an additional model for the response
      
      # Only use variables from the initial formula as predictors
      predictors_for_var <- setdiff(predictors, var)
      predictors_for_var <- predictors_for_var[!sapply(data[predictors_for_var], function(x) any(is.na(x)))]  # Avoid predictors that are also missing
      
      # If there are no valid predictors, use the intercept (1)
      if (length(predictors_for_var) > 0) {
        return(brms::bf(as.formula(paste0(var, " | mi() ~ ", paste(predictors_for_var, collapse = " + ")))))
      } else {
        return(brms::bf(as.formula(paste0(var, " | mi() ~ 1"))))  # Use the intercept (1) if there are no valid predictors
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
  
  #Model fitting
  model <- hbm(formula = all_formulas,
               hb_sampling= "Beta",
               hb_link = "logit",
               link_phi = link_phi,
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
               control = control,
               chains = chains,
               iter = iter,
               warmup = warmup,
               cores = cores,
               sample_prior = sample_prior,
               stanvars = stanvars,
               ...)
  return(model)
}