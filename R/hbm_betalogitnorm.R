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
#' @name hbm_betalogitnorm
#' 
#' @param response The dependent (outcome) variable in the model. This variable represents the main response being predicted or analyzed.
#' @param predictors A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.
#' @param n The number of sample units for each region used in the survey
#' @param deff Design Effect
#' @param link_phi Link function for the second parameter (phi), typically representing precision, shape, or dispersion depending on the family used (e.g., "log", "identity")
#' @param group The name of the grouping variable (e.g., area, cluster, region)
#' used to define the hierarchical structure for random effects. This variable should
#' correspond to a column in the input data and is typically used to model area-level
#' variation through random intercepts
#' @param sre An optional grouping factor mapping observations to spatial locations. 
#' If not specified, each observation is treated as a separate location. 
#' It is recommended to always specify a grouping factor to allow for handling of new data in postprocessing methods.
#' @param sre_type Determines the type of spatial random effect used in the model. The function currently supports "sar" and "car"
#' @param car_type Type of the CAR structure. Currently implemented are "escar" (exact sparse CAR), "esicar" (exact sparse intrinsic CAR), 
#' "icar" (intrinsic CAR), and "bym2". 
#' @param sar_type Type of the SAR structure. Either "lag" (for SAR of the response values) or 
#' "error" (for SAR of the residuals). 
#' @param M The M matrix in SAR is a spatial weighting matrix that shows the spatial relationship between locations with certain 
#' weights, while in CAR, the M matrix is an adjacency matrix that only contains 0 and 1 to show the proximity between locations. 
#' SAR is more focused on spatial influences with different intensities, while CAR is more on direct adjacency relationships. 
#' If sre is specified, the row names of M have to match the levels of the grouping factor
#' @param data Dataset used for model fitting
#' @param prior Priors for the model parameters (default: `NULL`).
#' Should be specified using the `brms::prior()` function or a list of such objects. 
#' For example, `prior = prior(normal(0, 1), class = "b")` sets a Normal(0,1) prior on the regression coefficients. 
#' Multiple priors can be combined using `c()`, e.g., 
#' `prior = c(prior(normal(0, 1), class = "b"), prior(exponential(1), class = "sd"))`.
#' If `NULL`, default priors from `brms` will be used.
#' @param handle_missing Mechanism to handle missing data (NA values) to ensure model stability and avoid estimation errors. 
#' Three approaches are supported. 
#' The `"deleted"` approach performs complete case analysis by removing all rows with any missing values before model fitting. 
#' This is done using a simple filter such as `complete.cases(data)`. 
#' It is recommended when the missingness mechanism is Missing Completely At Random (MCAR).
#' The `"multiple"` approach applies multiple imputation before model fitting. 
#' Several imputed datasets are created (e.g., using the `mice` package or the `brm_multiple()` function in `brms`), 
#' the model is fitted separately to each dataset, and the results are combined. 
#' This method is suitable when data are Missing At Random (MAR).
#' The `"model"` approach uses model-based imputation within the Bayesian model itself. 
#' Missing values are incorporated using the `mi()` function in the model formula (e.g., `y ~ mi(x1) + mi(x2)`), 
#' allowing the missing values to be jointly estimated with the model parameters. 
#' This method also assumes a MAR mechanism and is applicable only for continuous variables.
#' If data are suspected to be Missing Not At Random (MNAR), none of the above approaches directly apply. 
#' Further exploration, such as explicitly modeling the missingness process or conducting sensitivity analyses, is recommended.
#' @param m Number of imputations to perform when using the `"multiple"` approach for handling missing data (default: 5). 
#' This parameter is only used if `handle_missing = "multiple"`. 
#' It determines how many imputed datasets will be generated. 
#' Each imputed dataset is analyzed separately, and the posterior draws are then combined to account for both within-imputation and between-imputation variability, 
#' following Rubin’s rules. A typical choice is between 5 and 10 imputations, but more may be needed for higher missingness rates.
#' @param control A list of control parameters for the sampler (default: `list()`)
#' @param chains Number of Markov chains (default: 4)
#' @param iter Total number of iterations per chain (default: 4000)
#' @param warmup Number of warm-up iterations per chain (default: floor(iter/2))
#' @param cores Number of CPU cores to use (default: 1)
#' @param sample_prior (default: "no")
#' @param stanvars An optional `stanvar` or combination of `stanvar` objects used to define the hyperpriors for the hyperparameter \code{phi}. 
#' By default, if \code{phi} is not fixed, a gamma prior is used: \code{phi ~ gamma(alpha, beta)}, where \code{alpha} and \code{beta} can be defined via \code{stanvars}.
#' Use \code{"+"} to combine multiple \code{stanvar} definitions.
#' 
#' For example:
#' \code{
#' stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
#' stanvar(scode = "beta ~ gamma(1, 1);", block = "model")
#' }
#'
#' To use the default hyperprior for \code{phi}, set \code{stanvars = NULL}.
#' @param ... Additional arguments passed to the `brm()` function.
#' 
#' @return A `hbmfit` object
#' 
#' @importFrom stats as.formula
#' @importFrom brms stanvar set_prior 
#' 
#' @export
#' 
#' @author Sofi Zamzanah
#'
#' @references 
#' Liu, B. (2009). *Hierarchical Bayes Estimation and Empirical Best Prediction of Small-Area Proportions*. College Park, University of Maryland.
#' Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*. John Wiley & Sons, page 390.
#' Gelman, A. (2006). *Prior Distributions for Variance Parameters in Hierarchical Models (Comment on Article by Browne and Draper)*. Bayesian Analysis, 1(3), 527–528. 
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). *A Weakly Informative Default Prior Distribution for Logistic and Other Regression Models*.
#' 
#' @examples
#' \dontrun{
#' 
#' # Load the example dataset
#' library(hbsaems)
#' data("data_betalogitnorm")
#'
#' # Prepare the dataset
#' data <- data_betalogitnorm
#'
#' # Fit Beta Model
#' model1 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' data = data
#' )
#' summary(model1)
#' 
#' if you have the information of n and deff values you can use the following model
#' model1 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' n = "n",
#' deff = "deff",
#' data = data
#' )
#' summary(model1)
#' 
#' From this stage to the next will be explained the construction of the model with 
#' the condition that the user has information on the value of n and deff. 
#' If you do not have information related to the value of n and deff 
#' then simply delete the parameters n and deff in your model.
#' 
#' # Fit Beta Model with Grouping Variable as Random Effect
#' model2 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' n = "n",
#' deff = "deff",
#' group = "group",
#' data = data
#' )
#' summary(model2)
#'
#' # Fit Beta Model With Missing Data
#' data_miss <- data
#' data_miss[5:7, "y"] <- NA
#'
#' # a. Handling missing data by deleted (Only if missing in response)
#' model3 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' n = "n",
#' deff = "deff",
#' data = data_miss,
#' handle_missing = "deleted"
#' )
#' summary(model3)
#'
#' # b. Handling missing data using multiple imputation (m=5)
#' model4 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' n = "n",
#' deff = "deff",
#' data = data_miss,
#' handle_missing = "multiple"
#' )
#' summary(model4)
#' 
#' # c. Handle missing data during model fitting using mi()
#' data_miss <- data
#' data_miss$x1[3:5] <- NA 
#' data_miss$x2[14:17] <- NA 
#' model5 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' n = "n",
#' deff = "deff",
#' group = "group",
#' data = data_miss,
#' handle_missing = "model"
#' )
#'
#' # Fit Logit-Normal Model With Spatial Effect
#' data("adjacency_matrix_car")
#' M <- adjacency_matrix_car
#'
#' model6 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' n = "n",
#' deff = "deff",
#' sre = "sre",
#' sre_type = "car",
#' M = M,
#' data = data
#' )
#' summary(model6)
#' 

#' 
#' have input of argument stanvars
#' 
#' model7 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' data = data,
#' stanvars = stanvar(scode = "alpha ~ gamma(2, 1);", block = "model") +
#' stanvar(scode = "beta ~ gamma(1, 1);", block = "model") #stanvars of alpha and beta
#)
#' 
#' summary(model7)
#' 
#' have input of argument stanvars
#' 
#' model8 <- hbm_betalogitnorm(
#' response = "y",
#' predictors = c("x1", "x2", "x3"),
#' data = data,
#' stanvars = stanvar(scode = "beta ~ gamma(1, 1);", block = "model") #stanvars of beta
#)
#' 
#' summary(model8)
#' 
#' }
hbm_betalogitnorm <- function(response,
                     predictors,
                     n = NULL,
                     deff = NULL,
                     link_phi = "identity",
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
  
  if(!is.null(group)){
    if (!(group %in% names(data))) {
      stop(sprintf("Variable '%s' not found in the data.", group))
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
  
  # Prior Handling
  # Function to check whether a general prior is present for a given class
  has_general_prior <- function(prior_list_internal, prior_class) {
    if (is.null(prior_list_internal) || !inherits(prior_list_internal, "brmsprior"))
      return(FALSE)
    
    # Return TRUE if any prior matches the specified class and has no specific coefficient
    any(prior_list_internal$class == prior_class &
          (is.na(prior_list_internal$coef) | prior_list_internal$coef == ""))
  }
  
  ###### Defines stanvars of alpha and beta if not provided###
 
  
  #  Extract a list of stanvar elements from the input
  extract_stanvar_list <- function(stanvars) {
    if (is.null(stanvars)) {
      return(list())
    } else if (inherits(stanvars, "stanvar")) {
      # Single prior → wrap it in a list
      return(list(stanvars))
    } else if (inherits(stanvars, "stanvars")) {
      # Multiple priors combined via + → unclass to get the underlying list
      return(unclass(stanvars))
    }
    stop("Input must be the result of stanvar(...) or NULL")
  }
  
  # Get all variable names already defined in block = "model"
  get_model_vars <- function(stanvars) {
    sv_list <- extract_stanvar_list(stanvars)
    vars <- character()
    for (sv in sv_list) {
      if (!is.null(sv$block) && sv$block == "model") {
        # Capture the name on the left side of '~'
        m <- regexec("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*~", sv$scode)
        mm <- regmatches(sv$scode, m)[[1]]
        if (length(mm) >= 2) vars <- c(vars, mm[2])
      }
    }
    unique(vars)
  }
  
  #  Main function: add default priors for alpha and beta only if they're missing
  add_default_stanvars <- function(stanvars) {
    existing <- get_model_vars(stanvars)
    
    to_add <- list()
    if (!("alpha" %in% existing)) {
      to_add[[length(to_add) + 1]] <- stanvar(
        scode = "alpha ~ gamma(1, 1);",
        block = "model"
      )
    }
    if (!("beta" %in% existing)) {
      to_add[[length(to_add) + 1]] <- stanvar(
        scode = "beta ~ gamma(1, 1);",
        block = "model"
      )
    }
    
    # If nothing to add, return the input as-is
    if (length(to_add) == 0) return(stanvars)
    
    # Combine all new defaults into one stanvars object
    new_sv <- Reduce(`+`, to_add)
    
    # If input is NULL, just return the defaults; otherwise combine with input
    if (is.null(stanvars)) {
      return(new_sv)
    } else {
      return(stanvars + new_sv)
    }
  }
  
  #####
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
    
    if (is.null(prior)) {
      adjusted_prior <- c(
        brms::set_prior("student_t(4,0,10)" , class = "Intercept"),
        brms::set_prior("student_t(4,0,2.5)", class = "b")
      )
    } else {
      adjusted_prior <- prior
      
      if (any(adjusted_prior$class == "phi")){
        stop("Remove priors for 'phi' if phi is fixed using n and deff.")
      }
      
      if (!has_general_prior(adjusted_prior, "Intercept")) {
        adjusted_prior <- c(adjusted_prior, brms::set_prior("student_t(4,0,10)", class = "Intercept"))
      }
      
      if (!has_general_prior(adjusted_prior, "b")) {
        adjusted_prior <- c(adjusted_prior, brms::set_prior("student_t(4,0,2.5)", class = "b"))
      }
      
    }
    
    formula <- brms::bf(formula, phi ~ 0 + offset(phi_fixed))
  } else {
    # Define default prior
    stanvar_par <- 
      stanvar(scode = "
          real<lower=1> alpha;
          real<lower=0> beta;
        ", block = "parameters") 
      
    if (is.null(prior)) {
  
      stanvars <- stanvar_par + add_default_stanvars(stanvars)
      adjusted_prior <- c(
        brms::set_prior("student_t(4,0,10)", class = "Intercept"),
        brms::set_prior("student_t(4,0,2.5)", class = "b"),
        brms::set_prior("gamma(alpha, beta)", class = "phi")
      )
    } else {
        # Use the provided prior
        adjusted_prior <- prior
        
        # If no general prior for the intercept exists, add a default Student t  prior
        if (!has_general_prior(adjusted_prior, "Intercept")) {
          adjusted_prior <- c(adjusted_prior, brms::set_prior("student_t(4,0,10)", class = "Intercept"))
        }
        
        # If no general prior for the coefficients exists, add a default Student t prior
        if (!has_general_prior(adjusted_prior, "b")) {
          adjusted_prior <- c(adjusted_prior, brms::set_prior("student_t(4,0,2.5)", class = "b"))
        }
        
        # If no prior for phi is provided, add gamma(alpha, beta)
        if (!has_general_prior(adjusted_prior, "phi")) {
          stanvar_par<- 
            stanvar(scode = "
              real<lower=1> alpha;
              real<lower=0> beta;
            ", block = "parameters")
          stanvars <- stanvar_par + add_default_stanvars(stanvars)
          adjusted_prior <- c(adjusted_prior, brms::set_prior("gamma(alpha, beta)", class = "phi"))
        }
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
  if (!is.null(group)) {
    formula_re <- as.formula(paste("~", paste("(1 | ", group, ")", collapse = " + ")))
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
               prior = adjusted_prior,
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
