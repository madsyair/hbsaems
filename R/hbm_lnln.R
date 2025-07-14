#' @title Small Area Estimation using Hierarchical Bayesian under Lognormal Distribution
#'
#' @description 
#' This function implements a **Hierarchical Bayesian Small Area Estimation (HBSAE)** model
#' under a **Lognormal distribution** using **Bayesian inference** with the `brms` package.
#'
#' The response variable \eqn{y_i} in area \eqn{i} is assumed to follow a lognormal distribution:
#'
#' \deqn{y_i \mid \theta_i, \psi_i \sim \mathrm{Lognormal}(\theta_i, \psi_i)}
#'
#' which implies:
#'
#' \deqn{\log(y_i) \sim \mathcal{N}(\theta_i, \psi_i)}
#'
#'
#' The function utilizes the **Bayesian regression modeling framework** provided by `brms`,
#' which interfaces with 'Stan' for efficient Markov Chain Monte Carlo (MCMC) sampling.
#' The `brm()` function from `brms` is used to estimate posterior distributions based on user-defined
#' hierarchical and spatial structures.
#'
#' @name hbm_lnln
#' @param response A non-negative (x>0) dependent (outcome) variable assumed to follow a lognormal distribution.
#' @param predictors A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.
#' @param group The name of the grouping variable (e.g., area, cluster, region)
#' used to define the hierarchical structure for random effects. This variable should
#' correspond to a column in the input data and is typically used to model area-level
#' variation through random intercepts.
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
#' @param iter Total number of iterations per chain (default: 2000)
#' @param warmup Number of warm-up iterations per chain (default: floor(iter/2))
#' @param cores Number of CPU cores to use (default: 1)
#' @param sample_prior (default: "no")
#' @param ... Additional arguments
#'
#' @return A `hbmfit` object
#' 
#' @importFrom stats as.formula

#' 
#' @export
#' 
#' @author Arsyka Laila Oktalia Siregar
#'
#' @source 
#' Fabrizi, E., Ferrante, M. R., & Trivisano, C. (2018). Bayesian small area estimation for skewed business survey variables. Journal of the Royal Statistical Society. Series C (Applied Statistics), 67(4), 863–864. https://www.jstor.org/stable/26800576;
#' Gelman, A. (2006). Prior Distributions for Variance Parameters in Hierarchical Models (Comment on Article by Browne and Draper). Bayesian Analysis, 1(3), 527–528;
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A Weakly Informative Default Prior Distribution for Logistic and Other Regression Models.
#'
#' @examples
#' \donttest{
#' # Load necessary libraries
#' library(hbsaems)
#'
#' # Load custom dataset
#' data <- data_lnln
#' head(data)
#'
#' # --- 1. Prior Predictive Check ---
#' model.check_prior <- hbm_lnln(
#' response = "y_obs",
#' predictors = c("x1", "x2", "x3"),
#' group = "group",
#' data = data,
#' prior = c(
#' prior(normal(0.1, 0.1), class = "b"),
#' prior(normal(1, 1), class = "Intercept")
#' ),
#' sample_prior = "only",
#' iter = 4000,
#' warmup = 2000,
#' chains = 2,
#' seed = 123
#' )
#' hbpc(model.check_prior, response_var = "y_obs")
#'
#' # --- 2. Fit the Model with Data ---
#' model <- hbm_lnln(
#' response = "y_obs",
#' predictors = c("x1", "x2", "x3"),
#' group = "group",
#' data = data,
#' prior = c(
#' prior(normal(0.1, 0.1), class = "b"),
#' prior(normal(1, 1), class = "Intercept")
#' ),
#' iter = 10000,
#' warmup = 5000,
#' chains = 1,
#' seed = 123
#' )
#' summary(model)
#'
#' # --- 3. Fit Model with Spatial Effect (CAR) ---
#' M <- adjacency_matrix_car
#'
#' model.spatial <- hbm_lnln(
#' response = "y_obs",
#' predictors = c("x1", "x2", "x3"),
#' group = "group",
#' sre = "sre", # Spatial grouping variable (must match rows/cols of M)
#' sre_type = "car", # Spatial random effect type
#' car_type = "icar", # CAR model type
#' M = M, # Adjacency matrix (must be symmetric)
#' data = data,
#' prior = c(
#' prior(normal(0.1, 0.1), class = "b"),
#' prior(normal(1, 1), class = "Intercept")
#' ),
#' iter = 10000,
#' warmup = 5000,
#' chains = 1,
#' seed = 123
#' )
#' summary(model.spatial)
#' }

hbm_lnln <- function(
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
  
  # Check for non-positive or zero response values
  if (any(data[[response]] <= 0, na.rm = TRUE)) {
    stop("The response variable 'response' contains zero or negative values. Lognormal distribution requires strictly positive response values.")
  }

  # Build fixed effect formula
  fixed_effects <- paste(predictors, collapse = " + ")
  formula <- as.formula(paste(response, "~", fixed_effects))

  # Add random effect
  if (!is.null(group)) {
    formula_re <- as.formula(paste("~", paste("(1 | ", group, ")", collapse = " + ")))
  } else {
    formula_re <- NULL
  }
  
  # Prior Handling
  # Function to check whether a general prior is present for a given class
  has_general_prior <- function(prior_list_internal, prior_class) {
    if (is.null(prior_list_internal) || !inherits(prior_list_internal, "brmsprior"))
      return(FALSE)
    
    # Return TRUE if any prior matches the specified class and has no specific coefficient
    any(prior_list_internal$class == prior_class &
          (is.na(prior_list_internal$coef) | prior_list_internal$coef == ""))
  }
  
  if (is.null(prior)) {
    adjusted_prior <- c(
      brms::set_prior("student_t(4,0,10)", class = "Intercept"),
      brms::set_prior("student_t(4,0,2.5)", class = "b")
    )
  } else {
    # Use the provided prior
    adjusted_prior <- prior
    
    # If no general prior for the intercept exists, add a default student t prior
    if (!has_general_prior(adjusted_prior, "Intercept")) {
      adjusted_prior <- c(adjusted_prior, brms::set_prior("student_t(4,0,10)", class = "Intercept"))
    }
    
    # If no general prior for the coefficients exists, add a default student t prior
    if (!has_general_prior(adjusted_prior, "b")) {
      adjusted_prior <- c(adjusted_prior, brms::set_prior("student_t(4,0,2.5)", class = "b"))
    }
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
               prior = adjusted_prior,
               control = list(),
               chains = chains,
               iter = iter,
               warmup = warmup,
               cores = cores,
               sample_prior = sample_prior,
               ...)
  return(model)
}
