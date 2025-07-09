#' @title Small Area Estimation using Hierarchical Bayesian under Logit-Normal Model
#'
#' @description
#' This function implements a **Hierarchical Bayesian Small Area Estimation (HBSAE)**
#' under a **Logit-Normal Model** using **Bayesian inference** with the `brms` package.
#'
#' The model accounts for **fixed effects**, **random effects**, **spatial random effects (CAR/SAR models)**,
#' and **measurement error correction**, allowing for robust small area estimation.
#'
#' The function utilizes the **Bayesian regression modeling framework** provided by `brms`,
#' which interfaces with 'Stan' for efficient Markov Chain Monte Carlo (MCMC) sampling.
#' The `brm()` function from `brms` is used to estimate posterior distributions based on user-defined
#' hierarchical and spatial structures.
#'
#' @name hbm_binlogitnorm
#'
#' @param response The dependent (outcome) variable in the model. This variable represents the count of successes in a Binomial distribution.
#' @param trials Specifies the number of trials in a binomial model. This is required for binomial family models where the response variable is specified as a proportion.
#' @param predictors A list of independent (explanatory) variables used in the model. These variables form the fixed effects in the regression equation.
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
#' @param iter Total number of iterations per chain (default: 2000)
#' @param warmup Number of warm-up iterations per chain (default: floor(iter/2))
#' @param cores Number of CPU cores to use (default: 1)
#' @param sample_prior (default: "no")
#' @param ... Additional arguments
#'
#' @return A `hbmfit` object
#' 
#' @importFrom stats as.formula
#' @importFrom brms set_prior 
#' 
#' @export
#' 
#' @author Saniyyah Sri Nurhayati
#'
#' @references 
#' Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*. John Wiley & Sons, page 390.
#' Gelman, A. (2006). Prior Distributions for Variance Parameters in Hierarchical Models (Comment on Article by Browne and Draper). Bayesian Analysis, 1(3), 527–528. 
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A Weakly Informative Default Prior Distribution for Logistic and Other Regression Models.
#'
#' @examples
#' \dontrun{
#' 
#' # Load the example dataset
#' library(hbsaems)
#' data("data_binlogitnorm")
#'
#' # Prepare the dataset
#' data <- data_binlogitnorm
#'
#' # Fit Logit-Normal Model
#' model1 <- hbm_binlogitnorm(
#' response = "y",
#' trials = "n",
#' predictors = c("x1", "x2", "x3"),
#' data = data
#' )
#' summary(model1)
#'
#' # Fit Logit-Normal Model with Grouping Variable as Random Effect
#' model2 <- hbm_binlogitnorm(
#' response = "y",
#' trials = "n",
#' predictors = c("x1", "x2", "x3"),
#' group = "group",
#' data = data
#' )
#' summary(model2)
#'
#' # Fit Logit-Normal Model With Missing Data
#' data_miss <- data
#' data_miss[5:7, "y"] <- NA
#'
#' # a. Handling missing data by deleted (Only if missing in response)
#' model3 <- hbm_binlogitnorm(
#' response = "y",
#' trials = "n",
#' predictors = c("x1", "x2", "x3"),
#' data = data_miss,
#' handle_missing = "deleted"
#' )
#' summary(model3)
#'
#' # b. Handling missing data using multiple imputation (m=5)
#' model4 <- hbm_binlogitnorm(
#' response = "y",
#' trials = "n",
#' predictors = c("x1", "x2", "x3"),
#' data = data_miss,
#' handle_missing = "multiple"
#' )
#' summary(model4)
#'
#' # Fit Logit-Normal Model With Spatial Effect
#' data("adjacency_matrix_car")
#' M <- adjacency_matrix_car
#'
#' model5 <- hbm_binlogitnorm(
#' response = "y",
#' trials = "n",
#' predictors = c("x1", "x2", "x3"),
#' sre = "sre",
#' sre_type = "car",
#' M = M,
#' data = data
#' )
#' summary(model5)
#' 
#' } 
#' 
hbm_binlogitnorm <- function(response,
                            trials,
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
  
  if (!all(trials %in% names(data))) {
    stop("Trials not found in 'data'.")
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
  
  # Check handle missing for discreate distribution
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
  if (!is.null(group)) {
    formula_re <- as.formula(paste("~", paste("(1 |", group, ")", collapse = " + ")))
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
