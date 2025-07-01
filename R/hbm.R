#' Hierarchical Bayesian Small Area Models
#' @title hbm : Hierarchical Bayesian Small Area Models
#' 
#' @description This function provide flexible modeling approaches to estimate area-level statistics
#' while incorporating auxiliary information and spatial structures. This function allows users to fit Bayesian models 
#' using the `brms` package and supports Gaussian, Bernoulli, Poisson, and other distributions. It also accommodates 
#' spatial random effects (CAR and SAR) and missing data handling (deletion, model-based imputation, and multiple imputation).
#' 
#' @name hbm
#'
#' @param formula Formula specifying the model structure of auxiliary variables and direct estimates
#'                The formula must be provided as a `brmsformula` or `formula` object. For multivariate models with multiple 
#'                auxiliary variables, use the `+` operator to combine multiple `bf()` formulas.
#'                Example: `formula(y ~ x1 + x2 + x3)`, `bf(y ~ x1 + x2 + x3)`, or `bf(y | mi() ~ mi(x1)) + bf(x1 | mi() ~ x2)`
#' @param hb_sampling  A character string naming the distribution family of the response variable to be 
#'                     used in the model  (e.g., "gaussian", "bernoulli", "poisson")
#' @param hb_link  A specification for the model link function. This can be a name/expression or 
#'                 character string. See the ’Details’ section for more information on link functions supported by each family.
#' @param link_phi Link function for the second parameter (phi), typically representing precision, shape, or dispersion 
#'                 depending on the family used (e.g., "log", "identity")
#' @param re  Random effects formula specifying the grouping structure in the data. 
#'            For example, re = ~(1|area), where "area" is the grouping variable or cluster ID indicating 
#'            that observations within the same area share a common random effect. If not specified, 
#'            each row will be treated as its own group, meaning a separate random effect is estimated for each observation.
#' @param sre An optional grouping factor mapping observations to spatial locations. 
#'            If not specified, each observation is treated as a separate location. 
#'            It is recommended to always specify a grouping factor to allow for handling of new data in postprocessing methods.
#' @param sre_type Determines the type of spatial random effect used in the model. The function currently supports "sar" and "car"
#' @param car_type Type of the CAR structure. Currently implemented are "escar" (exact sparse CAR), "esicar" (exact sparse intrinsic CAR), 
#'                 "icar" (intrinsic CAR), and "bym2". 
#' @param sar_type Type of the SAR structure. Either "lag" (for SAR of the response values) or 
#'                 "error" (for SAR of the residuals). 
#' @param M The M matrix in SAR is a spatial weighting matrix that shows the spatial relationship between locations with certain 
#'          weights, while in CAR, the M matrix is an adjacency matrix that only contains 0 and 1 to show the proximity between locations. 
#'          SAR is more focused on spatial influences with different intensities, while CAR is more on direct adjacency relationships. 
#'          If sre is specified, the row names of M have to match the levels of the grouping factor
#' @param data Dataset used for model fitting
#' @param prior Priors for the model parameters (default: `NULL`).
#'              Should be specified using the `brms::prior()` function or a list of such objects. 
#'              For example, `prior = prior(normal(0, 1), class = "b")` sets a Normal(0,1) prior on the regression coefficients. 
#'              Multiple priors can be combined using `c()`, e.g., 
#'              `prior = c(prior(normal(0, 1), class = "b"), prior(exponential(1), class = "sd"))`.
#'              If `NULL`, default priors from `brms` will be used.
#' @param handle_missing Mechanism to handle missing data (NA values) to ensure model stability and avoid estimation errors. 
#'                       Three approaches are supported. 
#'                       The `"deleted"` approach performs complete case analysis by removing all rows with any missing values before model fitting. 
#'                       This is done using a simple filter such as `complete.cases(data)`. 
#'                       It is recommended when the missingness mechanism is Missing Completely At Random (MCAR).
#'                       The `"multiple"` approach applies multiple imputation before model fitting. 
#'                       Several imputed datasets are created (e.g., using the `mice` package or the `brm_multiple()` function in `brms`), 
#'                       the model is fitted separately to each dataset, and the results are combined. 
#'                       This method is suitable when data are Missing At Random (MAR).
#'                       The `"model"` approach uses model-based imputation within the Bayesian model itself. 
#'                       Missing values are incorporated using the `mi()` function in the model formula (e.g., `y ~ mi(x1) + mi(x2)`), 
#'                       allowing the missing values to be jointly estimated with the model parameters. 
#'                       This method also assumes a MAR mechanism and is applicable only for continuous variables.
#'                       If data are suspected to be Missing Not At Random (MNAR), none of the above approaches directly apply. 
#'                       Further exploration, such as explicitly modeling the missingness process or conducting sensitivity analyses, is recommended.
#' @param m Number of imputations to perform when using the `"multiple"` approach for handling missing data (default: 5). 
#'          This parameter is only used if `handle_missing = "multiple"`. 
#'          It determines how many imputed datasets will be generated. 
#'          Each imputed dataset is analyzed separately, and the posterior draws are then combined to account for both within-imputation and between-imputation variability, 
#'          following Rubin’s rules. A typical choice is between 5 and 10 imputations, but more may be needed for higher missingness rates.
#' @param control A list of control parameters for the sampler (default: `list()`)
#' @param chains Number of Markov chains (default: 4)
#' @param iter Total number of iterations per chain (default: 4000)
#' @param warmup Number of warm-up iterations per chain (default: floor(iter/2))
#' @param cores Number of CPU cores to use (default: 1)
#' @param sample_prior Character. Indicates whether draws from priors should be sampled in addition to posterior draws. The options are:
#'                    \code{"no"} (default): Do not draw from priors (only posterior draws are obtained). \code{"yes"}: Draw both from the prior and posterior.
#'                    \code{"only"}: Draw solely from the prior, ignoring the likelihood. which allows among others to generate draws from the prior predictive distribution.
#' @param ... Additional arguments
#' 
#' @return A `hbmfit` object containing : 
#'   \item{model}{Summary of `brms` object.}
#'   \item{handle_missing}{Handle missing option used in the model.}
#'   \item{data}{Data passed to the \code{hbm} function. }
#'   
#' @importFrom brms bf brm brm_multiple brmsfamily 
#' @importFrom stats terms update.formula update as.formula complete.cases
#' @import mice
#' 
#' @export
#' 
#' @author Achmad Syahrul Choir, Saniyyah Sri Nurhayati, and Sofi Zamzanah
#' 
#' @references 
#' Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*. John Wiley & Sons.
#' Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
#'
#' @examples
#' \dontrun{
#' 
#' # Load the example dataset
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
#' # Fit the Basic Model With Defined Random Effect
#' model_with_defined_re <- hbm(
#'   formula = bf(y ~ x1 + x2 + x3),  # Formula model
#'   hb_sampling = "gaussian",                   # Gaussian family
#'   hb_link = "identity",                       # Identity link
#'   re = ~(1 | group),                          # Defined random effect
#'   data = data,
#'   chains = 4,
#'   iter = 4000,
#'   warmup = 2000,
#'   cores = 2
#' )
#' summary(model_with_defined_re)
#' 
#' # Fit the Model with Missing Data
#' # a. Handling missing by deletion
#' data$y[3:5] <- NA 
#' model_deleted <- hbm(
#'   formula = bf(y ~ x1 + x2 + x3),
#'   hb_sampling = "gaussian",
#'   hb_link = "identity",
#'   re = ~(1 | group),
#'   data = data,
#'   handle_missing = "deleted",
#'   chains = 4,
#'   iter = 4000,
#'   warmup = 2000,
#'   cores = 2
#' )
#' summary(model_deleted)
#'
#' # b. Handling missing using multiple imputation
#' model_multiple <- hbm(
#'   formula = bf(y ~ x1 + x2 + x3),
#'   hb_sampling = "gaussian",
#'   hb_link = "identity",
#'   re = ~(1 | group),
#'   data = data,
#'   handle_missing = "multiple",
#'   chains = 4,
#'   iter = 4000,
#'   warmup = 2000,
#'   cores = 2
#' )
#' summary(model_multiple)
#' 
#' # c. Handling missing during modeling
#' data$y[3:5] <- NA 
#' data$x1[6:7] <- NA
#' model_model <- hbm(
#'   formula = bf(y | mi() ~ mi(x1) + x2 + x3) +
#'             bf(x1 | mi() ~ x2 + x3),
#'   hb_sampling = "gaussian",
#'   hb_link = "identity",
#'   re = ~(1 | group),
#'   data = data,
#'   handle_missing = "model",
#'   chains = 4,
#'   iter = 4000,
#'   warmup = 2000,
#'   cores = 2
#' )
#' summary(model_model)
#' 
#' # Fit the Model with Spatial Effect
#' # a. CAR (Conditional Autoregressive)
#' data("adjacency_matrix_car")
#' adjacency_matrix_car
#' 
#' model_spatial_car <- hbm(
#'   formula = bf(y ~ x1 + x2 + x3 ),  
#'   hb_sampling = "gaussian",       
#'   hb_link = "identity",           
#'   data = data,                    
#'   sre = "sre",
#'   sre_type = "car",
#'   M = adjacency_matrix_car,
#'   chains = 4,                    
#'   iter = 4000,                    
#'   warmup = 2000,                  
#'   cores = 2                       
#' )
#' summary(model_spatial_car)
#' 
#' # b. SAR (Simultaneous Autoregressive)
#' data("spatial_weight_sar")
#' spatial_weight_sar
#' 
#' model_spatial_sar <- hbm(
#'   formula = bf(y ~ x1 + x2 + x3 ),  
#'   hb_sampling = "gaussian",       
#'   hb_link = "identity",           
#'   data = data,                    
#'   sre_type = "sar",
#'   M = spatial_weight_sar,
#'   chains = 4,                    
#'   iter = 4000,                    
#'   warmup = 2000,                  
#'   cores = 2                       
#' )
#' 
#' }
hbm <- function(formula,
                hb_sampling = "gaussian",
                hb_link = "identity",
                link_phi = "log",
                re = NULL,
                sre = NULL,
                sre_type = NULL,
                car_type = NULL,
                sar_type = NULL,
                M = NULL,
                data,
                prior = NULL,
                handle_missing= NULL,
                m=5,
                control = list(),
                chains = 4,
                iter = 4000,
                warmup = floor(iter / 2),
                cores = 1,
                sample_prior = "no",
                ...) {
  
  # Convert data to data frame
  data <- data.frame(data)
  n <- nrow(data)
  data2 <- NULL
  
  # Extract main formula anda all formulas (if consist of more than one formula) main formula 
  if (length(class(formula))>1 && class(formula)[2] == "bform"){
    if (!is.null(formula$forms)) {
      main_formula <- as.formula(formula$forms[[1]])
      all_formulas <- formula
    } else {
      main_formula <- as.formula(formula$formula)
      all_formulas <- formula
    }
  } else if (inherits(formula, "formula")) {
    main_formula <- formula
    all_formulas <- brms::bf(formula)
  } else {
    stop("Formula must be specified as fomula() or bf()/brmsformula()")
  }
  
  # Extract variable from main formula
  response_var <- all.vars(main_formula[[2]])  # LHS: Response variable
  auxiliary_vars <- all.vars(main_formula[[3]]) # RHS: Predictors
  
  # Check missing data
  missing_y <- as.list(response_var[sapply(response_var, function(x) any(is.na(data[[x]])))])
  missing_y <- if (length(missing_y) == 0) NULL else missing_y
  missing_x <- as.list(auxiliary_vars[sapply(auxiliary_vars, function(x) any(is.na(data[[x]])))])
  missing_x <- if (length(missing_x) == 0) NULL else missing_x
  
  # Check the suitability of missing data handling
  discrete_families <- c("binomial", "bernoulli", "beta-binomial", "poisson", 
                         "negbinomial", "geometric", "categorical", "multinomial", 
                         "cumulative", "cratio", "sratio", "acat", 
                         "zero_inflated_binomial", "zero_inflated_beta_binomial", 
                         "zero_inflated_poisson", "zero_inflated_negbinomial")
  is_discrete <- hb_sampling %in% discrete_families
  if (is.null(handle_missing)) {
    if (!is.null(missing_y) || !is.null(missing_x)) {
      stop("Error: `handle_missing` must be specified when there are missing values. ",
           "Please set it to 'deleted', 'model', or 'multiple'.")
    }
  } else {
    # If handle_missing is specified, check compatibility
    if (is_discrete && handle_missing == "model") {
      stop("Error: Discrete distributions do not support `handle_missing = 'model'`. ",
           "Please use `'multiple'` instead.")
    }
  }
  
  # Initial condition
  data_complete <- data
  multiple <- FALSE
  
  # Missing data handling
  if (!is.null(missing_y) || !is.null(missing_x)){
    if (handle_missing == "deleted") {
      if (is.null(missing_x)) {
        # If response_var contains more than one variable (e.g., y and n in logistic models),
        # ensure that both are not NA in the retained rows.
        if (length(response_var) > 1) {
          data <- data[complete.cases(data[response_var]), , drop = FALSE]
        } else {
          # If response_var has only one variable, remove rows where it is NA.
          data <- data[!is.na(data[[response_var]]), , drop = FALSE]
          n <- nrow(data)
        }
        message("Rows with missing response variable were removed due to handle_missing = 'deleted'.")
      } else {
        stop("Option 'deleted' is only applicable when x is complete and y has missing values.")  
      }
    } else if (handle_missing == "multiple") {
      multiple <- TRUE
      message(paste("Missing data detected. Using brms_multiple imputation with m =", m))
    } else if (handle_missing == "model") {
      # Check if the user provided a formula
      if (!is.null(formula)) {
        missing_vars <- c(missing_y, missing_x)
        
        # Convert formulas to strings for validation
        if (!is.null(formula$forms)) {
          formula_strs <- sapply(all_formulas$forms, function(v) paste(deparse(v$formula), collapse = " "))
        } else {
          formula_strs <- paste(deparse(all_formulas$formula), collapse = " ")
        }
        
        # Check if missing variables have '| mi()'
        incomplete_responses <- missing_vars[!sapply(missing_vars, function(var) {
          any(grepl(paste0("\\b", var, " \\| mi\\("), formula_strs))
        })]
        
        incomplete_predictors <- missing_x[!sapply(missing_x, function(var) {
          any(grepl(paste0("mi\\(", var, "\\)"), formula_strs))
        })]
        
        if (length(incomplete_responses) > 0 || length(incomplete_predictors) > 0) {
          stop(paste("Error: The formula is incomplete. Missing '| mi()' for response:", 
                     paste(incomplete_responses, collapse = ", "), 
                     "or missing 'mi()' for predictors:",
                     paste(incomplete_predictors, collapse = ", ")))
        } 
        message("Missing data detected. Using mi() specification.")
      } else {
        # If no formula is provided, continue without modeling missing data
        stop(paste("No formula provided for modeling missing data during modelling."))
      }
    } 
  }
  
  # Add random effects to the formula
  if (!is.null(re)) {
    re_str <- as.character(re)
    valid_pattern <- "^\\(\\s*1\\s*\\|\\s*\\w+(\\s*\\+\\s*\\w+)*\\s*\\)(\\s*\\+\\s*\\(\\s*1\\s*\\|\\s*\\w+(\\s*\\+\\s*\\w+)*\\s*\\))*\\s*$"
    
    if (!grepl(valid_pattern, re_str[2])) {
      stop("Invalid re formula. Structure must follow:\n",
           "~ (1|group1) + (1|group2)")
    }
    
    if (!is.null(formula$forms)) {
      # Update all formulas and store them back
      all_formulas$forms <- lapply(all_formulas$forms, function(f) {
        f$formula <- update(f$formula, paste(". ~ . +", re_str[2]))
        return(f)  
      })
    } else {
      all_formulas$formula <- update(all_formulas$formula, paste(". ~ . +", re_str[2]))
    }
  } else {
    data$group <- as.factor(seq(nrow(data))) 
    
    if (!is.null(formula$forms)) {
      # Update all formulas and store them back
      all_formulas$forms <- lapply(all_formulas$forms, function(f) {
        f$formula <- update(f$formula, ". ~ . + (1 | group)")
        return(f)  
      })
    } else {
      all_formulas$formula <- update(all_formulas$formula, ". ~ . + (1 | group)")
    }
  }
 
  # Check grouping factor sre
  if(!is.null(sre)){
    if (!(sre %in% names(data))) {
      stop(sprintf("Variable '%s' not found in the data.", sre))
    }
  }
  
  # Add spatial effects
  if (!is.null(sre_type)){
    if (is.null(M)){
      stop("Spatial effect specified, but matrix M is NULL. Please provide a spatial correlation matrix.")
    } 
    
    #Add M to data2
    data2 <- list(M = M)
    
    if (sre_type == "car"){
      if(!is.null(car_type)){
        if(car_type %in% c("escar", "esicar", "icar", "bym2")){
          if (!is.null(sre)){
              sre_str <- paste0("car(M, gr =", sre, ", type = '", car_type, "')")
          } else{
              sre_str <- paste0("car(M, type = '", car_type, "')")
          }
        } else{
          stop("'car_type' should be one of 'escar', 'esicar', 'icar', 'bym2'")
        }
      } else {
        if(!is.null(sre)){
          sre_str <- paste0("car(M, gr =", sre, ", type = 'escar')")
        } else{
          sre_str <- paste("car(M, type = 'escar')")
        }
      }
    } else if (sre_type == "sar"){
      if(hb_sampling %in% c("gaussian", "student")){
        if(!is.null(sar_type)){
          if(sar_type %in% c("lag", "error")){
            sre_str <- paste0("sar(M, type = '", sar_type, "')")
          } else{
            stop("'sar_type' should be one of 'lag', 'error'")
          }
        } else{
          sre_str <- paste("sar(M, type = 'lag')")
        }
      } else{
        stop("Currently, only families gaussian and student support SAR structures.")
      }
    } else {
      stop("Invalid spatial effect type. Use 'car' or 'sar'.")
    }
    
    if (!is.null(formula$forms)) {
      # Update all formulas and store them back
      all_formulas$forms <- lapply(all_formulas$forms, function(f) {
        f$formula <- update(f$formula, paste(". ~ . +", sre_str))
        return(f)  
      })
    } else {
      all_formulas$formula <- update(all_formulas$formula, paste(". ~ . +", sre_str))
    }
  }
  
  # Add family to the formula 
  if (!is.null(formula$forms)) {
    all_formulas$forms[[1]]$family <- brms::brmsfamily(hb_sampling, hb_link, link_phi = link_phi)
  } else {
    all_formulas$family <- brms::brmsfamily(hb_sampling, hb_link, link_phi = link_phi)
  }
  
  # Check data availability
  all_vars <- c(response_var, auxiliary_vars)
  all_vars <- setdiff(all_vars, "M")
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in data: ", paste(missing_vars, collapse = ", ")))
  }
  
  # Modelling
  if (multiple) {
    data_multiple <- mice(data, m = m)
    if(hb_sampling=="binomial"){
      post_process <- function(data) {
        data[[response_var[1]]] <- pmin(data[[response_var[1]]], data[[response_var[2]]])
        return(data)
      }
      data_multiple <- complete(data_multiple, action = "all") 
      data_multiple <- lapply(data_multiple, post_process)
    }
    model <- brm_multiple(
      formula = all_formulas,
      data = data_multiple,
      data2 = data2,
      prior = prior,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      control = control,
      sample_prior = sample_prior,
      save_pars = brms::save_pars(all = TRUE),
      ...
    )
  } else {
    model <- brms::brm(
      formula = all_formulas,
      data = data,
      data2 = data2,
      prior = prior,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      control = control,
      sample_prior = sample_prior,
      save_pars = brms::save_pars(all = TRUE),
      ...
    )
  }
  
  return(structure(
    list(model = model, handle_missing = handle_missing, data = data_complete),
    class = "hbmfit"
  ))
}


