#' Check Model Goodness of Fit
#' @title hbmc: Check Model Goodness of Fit
#' @description This function computes and prints the WAIC and LOO model fit diagnostics for a `brmsfit` object. 
#' If a second model is provided, it also computes and prints the WAIC, LOO, and Bayes Factor for model comparison.
#' @name hbmc
#'
#' @param model A `brmsfit` or `hbmfit`object.
#' @param model2 An optional second `brmsfit` or `hbmfit` object object for model comparison.
#' @param ndraws Number of draws to plot in the posterior predictive check.
#' @param moment_match Logical; if `TRUE`, use moment matching for LOO computation.
#' @param plot_types Character vector specifying types of plots to generate (default: "pp_check", "params").
#' 
#' @return A list containing model fit diagnostics:
#'   \item{loo1}{LOO cross-validation diagnostics for `model`.}
#'   \item{waic1}{WAIC value for `model`.}
#'   \item{"pp_check"}{Posterior predictive check plot for assessing model fit for `model`.}
#'   \item{"params"}{Plot of marginal posterior distributions for model parameters for `model`.}
#'   \item{loo2}{(If `model2` is provided) LOO cross-validation diagnostics for `model2`.}
#'   \item{waic2}{(If `model2` is provided) WAIC value for `model2`.}
#'   \item{bf}{(If `model2` is provided) Bayes Factor comparing `model` and `model2` using bridge sampling.}
#'
#' @importFrom brms waic loo pp_check 
#' @importFrom bridgesampling bridge_sampler bf
#' @import bayesplot parameters
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' 
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
#' # Model Goodness of Fit Checks
#' hbmc(model)
#' 
#' }

hbmc <- function(model, 
                 model2 = NULL,
                 ndraws = 100, 
                 moment_match = FALSE,
                 plot_types = c("pp_check", "params")
                 ){
  
  if (inherits(model, "hbmfit")) {
    model <- model$model  
  }
  if (!inherits(model, "brmsfit")) {
    stop("Input model must be a brmsfit or hbmfit object.")
  }
  
  # Cek plot_types
  valid_plot_types <- c("pp_check", "params")
  invalid_plot <- setdiff(plot_types, valid_plot_types)
  if (length(invalid_plot) > 0) {
    stop("Invalid diag_tests: ", paste(invalid_plot, collapse = ", "))
  }
  
  # Extract model formula => respon variable
  model_formula <- model$formula
  if (!is.null(model_formula$forms)) {
    main_formula <- as.formula(model_formula$forms[[1]])
  } else {
    main_formula <- as.formula(model_formula$formula)
  }
  
  model_formula_str <- as.character(main_formula)
  formula_parts <- strsplit(model_formula_str, " ~")[[2]]
  response_var <- (formula_parts[1])
  response_var <- sub("\\| mi\\(\\)", "", response_var)
  response_var <- trimws(response_var)
  
  results <- list()
  
  # Posterior Predictive Check Plot
  results$pp_check <- brms::pp_check(model, ndraws = ndraws, resp=response_var)
  
  # Parameter Distribution Plot
  results$params <- plot(model, ask = FALSE) 
  
  # Loo
  results$loo1_values <- brms::loo(model, moment_match = moment_match, newdata = na.omit(model$data))

  #WAIC
  results$waic1_values <- brms::waic(model, newdata = na.omit(model$data))

  # Model Comparison
  if (!is.null(model2)) {
    if (inherits(model2, "hbmfit")) {
      model2 <- model2$model  
    }
    
    if (!inherits(model2, "brmsfit")) {
      stop("model2 must be a brmsfit object.")
    }
    
    results$waic2_values <- waic(model2)
    results$loo2_values <- loo(model2, moment_match = moment_match, newdata = na.omit(model2$data))
    
    # Hitung Bridge Sampling
    bridge1 <- bridgesampling::bridge_sampler(model)
    bridge2 <- bridgesampling::bridge_sampler(model2)
    
    # Hitung Bayes Factor
    results$bf <- bridgesampling::bf(bridge1, bridge2)
  }

  return(structure(
    list(loo1 = results$loo1_values, waic1 = results$waic1_values, pp_check = results$pp_check, params =results$params, loo2 = results$loo2_values, waic2 = results$waic2_values, bf = results$bf, model2 = model2),
    class = "hbmc"
  ))
}
