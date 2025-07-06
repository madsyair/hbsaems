#' Hierarchical Bayesian Prior Predictive Checking
#' @title hbpc : Hierarchical Bayesian Prior Predictive Checking
#' 
#' @description This function facilitates prior predictive checking for Bayesian models.
#' It is primarily used to visualize and summarize the implications of
#' the chosen priors by examining distributions generated from the priors alone.
#' @name hbpc
#'
#' @param model A `brmsfit` object, which is the fitted Bayesian model from `brms`.
#' Ideally, for prior predictive checks, this model should be fitted
#' with the argument `sample_prior = "only"`.
#' @param data The data that was used (or would be used) to fit the model.
#' Required by `pp_check` for comparing with observed data, though for
#' `sample_prior = "only"`, the focus is on `y_rep`.
#' @param response_var A character string specifying the name of the response variable in the data.
#' This is needed for `pp_check` to correctly identify the observed data `y`.
#' @param ndraws_ppc An integer specifying the number of draws to use for the
#' posterior predictive check (pp_check) plot. Default is 50.
#'
#' @return A list of class `hbpc_results` containing:
#' \item{prior_summary}{Summary of the priors used in the model (obtained from `brms::prior_summary(model)`).}
#' \item{prior_predictive_plot}{A `ggplot` object from `brms::pp_check` comparing the observed data (if available and appropriate) with draws from the prior predictive distribution.}
#' \item{prior_draws_summary}{(Optional) A summary of the parameter draws from the prior-only model, which can indicate the range and central tendency implied by the priors.}
#'
#' @import brms
#' @import ggplot2
#' @importFrom bayesplot ppc_dens_overlay
#' @importFrom stats rnorm
#' @export
#'
#' @author Achmad Syahrul Choir and Saniyyah Sri Nurhayati
#'
#' @references 
#' Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
#' Gabry,J., Simpson,D., Vehtari, A., Betancourt, M., Gelman, A., Visualization in Bayesian Workflow, Journal of the 
#' Royal Statistical Society Series A: Statistics in Society, Volume 182, Issue 2, February 2019, Pages 389–402
#'

#' @examples
#' \donttest{
#' # This is a conceptual example. Actual usage requires a brms model
#' # fitted with sample_prior = "only".
#'
#' # library(brms)
#' # # Assume 'data_df' is your data frame with 'response_variable' and 'predictor_variable'
#' # data_df <- data.frame(
#' # response_variable = rnorm(100),
#' # predictor_variable = rnorm(100)
#' # )
#' #
#' # # Define some priors
#' # example_priors <- c(prior(normal(0, 10), class = "b"),
#' # prior(cauchy(0, 1), class = "sigma"))
#' #
#' # # Fit a model with sample_prior = "only"
#' # # Note: For actual prior predictive checks, you'd use your actual model 
#' # # formula and data structure.
#' # # The data itself isn't used for fitting parameters when sample_prior = "only",
#' # # but its structure (like number of observations) can influence y_rep.
#' # fit_prior_only <- try(brm(
#' # bf(response_variable ~ predictor_variable),
#' # data = data_df,
#' # prior = example_priors,
#' # sample_prior = "only", # Crucial for prior predictive checks
#' # chains = 1, iter = 1000, warmup = 200, refresh = 0,
#' # control = list(adapt_delta = 0.8)
#' # ))
#' #
#' # if (!inherits(fit_prior_only, "try-error")) {
#' # # Perform prior predictive checking
#' # prior_check_output <- hbpc(
#' # model = fit_prior_only,
#' # data = data_df,
#' # response_var = "response_variable", # Specify the response variable name
#' # ndraws_ppc = 50
#' # )
#' #
#' # # Print the summary of priors
#' # print(prior_check_output$prior_summary)
#' #
#' # # Display the prior predictive plot
#' # # print(prior_check_output$prior_predictive_plot)
#' #
#' # # Print summary of parameter draws from prior
#' # # print(prior_check_output$prior_draws_summary)
#' # }
#' }
hbpc <- function(model, 
                 data, 
                 response_var, 
                 ndraws_ppc = 50) {
  if (inherits(model, "hbmfit")) {
    model <- model$model  
  }
  
  if (!inherits(model, "brmsfit")) {
    stop("Input model must be a brmsfit or hbmfit object.")
  }
  
  if (model$algorithm != "sampling" && model$sample_prior != "only") {
    warning("For meaningful prior predictive checks, the model should ideally be fitted with `sample_prior = \"only\"`.")
  }
  if (missing(data)) {
    data <- model$data # Try to get data from model if not provided
    # stop("The 'data' argument is required for pp_check.")
  }
  if (missing(response_var)) {
    # Attempt to guess response variable from model formula
    resp_from_formula <- model$formula$resp
    if (is.null(resp_from_formula) || length(resp_from_formula) == 0) {
      stop("The 'response_var' argument is required and could not be automatically determined from the model formula.")
    }
    # Handle cases like `y | trials(N)` or `y | mi()`
    response_var <- trimws(strsplit(resp_from_formula, "\\|")[[1]][1])
    
    if (!response_var %in% names(data)) {
      stop(paste("Automatically determined response variable '", response_var,
                 "' not found in the provided data. Please specify 'response_var' explicitly.", sep=""))
    }
    message(paste("Using '", response_var, "' as the response variable for pp_check.", sep=""))
  }
  if (!response_var %in% names(data)) {
    stop(paste("Specified response variable '", response_var, "' not found in the data.", sep=""))
  }
  
  
  results <- list()
  
  # 1. Prior Summary
  results$prior_summary <- tryCatch({
    brms::prior_summary(model)
  }, error = function(e) {
    paste("Could not retrieve prior summary:", e$message)
  })
  
  # 2. Prior Predictive Plot using pp_check
  # pp_check requires the response variable `y` from the data for comparison,
  # and `yrep` (predictions from the prior predictive distribution).
  results$prior_predictive_plot <- tryCatch({
    # Extract the observed response variable
    y_obs <- data[[response_var]]
    
    # Generate predicted draws from the prior predictive distribution
    yrep_draws <- brms::posterior_predict(model, ndraws = ndraws_ppc)
    
    # Use a dummy y (first row of yrep) if observed y is all NA
    dummy_y <- yrep_draws[1, ]
    
    # Combine observed and predicted values to assess value range
    combined_vals <- if (all(is.na(y_obs))) dummy_y else c(y_obs, as.vector(yrep_draws))
    
    # Decide whether to apply log scale based on how wide the value range is
    # (e.g., if max/min > 100, then values are very dispersed)
    use_log_scale <- (max(combined_vals, na.rm = TRUE) /
                        min(combined_vals[combined_vals > 0], na.rm = TRUE)) > 100
    
    # Generate plot depending on whether observed y is available
    p <- if (all(is.na(y_obs))) {
      # If y is all NA, overlay dummy y with predicted draws only
      bayesplot::ppc_dens_overlay(y = dummy_y, yrep = yrep_draws, alpha = 0.4, size = 0.7 ) +
        ggplot2::ggtitle(paste("Prior Predictive Check (No Observed Y):", response_var))
    } else {
      # If y is available, use brms pp_check with overlay density
      brms::pp_check(model, resp = response_var, ndraws = ndraws_ppc, type = "dens_overlay") + 
        ggplot2::ggtitle(paste("Prior Predictive Check:", response_var))
    }
    
    # Add log10 scale to x-axis if value range is too large
    if (use_log_scale) {
      p <- p + ggplot2::scale_x_log10() + ggplot2::labs(subtitle = "Log10 scale applied due to wide value range")
    }
    
    # Return final plot
    p
    
  }, error = function(e) {
    # In case of error, display a blank plot with an error message
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error generating prior predictive plot:", e$message), size = 3) +
      ggplot2::theme_void() +
      ggplot2::ggtitle("Prior Predictive Plot Error")
  })
  
  
  # 3. Summary of parameter draws from the prior-only model
  results$prior_draws_summary <- tryCatch({
    summary(model) # This will summarize the "posterior" draws which are actually prior draws
  }, error = function(e) {
    paste("Could not retrieve summary of prior draws:", e$message)
  })
  
  
  class(results) <- "hbpc_results"
  return(results)
}
