#' Check Model Goodness of Fit and Prior Sensitivity
#' 
#' @title hbmc: Check Model Goodness of Fit and Prior Sensitivity
#' 
#' @description This function computes model fit diagnostics (WAIC, LOO) and performs
#'              posterior predictive checks for the primary model. If the 'model' argument is a list
#'              containing two or more brmsfit/hbmfit objects, it performs model comparison
#'              using selected metrics (LOO, WAIC, Bayes Factor) between the first model and
#'              each subsequent model in the list. Prior sensitivity analysis can be run for
#'              each model if specified. LOO calculation includes checks for problematic Pareto k
#'              values. If k > 0.7 and moment_match was FALSE, a warning is issued. If k > 0.7 and
#'              moment_match was TRUE, reloo is attempted. Additional arguments for moment matching
#'              and reloo can be passed via `moment_match_args` and `reloo_args`.
#'              
#' @name hbmc
#'
#' @param model A single `brmsfit`/`hbmfit` object, or a list of `brmsfit`/`hbmfit` objects.
#' @param ndraws_ppc Number of draws to plot in the posterior predictive check (default: 100).
#' @param moment_match Logical; if `TRUE`, use moment matching for LOO computation directly.
#'                     If `FALSE` (default), and problematic Pareto k values are detected, a warning
#'                     will suggest re-running with `moment_match = TRUE`.
#' @param moment_match_args A list of additional arguments to pass to `brms::loo` when `moment_match = TRUE`.
#'                          Default is `list()`.
#' @param reloo_args A list of additional arguments to pass to `brms::loo` when `reloo = TRUE` is attempted.
#'                   Default is `list()`. Note that `k_threshold` is already set to 0.7 internally when reloo is attempted.
#' @param plot_types Character vector specifying types of plots to generate for the primary model
#'                   (default: c("pp_check", "params")).
#' @param comparison_metrics A character vector specifying which metrics to compute for model comparison
#'                           when multiple models are provided. Options are "loo", "waic", "bf" (Bayes Factor).
#'                           Default is `c("loo", "waic", "bf")` to compute all. If NULL or empty,
#'                           no comparison metrics will be computed.
#' @param run_prior_sensitivity Logical; if `TRUE`, attempt to run prior sensitivity analysis for each model (default: `FALSE`).
#' @param sensitivity_vars A character vector of parameter names for which to run sensitivity analysis.
#'                         Required if `run_prior_sensitivity` is `TRUE`.
#'
#' @return An object of class `hbmc_results`, which is a list containing:
#'   \item{primary_model_diagnostics}{A list containing diagnostics for the primary model:
#'     \itemize{
#'       \item `loo`: LOO diagnostics. May include notes about Pareto k values or reloo application.
#'       \item `waic`: WAIC value.
#'       \item `pp_check_plot`: Posterior predictive check plot.
#'       \item `params_plot`: Plot of marginal posterior distributions of parameters.
#'     }
#'   }
#'   \item{comparison_results}{A list where each element contains specified comparison metrics
#'                             (LOO, WAIC, Bayes Factor) for the primary model vs. another model.
#'                             LOO results may include notes about Pareto k or reloo.
#'                             NULL if fewer than two models or `comparison_metrics` is empty.
#'                             Each element is named e.g., "model1_vs_model2".}
##'   \item{prior_sensitivity_results}{A list where each element (named by model name) contains results
#'                                    from prior sensitivity analysis if \code{run_prior_sensitivity = TRUE}.
#'                                    \code{NULL} otherwise.
#'                                    Each element in the list is itself a list with the following components:
#'                                    \describe{
#'                                      \item{result}{A data frame or list summarizing the sensitivity analysis results,
#'                                                    such as changes in posterior estimates or model fit metrics (e.g., WAIC, LOO).}
#'                                      \item{plot}{A plot object (usually a ggplot) visualizing the effect of different priors
#'                                                  on model estimates or diagnostics. This plot can be displayed using \code{print()}.}}
#'                                    If no sensitivity was conducted or available, this field may contain a character
#'                                    message or be \code{NULL}.}
#'   \item{models_compared_count}{Integer, the number of models involved in comparison.}
#'
#' @import ggplot2
#' @import priorsense
#' @importFrom posterior variables
#' @importFrom bridgesampling bridge_sampler
#' @importFrom stats formula na.omit
#' @export
#'
#' @author Achmad Syahrul Choir, Saniyyah Sri Nurhayati, and Arsyka Laila Oktalia Siregar
#'
#' @references 
#' Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
#' Kallioinen, N., Paananen, T., Bürkner, PC. et al. Detecting and diagnosing prior and likelihood sensitivity with power-scaling. Stat
#'               Comput 34, 57 (2024).
#' Gabry,J., Simpson,D.,  Vehtari, A., Betancourt, M., Gelman, A., Visualization in Bayesian Workflow, Journal of the 
#'            Royal Statistical Society Series A: Statistics in Society, Volume 182, Issue 2, February 2019, Pages 389–402
#'
#' @examples
#' \donttest{
#' # Load the hbsaems package if not already loaded (for hbm function)
#' # library(hbsaems) # Assuming hbm is part of this package
#' # library(brms)    # For bf()
#'
#' # Prepare a sample dataset
#' set.seed(123)
#' sim_data <- data.frame(
#'   y = rnorm(100, 50, 10),
#'   x1 = rnorm(100, 5, 2),
#'   x2 = rnorm(100, 10, 3),
#'   grp = factor(sample(1:10, 100, replace = TRUE))
#' )
#'
#' # Define formulas and priors
#' f1 <- bf(y ~ x1 + (1 | grp))
#' common_priors <- c(prior(normal(0,10), class="b"), prior(cauchy(0,1), class="sd"))
#'
#' # Fit models using hbm (minimal iterations for example speed)
#' fit_hbm1 <- try(hbm(f1, data=sim_data, prior=common_priors, family=gaussian(),
#'                     chains=1, iter=1000, warmup=500, cores=1, refresh=0, save_model=NULL))
#'
#' if (!inherits(fit_hbm1, "try-error")) {
#'   # Example: Explicitly request moment_match = TRUE with specific moment_match_args
#'   # (e.g., if you want to pass 'k_threshold' to moment_match itself, though usually
#'   # k_threshold is for reloo)
#'   # For reloo, you might pass other args like 'psis_object' if precomputed.
#'   checks_custom_args <- try(hbmc(
#'       model = fit_hbm1,
#'       moment_match = TRUE,
#'       moment_match_args = list(k_threshold = 0.6), # Example arg for moment_match
#'       reloo_args = list(check = FALSE), # Example arg for reloo
#'       comparison_metrics = "loo"
#'   ))
#'    # if (!inherits(checks_custom_args, "try-error")) {
#'    #   print(summary(checks_custom_args))
#'    # }
#' }
#' }
hbmc <- function(model,
                 ndraws_ppc = 100,
                 moment_match = FALSE, 
                 moment_match_args = list(), # New argument
                 reloo_args = list(),        # New argument
                 plot_types = c("pp_check", "params"),
                 comparison_metrics = c("loo", "waic", "bf"),
                 run_prior_sensitivity = FALSE,
                 sensitivity_vars = NULL
){
  
  results <- list()
  model1_brms <- NULL 
  original_model_objects <- list() 
  all_brms_models <- list() 
  
  # --- Helper function for LOO calculation with Pareto k check and reloo ---
  calculate_loo_with_check <- function(model, model_name = "model",
                                   moment_match = FALSE,
                                   moment_match_args = list(),
                                   reloo_args = list(),
                                   safe_data_subset = NULL) {
    
    result <- list(
      estimates = NULL,
      pareto_k_summary = NULL,
      high_k_count = 0,
      warnings = character(0),
      reloo_attempted = FALSE,
      reloo_successful = FALSE,
      error = NULL
    )
    
    tryCatch({
      withCallingHandlers({
        
        # ========== CASE 1: moment_match = FALSE ==========
        if (!moment_match) {
          loo_base <- brms::loo(model, moment_match = FALSE, reloo = FALSE, newdata = safe_data_subset)
          result$estimates <- loo_base$estimates
          
          if (!is.null(loo_base$diagnostics$pareto_k)) {
            pk <- loo_base$diagnostics$pareto_k
            result$pareto_k_summary <- summary(pk)
            result$high_k_count <- sum(pk > 0.7)
            
            if (result$high_k_count > 0) {
              msg <- paste0("Model '", model_name, "': ", result$high_k_count,
                            " high Pareto k values (> 0.7). Consider setting moment_match = TRUE.")
              result$warnings <- c(result$warnings, msg)
              message("⚠️ ", msg)
            }
          }
          
        } else {
          # ========== CASE 2: moment_match = TRUE ==========
          
          reloo_requested_explicitly <- !is.null(reloo_args$reloo) && isTRUE(reloo_args$reloo)
          
          if (reloo_requested_explicitly) {
            # Run LOO directly with reloo = TRUE
            result$reloo_attempted <- TRUE
            
            loo_args <- c(
              list(x = model,
                   moment_match = TRUE,
                   reloo = TRUE,
                   newdata = safe_data_subset,
                   k_threshold = 0.7,
                   cores = 1),
              moment_match_args[!names(moment_match_args) %in% c("x", "moment_match", "reloo", "newdata", "cores")],
              reloo_args[!names(reloo_args) %in% c("x", "moment_match", "reloo", "newdata", "cores", "k_threshold")]
            )
            
            loo_obj <- do.call(brms::loo, loo_args)
            result$estimates <- loo_obj$estimates
            result$reloo_successful <- TRUE
            
          } else {
            # Run LOO with moment_match = TRUE, reloo = FALSE
            loo_base <- brms::loo(model, moment_match = TRUE, reloo = FALSE, newdata = safe_data_subset)
            result$estimates <- loo_base$estimates
            
            if (!is.null(loo_base$diagnostics$pareto_k)) {
              pk <- loo_base$diagnostics$pareto_k
              result$pareto_k_summary <- summary(pk)
              result$high_k_count <- sum(pk > 0.7)
              
              if (result$high_k_count > 0) {
                # Inform and run reloo automatically
                msg <- paste0("Model '", model_name, "': ", result$high_k_count,
                              " high Pareto k values remain after moment_match = TRUE. Re-running with reloo = TRUE.")
                result$warnings <- c(result$warnings, msg)
                message("⚠️ ", msg)
                
                result$reloo_attempted <- TRUE
                
                loo_reloo <- brms::loo(model,
                                       moment_match = TRUE,
                                       reloo = TRUE,
                                       newdata = safe_data_subset,
                                       k_threshold = 0.7,
                                       cores = 1)
                result$estimates <- loo_reloo$estimates
                result$reloo_successful <- TRUE
                
                if (!is.null(loo_reloo$diagnostics$pareto_k)) {
                  pk2 <- loo_reloo$diagnostics$pareto_k
                  result$pareto_k_summary <- summary(pk2)
                  result$high_k_count <- sum(pk2 > 0.7)
                  
                  if (result$high_k_count > 0) {
                    msg2 <- paste0("Model '", model_name, "': ", result$high_k_count,
                                   " high Pareto k values remain even after reloo = TRUE.")
                    result$warnings <- c(result$warnings, msg2)
                    message("⚠️ ", msg2)
                  } else {
                    message("✅ All high Pareto k values resolved with reloo.")
                  }
                }
              }
            }
          }
        }
        
      }, warning = function(w) {
        result$warnings <- c(result$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    }, error = function(e) {
      err_msg <- paste0("❌ LOO failed for model '", model_name, "': ", e$message)
      result$error <- err_msg
      result$warnings <- c(result$warnings, err_msg)
      message(err_msg)
    })
    
    return(result)
  }
  
  # --- Determine primary model and list of all brms models ---
  is_model_list <- is.list(model) && !inherits(model, "brmsfit") && !inherits(model, "hbmfit")
  input_models_processed <- list()
  
  if (is_model_list) {
    if (length(model) == 0) {
      stop("If 'model' is a list, it must contain at least one brmsfit/hbmfit object.")
    }
    input_models_processed <- model
  } else {
    input_models_processed[[1]] <- model 
  }
  
  model_input_names <- names(input_models_processed)
  if (is.null(model_input_names)) {
    model_input_names <- paste0("model", seq_along(input_models_processed))
  } else {
    unnamed_idx <- model_input_names == ""
    model_input_names[unnamed_idx] <- paste0("model", seq_along(input_models_processed))[unnamed_idx]
  }
  names(input_models_processed) <- model_input_names
  
  for (i in seq_along(input_models_processed)) {
    current_model_input <- input_models_processed[[i]]
    model_name <- model_input_names[i]
    
    if (inherits(current_model_input, "hbmfit")) {
      all_brms_models[[model_name]] <- current_model_input$model
      original_model_objects[[model_name]] <- current_model_input
    } else if (inherits(current_model_input, "brmsfit")) {
      all_brms_models[[model_name]] <- current_model_input
      original_model_objects[[model_name]] <- current_model_input
    } else {
      stop(paste0("Element '", model_name, "' in 'model' must be a brmsfit or hbmfit object."))
    }
  }
  
  if (length(all_brms_models) == 0) {
    stop("No valid brmsfit models found.")
  }
  model1_brms <- all_brms_models[[1]] 
  model1_name <- names(all_brms_models)[1]
  results$all_brms_models <- all_brms_models
  
  # Validate plot_types
  valid_plot_types <- c("pp_check", "params")
  invalid_plot <- setdiff(plot_types, valid_plot_types)
  if (length(invalid_plot) > 0) {
    warning("Invalid plot_types specified: ", paste(invalid_plot, collapse = ", "),
            ". Only 'pp_check' and 'params' are currently supported.")
    plot_types <- intersect(plot_types, valid_plot_types)
  }
  
  # Validate comparison_metrics
  if (is.null(comparison_metrics)) comparison_metrics <- character(0) 
  valid_comp_metrics <- c("loo", "waic", "bf")
  invalid_comp_metrics <- setdiff(comparison_metrics, valid_comp_metrics)
  if (length(invalid_comp_metrics) > 0) {
    warning("Invalid comparison_metrics specified: ", paste(invalid_comp_metrics, collapse=", "),
            ". Valid options are 'loo', 'waic', 'bf'. Ignoring invalid ones.")
    comparison_metrics <- intersect(comparison_metrics, valid_comp_metrics)
  }
  
  # --- Diagnostics for Primary Model (model1_brms) ---
  primary_model_diagnostics <- list()
  resp_var_name_model1 <- model1_brms$formula$resp
  if (length(resp_var_name_model1) > 1 && "trials" %in% names(model1_brms$formula)) {
    resp_var_name_model1 <- resp_var_name_model1[1]
  }
  resp_var_name_model1 <- trimws(strsplit(resp_var_name_model1, "\\|")[[1]][1])
  
  if ("pp_check" %in% plot_types) {
    primary_model_diagnostics$pp_check_plot <- tryCatch({
      brms::pp_check(model1_brms, ndraws = ndraws_ppc, resp = resp_var_name_model1)
    }, error = function(e) {
      warning(paste("Could not generate pp_check plot for primary model (", model1_name ,"):", e$message))
      ggplot() + annotate("text", x=0.5,y=0.5,label=paste("PPC plot error (primary model):\n",e$message)) + theme_void()
    })
  }
  
  if ("params" %in% plot_types) {
    primary_model_diagnostics$params_plot <- tryCatch({
      plot(model1_brms, ask = FALSE)
    }, error = function(e) {
      warning(paste("Could not generate parameter plots for primary model (", model1_name ,"):", e$message))
      list(error_plot = ggplot() + annotate("text",x=0.5,y=0.5,label=paste("Param plot error (primary model):\n",e$message)) + theme_void())
    })
  }
  
  original_model1_obj_for_data <- original_model_objects[[1]]
  data_for_loo_waic1 <- if(!is.null(model1_brms$data) && nrow(model1_brms$data) > 0) model1_brms$data else NULL
  if(is.null(data_for_loo_waic1) && inherits(original_model1_obj_for_data, "hbmfit") && !is.null(original_model1_obj_for_data$data)){
    data_for_loo_waic1 <- original_model1_obj_for_data$data
  }
  
  vars_in_formula1 <- tryCatch(all.vars(formula(model1_brms)$formula), error = function(e) NULL)
  safe_data_subset1 <- if(!is.null(data_for_loo_waic1) && !is.null(vars_in_formula1)) {
    vars_present <- intersect(vars_in_formula1, names(data_for_loo_waic1))
    if (length(vars_present) < length(vars_in_formula1) && length(vars_present) > 0) {
      # warning(paste0("Not all variables from primary model's (", model1_name, ") formula found in data for LOO/WAIC. Using subset."))
    } else if (length(vars_present) == 0 && length(vars_in_formula1) > 0) {
      warning(paste0("No variables from primary model's (", model1_name, ") formula found in data for LOO/WAIC. LOO/WAIC might fail."))
      NULL
    }
    if (length(vars_present) > 0) na.omit(data_for_loo_waic1[, vars_present, drop=FALSE]) else NULL
  } else { NULL }
  
  if (("loo" %in% comparison_metrics)) {
    primary_model_diagnostics$loo <- calculate_loo_with_check(model1_brms, model1_name, moment_match, moment_match_args, reloo_args, safe_data_subset1)
  } else { primary_model_diagnostics$loo <- NULL }
  
  if (("waic" %in% comparison_metrics)) {
    primary_model_diagnostics$waic <- tryCatch({
      brms::waic(model1_brms, newdata = safe_data_subset1)
    }, error = function(e) {
      warning(paste("WAIC computation for primary model (", model1_name ,") failed:", e$message))
      list(estimates=data.frame(Estimate=NA,SE=NA,row.names="elpd_waic"),error_message=e$message)
    })
  } else { primary_model_diagnostics$waic <- NULL }
  results$primary_model_diagnostics <- primary_model_diagnostics
  
  # --- Model Comparison (if more than one model and metrics selected) ---
  results$comparison_results <- NULL
  results$models_compared_count <- 0
  
  if (length(all_brms_models) > 1 && length(comparison_metrics) > 0) {
    results$models_compared_count <- length(all_brms_models)
    comparison_list <- list()
    
    bridge1_samp <- NULL
    if ("bf" %in% comparison_metrics) {
      bridge1_samp <- try(bridgesampling::bridge_sampler(model1_brms, silent = TRUE), silent = TRUE)
      if(inherits(bridge1_samp, "try-error")){
        warning("Bridge sampling for the primary model (", model1_name, ") failed. Bayes Factor comparisons will not be available. Error: ", as.character(bridge1_samp))
      }
    }
    
    for (i in 2:length(all_brms_models)) {
      model_i_name <- names(all_brms_models)[i]
      model_i_brms <- all_brms_models[[model_i_name]]
      original_model_i_obj_for_data <- original_model_objects[[model_i_name]] 
      
      current_comparison <- list()
      
      data_for_loo_waic_i <- if(!is.null(model_i_brms$data) && nrow(model_i_brms$data) > 0) model_i_brms$data else NULL
      if(is.null(data_for_loo_waic_i) && inherits(original_model_i_obj_for_data, "hbmfit") && !is.null(original_model_i_obj_for_data$data)){
        data_for_loo_waic_i <- original_model_i_obj_for_data$data
      }
      
      vars_in_formula_i <- tryCatch(all.vars(formula(model_i_brms)$formula), error = function(e) NULL)
      safe_data_subset_i <- if(!is.null(data_for_loo_waic_i) && !is.null(vars_in_formula_i)) {
        vars_present_i <- intersect(vars_in_formula_i, names(data_for_loo_waic_i))
        if (length(vars_present_i) < length(vars_in_formula_i) && length(vars_present_i) > 0) {
          # warning(paste0("Not all variables from ",model_i_name,"'s formula found in data for LOO/WAIC. Using subset."))
        } else if (length(vars_present_i) == 0 && length(vars_in_formula_i) > 0) {
          warning(paste0("No variables from ",model_i_name,"'s formula found in data for LOO/WAIC. LOO/WAIC might fail."))
          NULL
        }
        if(length(vars_present_i) > 0) na.omit(data_for_loo_waic_i[, vars_present_i, drop=FALSE]) else NULL
      } else { NULL }
      
      if ("loo" %in% comparison_metrics) {
        current_comparison$loo_model_i <- calculate_loo_with_check(model_i_brms, model_i_name, moment_match, moment_match_args, reloo_args, safe_data_subset_i)
      }
      
      if ("waic" %in% comparison_metrics) {
        current_comparison$waic_model_i <- tryCatch({
          brms::waic(model_i_brms, newdata = safe_data_subset_i)
        }, error = function(e) {
          warning(paste("WAIC computation for", model_i_name, "failed:", e$message))
          list(estimates=data.frame(Estimate=NA,SE=NA,row.names="elpd_waic"),error_message=e$message)
        })
      }
      
      if ("bf" %in% comparison_metrics) {
        if (!is.null(bridge1_samp) && !inherits(bridge1_samp, "try-error")) {
          bridge_i_samp <- try(bridgesampling::bridge_sampler(model_i_brms, silent = TRUE), silent = TRUE)
          if (!inherits(bridge_i_samp, "try-error")) {
            current_comparison$bf_comparison <- tryCatch({
              bridgesampling::bf(bridge1_samp, bridge_i_samp)
            }, error = function(e) {
              warning(paste("Bayes Factor computation between", model1_name, "and", model_i_name, "failed:", e$message))
              list(bf=NA,error_message=e$message)
            })
          } else {
            warning(paste("Bridge sampling for", model_i_name, "failed. Bayes Factor comparison with", model1_name, "cannot be computed. Error:", as.character(bridge_i_samp)))
            current_comparison$bf_comparison <- list(bf=NA,error_message=paste("Bridge sampling failed for", model_i_name))
          }
        } else {
          current_comparison$bf_comparison <- list(bf=NA,error_message=paste("Bridge sampling for primary model (", model1_name, ") failed, so no BF comparison possible."))
        }
      }
      
      if(length(current_comparison) > 0) { 
        comparison_list[[paste0(model1_name, "_vs_", model_i_name)]] <- current_comparison
      }
    }
    
    if(length(comparison_list) > 0) results$comparison_results <- comparison_list
  }
  
  
  # --- Prior Sensitivity Analysis (performed for each model if run_prior_sensitivity is TRUE) ---
  results$prior_sensitivity_results <- NULL
  if (run_prior_sensitivity) {
    if (is.null(sensitivity_vars) || length(sensitivity_vars) == 0) {
      warning("`run_prior_sensitivity` is TRUE, but `sensitivity_vars` is not specified. Skipping sensitivity analysis for all models.")
      results$prior_sensitivity_results <- list(message = "Skipped: sensitivity_vars not specified.")
    } else {
      sensitivity_results_list <- list()
      for (model_idx in seq_along(all_brms_models)) {
        current_brms_model <- all_brms_models[[model_idx]]
        current_model_name <- names(all_brms_models)[model_idx]
        current_original_input <- original_model_objects[[current_model_name]] 
        
        model_data_sens <- if(!is.null(current_brms_model$data) && nrow(current_brms_model$data) > 0) current_brms_model$data else NULL
        if(is.null(model_data_sens) && inherits(current_original_input, "hbmfit") && !is.null(current_original_input$data)){
          model_data_sens <- current_original_input$data
        }
        model_formula_sens <- formula(current_brms_model) 
        model_family_sens <- current_brms_model$family 
        model_priors_sens <- current_brms_model$prior 
        
        if (is.null(model_data_sens) || is.null(model_formula_sens) || is.null(model_family_sens) || is.null(model_priors_sens)) {
          warning(paste0("Could not extract all required components (data, formula, family, prior) for model '",
                         current_model_name, "'. Skipping prior sensitivity analysis for this model."))
          sensitivity_results_list[[current_model_name]] <- "Skipped: Missing components from model object."
          next
        }
        
        sensitivity_status <- tryCatch({
          # draws <- posterior::as_draws_array(current_brms_model) # Not directly used in placeholder
          model_params <- posterior::variables(current_brms_model)
          valid_sensitivity_vars <- sensitivity_vars[sensitivity_vars %in% model_params]
          
          if(length(valid_sensitivity_vars) > 0) {
            # Run priorsense sensitivity analysis and generate plot
            sens_result <- priorsense::powerscale_sensitivity(
              current_brms_model,
              variable = valid_sensitivity_vars
            )
            
            sens_plot <- priorsense::powerscale_plot_dens(
              current_brms_model,
              variable = valid_sensitivity_vars
            )
            
            list(
              result = sens_result,
              plot = sens_plot
            )
          } else {
            paste0("None of the specified sensitivity_vars found in model '", current_model_name, "' parameters.")
          }
        }, error = function(e) {
          warning(paste("Error during prior sensitivity analysis for model '", current_model_name, "':", e$message))
          paste("Error during prior sensitivity analysis for model '", current_model_name, "':", e$message)
        })
        sensitivity_results_list[[current_model_name]] <- sensitivity_status
      }
      if(length(sensitivity_results_list) > 0) results$prior_sensitivity_results <- sensitivity_results_list
    }
  }
  
  class(results) <- "hbmc_results"
  return(results)
}
