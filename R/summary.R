#' Create a summary of a fitted model represented by a hbmfit object
#'
#' @param object A `hbmfit` object
#' @param ... Other potential arguments
#'
#' @return Summary of the model
#' 
#' @export 
#' 
summary.hbmfit <- function(object, ...) {
  if (!inherits(object, "hbmfit")) {
    stop("Input must be of class 'hbmfit'.")
  }
  
  # Get the brmsfit object
  brms_model <- object$model
  if (!inherits(brms_model, "brmsfit")) {
    stop("The 'model' component of the hbmfit object is not a valid brmsfit object.")
  }
  
  cat("\n======= Hierarchical Bayesian Model Summary (from hbmfit) =======\n")
  
  # Print the standard brms summary (which includes fixed effects, family, etc.)
  cat("\n--- Full brms Model Summary ---\n")
  print(brms_model) # This calls summary.brmsfit then print.brmssummary
  
  # Explicitly extract and print Group-Level Effects (Random Effects)
  # The summary() call on a brmsfit object returns a brmssummary object
  brms_summary_obj <- summary(brms_model) 
  
  cat("\n\n===== Detailed Group-Level Effects (Random Effects) Summary =====\n")
  if (!is.null(brms_summary_obj$random) && length(brms_summary_obj$random) > 0) {
    # The 'random' component is a list of data frames, one for each random effect term/group
    for (group_name in names(brms_summary_obj$random)) {
      cat("\n--- Grouping Factor:", group_name, "---\n")
      # Each element in brms_summary_obj$random is typically a matrix or data.frame
      # containing estimates, CIs, Rhat, ESS for the random effects of that group.
      # Add explanation about the upcoming data frame
      group_df <- brms_summary_obj$random[[group_name]]
      cat(paste0(
        "The table below (in the next console output) contains group-level estimates\n",
        "for the factor '", group_name, "', with dimensions ", 
        nrow(group_df), " rows x ", ncol(group_df), " columns.\n\n",
        "Columns: ", paste(colnames(group_df), collapse = ", "), "."
      ))
      print(group_df)
      
    }
  } else {
    cat("\nNo group-level (random) effects found in this model.\n")
  }
  
  cat("\n\n=========== Missing Data Handling (specified in hbm) ============\n")
  if (is.null(object$handle_missing)) {
    cat("No specific missing data handling method was applied through the hbm function.\n")
    cat("(brms internal defaults for NA handling in predictors/response may have applied if NAs were present and not pre-processed).\n")
  } else {
    cat("Missing data handling method specified in hbm:", object$handle_missing)
    # Check if the model data is a list of imputed datasets (from brm_multiple)
    if (inherits(brms_model$data, "list") && !is.data.frame(brms_model$data) && object$handle_missing == "multiple") {
      cat(" (using", length(brms_model$data), "imputed datasets)\n")
    } else {
      cat("\n")
    }
  }
  
  invisible(object)
}


#' Print a summary for a fitted model represented by a hbmfit object
#'
#' @param x A `hbmfit` object (changed from object to x for consistency with generic)
#' @param ... Other potential arguments
#'
#' @return Summary of the model
#' @export
#'
print.hbmfit <- function(x, ...) {
  summary(x, ...) # Call the S3 summary method
}

#' Create a summary of a prediction result
#'
#' @param object A `hbsae` object
#' @param ... Other potential arguments
#'
#' @return Summary of a prediction result
#' 
#' @export
#' 
summary.hbsae_results <- function(object, ...) {
  if (!inherits(object, "hbsae_results")) {
    stop("Input must be of class 'hbsae_results'.")
  }
  
  cat("\n=============== Summary of hbsae Prediction ===============\n")
  
  cat("\n>> Model Performance Metrics (based on provided data for prediction)\n")
  if (!is.null(object$rse_model)) {
    cat("Mean RSE (%) of Predictions:", round(object$rse_model, 2), "\n")
  }
  
  if (!is.null(object$mse_model)) {
    cat("Mean MSE of Predictions:", round(object$mse_model, 2), "\n")
  }
  
  cat("\n=================== Prediction Table =====================\n")
  cat("Access the full data frame with $result_table\n")
  if (!is.null(object$result_table) && nrow(object$result_table) > 0) {
    cat(paste0(
      "The table below (in the next console output) shows the model results summary.\n",
      "Dimensions: ", nrow(object$result_table), " rows x ", ncol(object$result_table), " columns.\n",
      "Columns: ", paste(colnames(object$result_table), collapse = ", "), ".\n"
    ))
    print(object$result_table)
  } else {
    cat("No prediction results available in the table.\n")
  }
  
  invisible(object)
}

#' Print a summary for a prediction result
#'
#' @param x A `hbsae` object 
#' @param ... Other potential arguments
#'
#' @return Summary of a prediction result
#' @export
#'
print.hbsae_results <- function(x, ...) {
  summary(x, ...) # Call the S3 summary method
}

#' Create a summary of a convergence check
#'
#' @param object A `hbcc` object
#' @param ... Other potential arguments
#'
#' @return Summary of a convergence check
#' 
#' @export
#' 
summary.hbcc_results <- function(object, ...) {
  if (!inherits(object, "hbcc_results")) {
    stop("Input must be of class 'hbcc'.")
  }
  
  cat("\n=========== Model Convergence Diagnostics Summary ===========\n")
  
  if (!is.null(object$rhat_ess)) {
    cat("\n----- R-hat and Effective Sample Size (ESS) -----\n")
    ess_df <- object$rhat_ess
    
    cat(
      paste0(
        "The following table (shown in the next console output) summarizes convergence diagnostics",
        "including R-hat, Bulk ESS, and Tail ESS for each parameter.\n",
        "Table dimensions: ", nrow(ess_df), " rows x ", ncol(ess_df), " columns.\n",
        "Columns: ", paste(colnames(ess_df), collapse = ", "), ".\n\n"
      )
    )
    
    print(ess_df)
  } else {
    cat("\n!!! R-hat and ESS diagnostics not computed or available.\n")
  }
  
  if (!is.null(object$geweke)) {
    cat("\n----------------- Geweke Diagnostic -----------------\n")
    # Geweke output can be verbose, print a summary or head
    print(utils::head(object$geweke$z)) 
    cat("Note: Displaying Z-scores. Values outside approx. +/- 2 may indicate non-convergence.\n")
  } else {
    cat("\n!!! Geweke diagnostic not computed or available.\n")
  }
  
  if (!is.null(object$raftery)) {
    cat("\n-------------- Raftery-Lewis Diagnostic --------------\n")
    print(object$raftery$resmatrix)
  } else {
    cat("\n!!! Raftery-Lewis diagnostic not computed or available.\n")
  }
  
  if (!is.null(object$heidel)) {
    cat("\n----------- Heidelberger-Welch Diagnostic -----------\n")
    # Heidel output is a matrix of logicals (pass/fail) and test stats
    print(utils::head(object$heidel))
    cat("Note: First column indicates stationarity test result (1=pass, 0=fail).\n")
  } else {
    cat("\n!!! Heidelberger-Welch diagnostic not computed or available.\n")
  }
  
  cat("\n------------------- Diagnostic Plot --------------------\n")
  cat("\nDiagnostic plots are stored in the '$plots' element and can be printed separately (e.g., object$plots$trace).\n")
  
  invisible(object)
}

#' Print a summary for a convergence check
#'
#' @param x A `hbcc` object 
#' @param ... Other potential arguments
#'
#' @return Summary of a convergence check
#' @export
#'
print.hbcc_results <- function(x, ...) {
  summary(x, ...) # Call the S3 summary method
}

#' Create a summary of a model goodness of fit and prior sensitivity
#'
#' @param object A `hbmc_results` object
#' @param ... Other potential arguments
#'
#' @return Summary of a model goodness of fit and prior sensitivity
#' 
#' @export 
#' 
summary.hbmc_results <- function(object, ...) {
  if (!inherits(object, "hbmc_results")) {
    stop("Input must be of class 'hbmc_results'.")
  }
  
  cat("\n=============== Model Goodness of Fit & Comparison Summary9 ===============\n")
  
  # --- Primary Model Diagnostics ---
  cat("\n=============== Primary Model Diagnostics ===============\n")
  if (!is.null(object$primary_model_diagnostics)) {
    cat("Diagnostics for Primary Model ", names(object$all_brms_models)[1]) # Assuming all_brms_models is part of object or reconstruct name
    primary_diag <- object$primary_model_diagnostics
    
    cat("\n\n------------- Leave-One-Out (LOO) -------------\n")
    if (!is.null(primary_diag$loo)) {
      if(!is.null(primary_diag$loo$error_message)) {
        cat("LOO computation failed: ", as.character(primary_diag$loo$error_message), "\n")
      } else if (!is.null(primary_diag$loo$estimates)) {
        print(primary_diag$loo$estimates)
        if (!is.null(primary_diag$loo$high_k_count) && primary_diag$loo$high_k_count > 0) {
          cat(">> Pareto k diagnostic summary:\n"); print(primary_diag$loo$pareto_k_diagnostics)
          cat(">> Number of high Pareto k values (>0.7):", primary_diag$loo$high_k_count, "\n")
        }
        if (isTRUE(primary_diag$loo$moment_match_requested_by_user)) cat("Moment matching was requested by user.\n")
        if (isTRUE(primary_diag$loo$reloo_attempted)) {
          cat(">> Reloo was attempted. Successful: ", isTRUE(primary_diag$loo$reloo_successful), "\n")
        }
        if (length(primary_diag$loo$warning_messages) > 0) {
          cat(">> LOO Warnings/Messages:\n"); for (msg in primary_diag$loo$warning_messages) cat("- ", msg, "\n")
        }
      } else { cat("LOO estimates not available.\n") }
    } else { cat("\nLOO for Primary Model not computed.\n") }
    
    cat("\n\n-- Widely Applicable Information Criterion (WAIC) --\n")
    if (!is.null(primary_diag$waic)) {
      if(!is.null(primary_diag$waic$error_message)) { 
        cat("WAIC computation failed: ", as.character(primary_diag$waic$error_message), "\n")
      } else if (!is.null(primary_diag$waic$estimates)) { 
        print(primary_diag$waic$estimate) 
      } else { cat("WAIC estimates not available.\n") }
    } else { cat("\nWAIC for Primary Model not computed.\n") }
    
    cat("\n----------- Posterior Predictive Check Plot ----------\n")
    if(!is.null(primary_diag$pp_check_plot)) {
      cat("Posterior predictive check plot for primary model stored in \n")
      cat("'$primary_model_diagnostics$pp_check_plot'.\n")
      print(primary_diag$pp_check_plot)
    }
    
    cat("\n------------------- Parameter Plot -------------------\n")
    if(!is.null(primary_diag$params_plot)) {
      cat("Parameter plots for primary model stored in \n")
      cat("'$primary_model_diagnostics$params_plot'.\n")
      print(primary_diag$pp_check_plot)
    }
  }
  
  # --- Model Comparison Results ---
  cat("\n=============== Model Comparison Results ================\n")
  if (!is.null(object$comparison_results) && length(object$comparison_results) > 0) {
    cat("\n---------- Model Comparison Results ----------\n")
    for (comp_name in names(object$comparison_results)) {
      cat("\n-- Comparison:", comp_name, "--\n")
      comp <- object$comparison_results[[comp_name]]
      model_i_name <- sub(paste0(names(object$all_brms_models)[1], "_vs_"), "", comp_name) # Assuming all_brms_models is accessible or reconstruct
      
      if (!is.null(comp$loo_model_i)) {
        cat(paste0("- LOO (", model_i_name, "):\n"))
        if(!is.null(comp$loo_model_i$error_message)) {
          cat("  LOO computation failed: ", as.character(comp$loo_model_i$error_message), "\n")
        } else if (!is.null(comp$loo_model_i$estimates)) {
          print(comp$loo_model_i$estimates)
          if (comp$loo_model_i$high_k_count > 0) {
            cat("  Pareto k diagnostic summary:\n"); print(comp$loo_model_i$pareto_k_diagnostics)
            cat("  Number of high Pareto k values (>0.7):", comp$loo_model_i$high_k_count, "\n")
          }
          if (length(comp$loo_model_i$warning_messages) > 0) {
            cat("  LOO Warnings/Messages for ", model_i_name, ":\n"); for (msg in comp$loo_model_i$warning_messages) cat("  - ", msg, "\n")
          }
        } else { cat(paste0("  LOO estimates for ", model_i_name, " not available.\n")) }
      }
      if (!is.null(comp$waic_model_i)) {
        cat(paste0("- WAIC (", model_i_name, "):\n"))
        if(!is.null(comp$waic_model_i$error_message)) {
          cat("  WAIC computation failed: ", as.character(comp$waic_model_i$error_message), "\n")
        } else if(!is.null(comp$waic_model_i$estimates)){
          print(comp$waic_model_i$estimates)
        } else {cat(paste0("  WAIC estimates for ", model_i_name, " not available.\n"))}
      }
      if (!is.null(comp$bf_comparison)) {
        cat(paste0("- Bayes Factor (vs ", model_i_name, "):\n"))
        if(!is.null(comp$bf_comparison$error_message) && comp$bf_comparison$error_message != "") {
          cat("  Bayes Factor computation failed: ", as.character(comp$bf_comparison$error_message), "\n")
        } else if (!is.null(comp$bf_comparison$bf) && length(comp$bf_comparison$bf) == 1 && !is.na(comp$bf_comparison$bf)) {
          cat("  Bayes Factor (BF): ", round(comp$bf_comparison$bf, 3), "\n")
        } else { cat("  Bayes Factor not available or computation failed.\n") }
      }
    }
  } else if (object$models_compared_count > 1 && is.null(object$comparison_results)) {
    cat("\n\nModel comparison metrics were requested but not computed or available.\n")
  } else {
    cat("Model comparison metrics was not run or no results are available.\n")
  }
  
  # --- Prior Sensitivity Analysis Results ---
  cat("\n========== Prior Sensitivity Analysis Results ===========\n")
  cat("\n--- Prior Sensitivity Analysis Summary ---\n\n")
  if (!is.null(object$prior_sensitivity_results)) {
    if (is.list(object$prior_sensitivity_results) && !is.null(object$prior_sensitivity_results$message) && length(object$prior_sensitivity_results)==1) {
      # Handle simple message case
      cat(object$prior_sensitivity_results$message, "\n")
    } else if (is.list(object$prior_sensitivity_results)) {
      for(model_sens_name in names(object$prior_sensitivity_results)){
        sens_result <- object$prior_sensitivity_results[[model_sens_name]]
        cat("\n- For model '", model_sens_name, "':\n", sep = "")
        cat("\n  > Sensitivity result:\n")
        print(sens_result$result)
        cat("\n  > Plot:\n")
        print(sens_result$plot)
        cat("  Plot may appear in a separate graphics window.\n")
      }
    } else if (is.character(object$prior_sensitivity_results)) {
      cat(object$prior_sensitivity_results, "\n")
    } else {
      cat("Prior sensitivity results are available but in an unrecognized format.\n")
    }
  } else {
    cat("Prior sensitivity analysis was not run or no results are available.\n")
  }
  
  invisible(object)
}

#' Print a summary for a model goodness of fit and prior sensitivity
#'
#' @param x A `hbmc_results` object
#' @param ... Other potential arguments
#'
#' @return Summary of a model goodness of fit and prior sensitivity
#' @export
#'
print.hbmc_results <- function(x, ...) { 
  summary(x, ...) 
}


#' Create a summary of a prior predictive check
#'
#' @param object A `hbpc_results` object
#' @param ... Other potential arguments
#'
#' @return Summary of a prior predictive check
#' 
#' @export
#' 
summary.hbpc_results <- function(object, ...) {
  if (!inherits(object, "hbpc_results")) {
    stop("Input must be of class 'hbpc_results'.")
  }
  
  cat("\n================= Prior Predictive Check Summary =================\n")
  
  cat("\n-------------- Priors Used in the Model --------------\n")
  if (is.data.frame(object$prior_summary) || is.matrix(object$prior_summary)) {
    print(object$prior_summary)
  } else {
    cat(as.character(object$prior_summary), "\n")
  }
  
  cat("\n----- Summary of Parameter Draws from Prior Model -----\n")
  if (inherits(object$prior_draws_summary, "brmssummary")) {
    print(object$prior_draws_summary)
  } else {
    cat(as.character(object$prior_draws_summary), "\n")
  }
  
  cat("\n--------------- Priors Predictive Plot ----------------\n")
  cat("\nNote: The prior predictive plot is stored in '$prior_predictive_plot'.\n")
  cat("This plot compares observed data (if applicable) with predictions generated solely from the priors.\n")
  
  invisible(object)
}

#' Print a summary for a prior predictive check
#'
#' @param x A `hbpc_results` object 
#' @param ... Other potential arguments
#'
#' @return Summary of a prior predictive check
#' @export
#'
print.hbpc_results <- function(x, ...) {
  summary(x, ...)
}
