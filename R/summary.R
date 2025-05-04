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
  
  cat("\n===== Parameter Estimation from brms =====\n")
  print(object$model)
  
  cat("\n========= Missing Data Handling ==========\n")
  if (is.null(object$handle_missing)) {
    cat("No missing data handling\n")
  } else {
    cat(" The model handles missing data with type:", object$handle_missing, "\n")
  }
  
  invisible(object)
}


#' Print a summary for a fitted model represented by a hbmfit object
#'
#' @param object A `hbmfit` object 
#' @param ... Other potential arguments
#'
#' @return Summary of the model
#' @export
#'
print.hbmfit <- function(object, ...) {
  summary(object, ...)
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
summary.hbsae <- function(object, ...) {
  if (!inherits(object, "hbsae")) {
    stop("Input must be of class 'hbsae'.")
  }
  
  cat("\n===== Summary of hbsae Prediction =====\n")
  
  cat("\n>> Model Performance Metrics\n")
  cat("RSE (%) (Model):", object$rse_model, "\n")
  
  cat("\n========== Prediction Table ==========\n")
  cat("Access data frame with $result_table")
  print(object$result_table)
  
  invisible(object)
}

#' Print a summary for a prediction result
#'
#' @param object A `hbsae` object 
#' @param ... Other potential arguments
#'
#' @return Summary of a prediction result
#' @export
#'
print.hbsae <- function(x, ...) {
  summary.hbsae(x, ...)
  invisible(x)
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
summary.hbcc <- function(object, ...) {
  if (!inherits(object, "hbcc")) {
    stop("Input must be of class 'hbcc'.")
  }
  
    cat("\n ----------- Model Diagnostics Summary -----------\n")
  
  # Display Geweke diagnostic
  if (!is.null(object$geweke)) {
    cat("\n =============== Geweke Diagnostic ===============\n")
    print(object$geweke)
  } else {
    cat("\n !!! Geweke diagnostic not available.\n")
  }
  
  # Display Raftery diagnostic
  if (!is.null(object$raftery)) {
    cat("\n =============== Raftery Diagnostic ==============\n")
    print(object$raftery)
  } else {
    cat("\n !!! Raftery diagnostic not available.\n")
  }
  
  # Display Heidel diagnostic
  if (!is.null(object$heidel)) {
    cat("\n ======= Heidelberger and Welch Diagnostic =======\n")
    print(object$heidel)
  } else {
    cat("\n !!! Heidel diagnostic not available.\n")
  }
  
  # Display Rhat and ESS
  if (!is.null(object$rhat_ess)) {
    cat("\n ===== Rhat and Effective Sample Size (ESS) ======\n")
    print(object$rhat_ess)
  } else {
    cat("\n !!! Rhat and ESS diagnostics not available.\n")
  }
  
  # Display Plots
  if (!is.null(object$plots) && length(object$plots) > 0) {
    cat("\n =============== Diagnostic Plots ===============\n")
    for (plot_name in names(object$plots)) {
      cat("\nPlot:", plot_name, "\n")
      print(object$plots[[plot_name]])
    }
  } else {
    cat("\n !!! No diagnostic plots available.\n")
  }
  
  invisible(object)
}

#' Print a summary for a convergence check
#'
#' @param object A `hbcc` object 
#' @param ... Other potential arguments
#'
#' @return Summary of a convergence check
#' @export
#'
print.hbcc <- function(x, ...) {
  summary.hbcc(x, ...)
  invisible(x)
}

#' Create a summary of a model goodness of fit
#'
#' @param object A `hbmc` object
#' @param ... Other potential arguments
#'
#' @return Summary of a model goodness of fit
#' 
#' @export 
#' 
summary.hbmc <- function(object, ...) {
  if (!inherits(object, "hbmc")) {
    stop("Input must be of class 'hbmc'.")
  }
  
  # Output LOO
  if (!is.null(object$loo1_values)) {
    cat("\n========== Leave-One-Out (LOO) Cross-Validation ==========\n")
    print(object$loo1)
  } else {
    cat("\n !!! LOO not available.\n")
  }
  
  # Output WAIC
  if (!is.null(object$waic1_values)) {
    cat("\n===== Widely Applicable Information Criterion (WAIC) =====\n")
    print(object$waic1)
  } else {
    cat("\n !!! WAIC not available.\n")
  }
  
  # Output Diagnostic Plots
  cat("\n=================== Diagnostic Plots ====================\n")
  # Posterior Predictive Check
  if (!is.null(object$pp_check)) {
    cat("\nPlot: pp_check\n")
    print(object$pp_check)
  }
  # Parameter Distribution Plot
  if (!is.null(object$params)) {
    cat("\nPlot: params\n")
    print(object$params)
  }
  # If neither plot is available
  if (is.null(object$pp_check) && is.null(object$params)) {
    cat("\n !!! No diagnostic plots available.\n")
  }
  
  # Output Model Comparison
  if (!is.null(object$model2)) {
    cat("\n=================== Model Comparison ====================\n")
    
    if (!is.null(object$loo2_values)) {
      cat("\n--- LOO Comparison ---\n")
      cat("Model 1 LOO:", object$loo1$elpd_loo, "\n")
      cat("Model 2 LOO:", object$loo2$elpd_loo, "\n")
      cat("Difference:", object$loo1$elpd_loo - object$loo2$elpd_loo, "\n")
    } else {
      cat("\n !!! LOO for Model 2 not available.\n")
    }
    
    if (!is.null(object$waic2_values)) {
      cat("\n--- WAIC Comparison ---\n")
      cat("Model 1 WAIC:", object$waic1$waic, "\n")
      cat("Model 2 WAIC:", object$waic2$waic, "\n")
      cat("Difference:", object$waic1$waic - object$waic2$waic, "\n")
    } else {
      cat("\n !!! WAIC for Model 2 not available.\n")
    }
    
    if (!is.null(object$bf)) {
      cat("\n--- Bayes Factor ---\n")
      print(object$bf)
    } else {
      cat("\n !!! Bayes Factor not available.\n")
    }
  }
  invisible(object)
}

#' Print a summary for a model goodness of fit
#'
#' @param object A `hbmc` object 
#' @param ... Other potential arguments
#'
#' @return Summary of a model goodness of fit
#' @export
#'
print.hbmc <- function(x, ...) {
  summary.hbmc(x, ...)
  invisible(x)
}