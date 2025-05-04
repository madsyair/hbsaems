#' Hierarchical Bayesian Convergence Checks
#' @title hbcc : Hierarchical Bayesian Convergence Checks
#' @description This function is designed to evaluate the convergence and quality of a Bayesian hierarchical model. 
#' It performs several diagnostic tests and generates various plots to assess Markov Chain Monte Carlo (MCMC) performance.
#' @name hbcc
#'
#' @param model A `brmsfit` or `hbmfit` object.
#' @param diag_tests Character vector of diagnostic tests (default:"rhat", "geweke", "raftery", "heidel")
#' @param plot_types Character vector of plot types (default: trace","dens","acf", "nuts_energy", "rhat", "neff")
#'
#' @return A list containing:
#'   \item{rhat_ess}{Matrix of \code{Rhat}, \code{Bulk_ESS}, and \code{Tail_ESS} values for fixed and random effects.}
#'   \item{geweke}{Geweke diagnostic results (if selected).}
#'   \item{raftery}{Raftery-Lewis diagnostic results (if selected).}
#'   \item{heidel}{Heidelberger-Welch diagnostic results (if selected).}
#'   \item{plots}{A list of generated MCMC diagnostic plots, which may include:}
#'     \itemize{
#'       \item \code{"trace"} - Trace plot of the MCMC chains.
#'       \item \code{"dens"} - Density plot of the posterior distributions.
#'       \item \code{"acf"} - Autocorrelation function plot.
#'       \item \code{"nuts_energy"} - NUTS energy diagnostic plot.
#'       \item \code{"rhat"} - Rhat plot (if available).
#'       \item \code{"neff"} - Effective sample size plot.
#'     }
#'
#' @importFrom brms mcmc_plot as_draws_matrix as_draws_df as_draws_array
#' @importFrom coda as.mcmc geweke.diag raftery.diag heidel.diag
#' @importFrom posterior ess_bulk ess_tail
#' @importFrom ggplot2 ggtitle theme_minimal
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
#' # Convergence Checks
#' hbcc(model)
#' 
#' }

hbcc <- function(model, 
                 diag_tests = c("rhat", "geweke", "heidel","raftery" ), 
                 plot_types = c("trace","dens","acf", "nuts_energy", "rhat", "neff")
                 ) {
  
  if (inherits(model, "hbmfit")) {
    model <- model$model  
  }
  
  if (!inherits(model, "brmsfit")) {
    stop("Input model must be a brmsfit or hbmfit object.")
  }
  
  valid_diag_tests <- c("rhat", "geweke", "heidel","raftery" )
  valid_plot_types <- c("trace","dens","acf", "nuts_energy", "rhat", "neff")
  
  # Cek diag_tests
  invalid_diag <- setdiff(diag_tests, valid_diag_tests)
  if (length(invalid_diag) > 0) {
    stop("Invalid diag_tests: ", paste(invalid_diag, collapse = ", "))
  }
  
  # Cek plot_types
  invalid_plot <- setdiff(plot_types, valid_plot_types)
  if (length(invalid_plot) > 0) {
    stop("Invalid plot_types: ", paste(invalid_plot, collapse = ", "))
  }
  
  # Diagnostic tests
  results <- list()
  if ("rhat" %in% diag_tests) {
    # Summary diagnostics
    results$rhat_ess <- tryCatch({
      sum_fixed <- summary(model)$fixed[, c("Rhat", "Bulk_ESS", "Tail_ESS")]
      sum_random <- if (!is.null(summary(model)$random)) {
        summary(model)$random[[1]][, c("Rhat", "Bulk_ESS", "Tail_ESS")]
      } else {
        NULL
      }
      rbind(sum_fixed, sum_random)
    }, error = function(e) NULL)
  }
  
  draw_samples <- brms::as_draws_matrix(model)
  mcmc_samples <- coda::as.mcmc(draw_samples)
  
  if ("geweke" %in% diag_tests) {
    results$geweke <- tryCatch(geweke.diag(mcmc_samples), error = function(e) NULL)
  }
  if ("raftery" %in% diag_tests) {
    results$raftery <- tryCatch(raftery.diag(mcmc_samples), error = function(e) NULL)
  }
  if ("heidel" %in% diag_tests) {
    results$heidel <- tryCatch(heidel.diag(mcmc_samples), error = function(e) NULL)
  }
  
  # Plot diagnostics
  results$plots <- list()
  for (type in plot_types) {
    if (type %in% c("trace", "dens", "acf", "nuts_energy", "rhat", "neff")) {      
      results$plots[[type]] <- tryCatch(
        mcmc_plot(model, type = type),
        error = function(e) NULL
      )
    }
  }
  
  return(structure(
    list(rhat_ess = results$rhat_ess, geweke = results$geweke, raftery = results$raftery, heidel = results$heidel, plots = results$plots),
    class = "hbcc"
  ))
}
