#' Hierarchical Bayesian Small Area Models
#' @title Hierarchical Bayesian Small Area Models
#' @description Hierarchical Bayesian Small Area Models
#' @name hbm
#' @param formula Formula specifying the model structure
#' @param sae_sampling The family distribution for the response
#' @param sae_link Link function for HBSAE
#' @param re Random effects formula
#' @param sre Spatial random effect formula with options for CAR or SAR
#' @param data Dataset used for model fitting
#' @param prior Priors for the model parameters (default: `NULL`)
#' @param control A list of control parameters for the sampler (default: `list()`)
#' @param chains Number of Markov chains (default: 4)
#' @param iter Total number of iterations per chain (default: 2000)
#' @param warmup Number of warm-up iterations per chain (default: floor(iter/2))
#' @param cores Number of CPU cores to use (default: 1)
#' @param seed Random seed for reproducibility (default: `NULL`)
#' @param ... Additional arguments passed to `brm`
#' @return A `brmsfit` object
#' @importFrom brms brm
#' @importFrom brms brmsfamily
#' @importFrom brms bf
#' @param formula Formula specifying the model structure
#' @param sae_sampling The family distribution for the response
#' @param sae_link Link function for HBSAE
#' @param re Random effects formula
#' @param sre Spatial random effect formula with options for CAR or SAR
#' @param data Dataset used for model fitting
#' @return A `brmsfit` object
#' #' @export
hbm <- function(formula,
                sae_sampling = "gaussian",
                sae_link = "identity",
                re = NULL,
                sre = NULL,
                data,
                prior = NULL,
                control = list(),
                chains = 4,
                iter = 2000,
                warmup = floor(iter / 2),
                cores = 1,
                seed = NULL,
                ...)  {
data<-data.frame(data)
  n<-nrow(data)
    if (!is.null(re)) {
    formula <- bf(formula + re)
  }else{
    data$re <- rep(1,n)
    formula <- bf(formula + re)
  }
  if (!is.null(sre)) {
    if (grepl("car|sar", sre)) {
      warning("CAR and SAR spatial effects require advanced setup.")
    }
    formula <- bf(formula + sre)
  }

  brms_model <-brm(formula = formula,
                    family = brmsfamily(sae_sampling, link = sae_link),
                    data = data,
                    prior = prior,
                    control = control,
                    chains = chains,
                    iter = iter,
                    warmup = warmup,
                    cores = cores,
                    seed = seed,
                    ...)

  return(brms_model)

}
