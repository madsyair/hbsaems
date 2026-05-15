# R/prior-check.R
# =============================================================================
# Primary prior-predictive-check API for hbsaems v0.3.0+.
# Replaces the body of the deprecated hbpc().
# =============================================================================

#' Prior Predictive Check for Fitted HBMs
#'
#' Generates prior predictive samples from a model fit with
#' \code{sample_prior = "only"} and compares them to the observed data.
#' This is the primary prior-check function (supersedes the deprecated
#' \code{\link{hbpc}}).
#'
#' @param model        An \code{hbmfit} or \code{brmsfit} object fit with
#'   \code{sample_prior = "only"} (see \code{\link{hbm}}).
#' @param data         A \code{data.frame} containing the response variable.
#' @param response_var Character scalar.  Name of the response variable column.
#' @param ndraws_ppc   Integer.  Number of prior predictive draws to overlay
#'   on the plot (default \code{50}).
#' @param ...          Currently unused; reserved for future extensions.
#'
#' @return An \code{hbpc_results} object with components:
#'   \describe{
#'     \item{\code{prior_predictive_plot}}{A \code{ggplot} from
#'       \code{\link[brms]{pp_check}}, or \code{NULL} if it could not be
#'       generated.}
#'     \item{\code{prior_draws}}{A draws matrix from
#'       \code{\link[brms]{posterior_predict}} sized
#'       \code{ndraws_ppc \\times nrow(data)}.}
#'     \item{\code{observed}}{The observed response vector.}
#'   }
#'
#' @details
#' The prior predictive distribution is
#' \deqn{p(y_{\text{rep}}) =
#'   \int p(y_{\text{rep}} \mid \theta)\, p(\theta) \, \mathrm{d}\theta,}
#' that is, the marginal distribution of new data \eqn{y_{\text{rep}}} under
#' the prior alone.  Comparing this to the observed data is a fast sanity
#' check: if the prior predictive places no mass anywhere near the data,
#' the priors are likely too tight or in the wrong location.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' # `sample_prior = "only"` requires all coefficients to have a proper
#' # prior; supply explicit priors on the regression class.
#' model_prior <- hbm(
#'   formula      = brms::bf(y ~ x1 + x2 + x3),
#'   data         = data_fhnorm,
#'   sample_prior = "only",
#'   prior        = c(
#'     brms::prior(normal(0, 1), class = "b"),
#'     brms::prior(normal(0, 5), class = "Intercept")
#'   ),
#'   chains = 2, iter = 1000, warmup = 500, cores = 1,
#'   seed = 42, refresh = 0
#' )
#' pc <- prior_check(model_prior,
#'                   data         = data_fhnorm,
#'                   response_var = "y")
#' print(pc)
#' plot(pc)
#' }
#'
#' @seealso \code{\link{hbm}}, \code{\link{convergence_check}}
#' @export
prior_check <- function(model, data, response_var, ndraws_ppc = 50, ...) {

  # Input validation
  if (!is.hbmfit(model) && !inherits(model, "brmsfit"))
    stop("'model' must be an hbmfit or brmsfit object.", call. = FALSE)
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.", call. = FALSE)
  if (!is.character(response_var) || length(response_var) != 1L)
    stop("'response_var' must be a single character string.", call. = FALSE)
  if (!response_var %in% names(data))
    stop("Response variable '", response_var,
         "' not found in 'data'.", call. = FALSE)
  if (!is.numeric(ndraws_ppc) || length(ndraws_ppc) != 1L ||
      ndraws_ppc < 1L)
    stop("'ndraws_ppc' must be a positive integer.", call. = FALSE)
  ndraws_ppc <- as.integer(ndraws_ppc)

  brms_model <- if (is.hbmfit(model)) model$model else model
  observed   <- data[[response_var]]

  # Prior predictive draws
  draws <- tryCatch(
    brms::posterior_predict(brms_model,
                            ndraws   = ndraws_ppc,
                            draw_ids = seq_len(ndraws_ppc)),
    error = function(e) {
      stop("Failed to generate prior predictive draws: ", e$message,
           "\nDid you fit the model with sample_prior = 'only'?",
           call. = FALSE)
    }
  )

  # Plot
  ppc_plot <- tryCatch(
    brms::pp_check(brms_model, ndraws = ndraws_ppc),
    error = function(e) {
      message("pp_check() failed: ", e$message)
      NULL
    }
  )

  structure(
    list(
      prior_predictive_plot = ppc_plot,
      prior_draws           = draws,
      observed              = observed
    ),
    class = "hbpc_results"
  )
}
