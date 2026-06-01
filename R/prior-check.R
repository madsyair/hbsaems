# R/prior-check.R  (hbsaems 1.1.0)
# =============================================================================
# prior_check(): data and response_var are now OPTIONAL.  When omitted they
# are auto-detected from the fitted model (data from the stored model frame,
# response_var from the model formula).  Supplying them explicitly is
# unchanged and always takes precedence -- this is a purely additive,
# backward-compatible enhancement (new in 1.1.0).
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
#'   \strong{Optional since 1.1.0:} if \code{NULL} (default) the data frame
#'   stored on the fitted model is used.
#' @param response_var Character scalar naming the response column.
#'   \strong{Optional since 1.1.0:} if \code{NULL} (default) it is determined
#'   from the model formula's left-hand side.
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
#' the marginal distribution of new data under the prior alone.  Comparing it
#' to the observed data is a fast sanity check: if the prior predictive places
#' no mass anywhere near the data, the priors are likely too tight or in the
#' wrong location.
#'
#' @section Automatic argument detection (1.1.0):
#' When \code{data} is omitted it is taken from \code{model$data} (the model
#' frame stored on the fit).  When \code{response_var} is omitted it is read
#' from the model formula via \code{\link[brms]{brmsterms}}; if the formula
#' has no left-hand side (so no response can be determined), an error is
#' raised asking the caller to supply \code{response_var} explicitly.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model_prior <- hbm(
#'   formula      = brms::bf(y ~ x1 + x2 + x3),
#'   data         = data_fhnorm,
#'   sample_prior = "only",
#'   prior        = c(
#'     brms::prior(normal(0, 1), class = "b"),
#'     brms::prior(normal(0, 5), class = "Intercept")
#'   ),
#'   chains = 4, iter = 2000, warmup = 1000, cores = 1,
#'   seed = 42, refresh = 0
#' )
#'
#' # Explicit (as before):
#' pc <- prior_check(model_prior, data = data_fhnorm, response_var = "y")
#'
#' # New in 1.1.0 -- data and response auto-detected from the model:
#' pc <- prior_check(model_prior)
#' print(pc)
#' plot(pc)
#' }
#'
#' @seealso \code{\link{hbm}}, \code{\link{convergence_check}}
#' @export
prior_check <- function(model, data = NULL, response_var = NULL,
                        ndraws_ppc = 50, ...) {

  # -- Model validation -------------------------------------------------------
  if (!is.hbmfit(model) && !inherits(model, "brmsfit"))
    stop("'model' must be an hbmfit or brmsfit object.", call. = FALSE)

  brms_model <- if (is.hbmfit(model)) model$model else model

  # -- Auto-detect data when omitted -----------------------------------------
  if (is.null(data)) {
    data <- tryCatch(
      if (is.hbmfit(model)) model$data else brms_model$data,
      error = function(e) NULL
    )
    if (!is.data.frame(data))
      stop("'data' could not be automatically determined from the model; ",
           "please supply 'data' explicitly.", call. = FALSE)
  }
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.", call. = FALSE)

  # -- Auto-detect response_var when omitted ---------------------------------
  response_auto <- is.null(response_var)
  if (response_auto) {
    response_var <- .detect_response_var(brms_model)
    if (is.null(response_var) || !nzchar(response_var))
      stop("The 'response_var' argument is required and could not be ",
           "automatically determined from the model formula; please ",
           "supply 'response_var' explicitly.", call. = FALSE)
  }
  if (!is.character(response_var) || length(response_var) != 1L)
    stop("'response_var' must be a single character string.", call. = FALSE)

  if (!response_var %in% names(data)) {
    if (response_auto)
      stop("Automatically determined response variable '", response_var,
           "' not found in the provided data.", call. = FALSE)
    stop("Response variable '", response_var,
         "' not found in 'data'.", call. = FALSE)
  }

  # -- ndraws_ppc validation --------------------------------------------------
  if (!is.numeric(ndraws_ppc) || length(ndraws_ppc) != 1L ||
      ndraws_ppc < 1L)
    stop("'ndraws_ppc' must be a positive integer.", call. = FALSE)
  ndraws_ppc <- as.integer(ndraws_ppc)

  observed <- data[[response_var]]

  # -- Multivariate handling --------------------------------------------------
  # For multivariate brms models, posterior_predict() returns a 3-D array
  # with one slice per response and pp_check() requires `resp` to
  # disambiguate.  Detect the multivariate case and pass resp = response_var.
  is_mv <- tryCatch(
    !is.null(brms_model$formula$forms),
    error = function(e) FALSE
  )

  pp_args <- list(object   = brms_model,
                  ndraws   = ndraws_ppc,
                  draw_ids = seq_len(ndraws_ppc))
  if (is_mv) pp_args$resp <- response_var

  draws <- tryCatch(
    do.call(brms::posterior_predict, pp_args),
    error = function(e) {
      stop("Failed to generate prior predictive draws: ", e$message,
           "\nDid you fit the model with sample_prior = 'only'?",
           call. = FALSE)
    }
  )

  ppc_args <- list(object = brms_model, ndraws = ndraws_ppc)
  if (is_mv) ppc_args$resp <- response_var
  ppc_plot <- tryCatch(
    do.call(brms::pp_check, ppc_args),
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


# -----------------------------------------------------------------------------
# Internal: determine the response variable name from a fitted brms model's
# formula.  Returns a single character string, or NULL when the formula has
# no usable left-hand side (e.g. a prior-only formula written as ~ x1 + x2).
# -----------------------------------------------------------------------------
.detect_response_var <- function(brms_model) {
  f <- tryCatch(stats::formula(brms_model), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  resp <- tryCatch({
    bt <- brms::brmsterms(f)
    if (!is.null(bt$responses)) {
      bt$responses[1L]               # multivariate: take the first response
    } else if (!is.null(bt$respform)) {
      all.vars(bt$respform)[1L]      # univariate
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (length(resp) == 0L) return(NULL)
  # brms::brmsterms()$responses is a *named* character vector; strip the
  # name so the result is a plain scalar (a stray name would otherwise be
  # carried into pp_check(resp = ...) / posterior_predict()).
  as.character(resp)[1L]
}
