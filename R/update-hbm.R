# R/update-hbm.R
# =============================================================================
# update_hbm() -- refit a model with modified arguments.
# Paired with update.hbmfit() in hbmfit-methods.R, which simply calls this.
# =============================================================================

#' Update a Fitted HBM
#'
#' Refits an \code{hbmfit} model with one or more arguments changed.  Useful
#' for re-running with longer chains, more iterations, or new data without
#' retyping the full \code{\link{hbm}} call.
#'
#' @param object   An \code{hbmfit} object.
#' @param newdata  Optional replacement \code{data.frame}.
#' @param formula. Optional new formula (note the trailing dot, following
#'   \code{stats::update}).  Pass \code{. ~ . + new_predictor} to add a term.
#' @param iter     Optional new total number of iterations.
#' @param warmup   Optional new warm-up length.
#' @param chains   Optional new number of MCMC chains.
#' @param cores    Optional new number of cores.
#' @param control  Optional new control list (e.g.\ \code{list(adapt_delta = 0.99)}).
#' @param ...      Additional arguments forwarded to \code{\link[brms]{update.brmsfit}}.
#'
#' @return An updated \code{hbmfit} object.
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | group),    # area-level random effect
#'              chains = 2, iter = 1000, warmup = 500, cores = 1,
#'              seed = 1, refresh = 0)
#'
#' # Re-run with more iterations
#' model2 <- update_hbm(model, iter = 4000, warmup = 2000)
#'
#' # Add a predictor
#' model3 <- update_hbm(model, formula. = . ~ . + x2)
#' }
#' @export
update_hbm <- function(object,
                       newdata  = NULL,
                       formula. = NULL,
                       iter     = NULL,
                       warmup   = NULL,
                       chains   = NULL,
                       cores    = NULL,
                       control  = NULL,
                       ...) {

  if (!is.hbmfit(object))
    stop("'object' must be an hbmfit object.", call. = FALSE)

  # Build the argument list dynamically: only include arguments the user
  # actually changed.  This avoids accidentally overriding settings stored
  # in the original brmsfit with NULLs.
  args <- list(object = object$model)
  if (!is.null(formula.)) args$formula. <- formula.
  if (!is.null(newdata))  args$newdata  <- newdata
  if (!is.null(iter))     args$iter     <- iter
  if (!is.null(warmup))   args$warmup   <- warmup
  if (!is.null(chains))   args$chains   <- chains
  if (!is.null(cores))    args$cores    <- cores
  if (!is.null(control))  args$control  <- control
  args <- c(args, list(...))

  new_model <- do.call(update, args)
  data_use  <- if (!is.null(newdata)) newdata else object$data

  new_hbmfit(
    model          = new_model,
    missing_method = object$missing_method,
    data           = data_use
  )
}
