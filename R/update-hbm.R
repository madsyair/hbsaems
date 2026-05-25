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
#' @section Auto-fallback for new formula terms:
#' When you supply a new \code{formula.} that references variables not in
#' the original model frame, \pkg{brms}'s default \code{update.brmsfit}
#' refuses with \emph{"New variables found ...; supply data again via
#' newdata"}.  \code{update_hbm} catches this specific error and
#' automatically retries with \code{newdata = object$data} (the data
#' frame stored on the original \code{hbmfit}).  Pass an explicit
#' \code{newdata} to override this behaviour.
#'
#' @section Fixed-parameter columns:
#' Models fitted with \code{sampling_variance}, \code{n} + \code{deff}
#' (in \code{hbm_betalogitnorm}), or \code{fixed_params} carry hidden
#' offset columns named \code{.hbsaems_<par>_fixed} in their data
#' frames.  When \code{update_hbm} receives a \code{newdata} that
#' \emph{does not} have these columns, brms refuses to refit with
#' \emph{"variables can neither be found in 'data' nor in 'data2'"}.
#' \code{update_hbm} now detects this case and:
#' \itemize{
#'   \item warns the user when offset columns are present in the
#'         original data but missing in \code{newdata};
#'   \item either copies the columns over from the original
#'         \code{object$data} (when \code{nrow(newdata) ==
#'         nrow(object$data)}, which is the typical "same areas,
#'         updated covariates" case), or
#'   \item raises an informative error pointing the user to either
#'         supply the columns in \code{newdata} explicitly or refit
#'         from scratch with a fresh \code{hbm()} call.
#' }
#'
#' @examples
#' \donttest{
#' library(hbsaems)
#' library(brms)
#' data("data_fhnorm")
#' model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
#'              re = ~ (1 | regency),    # area-level random effect
#'              chains = 4, iter = 2000, warmup = 1000, cores = 1,
#'              seed = 1, refresh = 0)
#'
#' # Re-run with slightly more iterations (no data change needed).
#' # In production you would jump to e.g. iter = 4000, warmup = 2000.
#' model2 <- update_hbm(model, iter = 1000, warmup = 500)
#'
#' # Add a predictor: the auto-fallback transparently retries with the
#' # stored data frame.  Equivalent to passing newdata = data_fhnorm.
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

  # -- 1. Preserve `.hbsaems_<par>_fixed` columns when newdata is supplied ----
  # Models fitted with sampling_variance / n+deff / fixed_params attach
  # hidden offset columns to the model data frame.  If the user provides
  # a newdata that lacks these columns, brms::update.brmsfit() refuses
  # with "variables can neither be found in 'data' nor in 'data2'".
  # We detect the case and copy the columns over when row counts match;
  # otherwise we raise an informative error.
  if (!is.null(newdata)) {
    fixed_cols <- grep("^\\.hbsaems_.*_fixed$",
                        names(object$data), value = TRUE)
    if (length(fixed_cols) > 0L) {
      missing_in_new <- setdiff(fixed_cols, names(newdata))
      if (length(missing_in_new) > 0L) {
        if (nrow(newdata) == nrow(object$data)) {
          # Same-size newdata: assume same row ordering and copy columns over.
          # This is the typical "tweak some covariates, refit" use case.
          warning(
            "newdata is missing hbsaems offset column(s): ",
            paste(shQuote(missing_in_new), collapse = ", "), ". ",
            "Copying from the original `object$data` (row order is ",
            "assumed unchanged).  If you have re-ordered or filtered ",
            "the data, supply the columns explicitly in `newdata`.",
            call. = FALSE
          )
          for (col in missing_in_new) {
            newdata[[col]] <- object$data[[col]]
          }
        } else {
          stop(
            "newdata is missing hbsaems offset column(s) that the model ",
            "requires: ", paste(shQuote(missing_in_new), collapse = ", "),
            ".\n",
            "  These columns were generated by the original `hbm()` call ",
            "from `sampling_variance`, `n`+`deff`, or `fixed_params`. ",
            "Since `nrow(newdata)` (", nrow(newdata), ") differs from ",
            "`nrow(object$data)` (", nrow(object$data),
            "), update_hbm() cannot safely copy them over.\n",
            "  Options:\n",
            "    * Supply the offset column(s) in `newdata` directly, or\n",
            "    * Refit from scratch with `hbm(...)` and the original ",
            "sugar arguments (e.g. `sampling_variance = \"D\"`).",
            call. = FALSE
          )
        }
      }
    }
  }

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

  # Defensive: when the user supplies a new formula but no newdata, and
  # the new formula introduces variables not present in the original
  # model frame, brms refuses with "New variables found: ... Please
  # supply your data again via argument 'newdata'."  We try once with
  # the call as-is, and if it fails for this specific reason, we
  # retry with `newdata = object$data` (since the variables typically
  # exist in the stored data frame).
  new_model <- tryCatch(
    do.call(update, args),
    error = function(e) {
      msg <- conditionMessage(e)
      if (!is.null(formula.) && is.null(newdata) &&
          grepl("New variables found", msg, fixed = TRUE)) {
        args$newdata <- object$data
        do.call(update, args)
      } else {
        stop(e)
      }
    }
  )
  data_use <- if (!is.null(newdata)) newdata else object$data

  new_hbmfit(
    model          = new_model,
    missing_method = object$missing_method,
    data           = data_use
  )
}
