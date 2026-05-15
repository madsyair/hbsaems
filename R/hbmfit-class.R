# R/hbmfit-class.R
# =============================================================================
# hbmfit S3 class -- constructor, validators, class predicates.
#
# An hbmfit object is a named list with three mandatory slots:
#   $model          -- a brmsfit (or brmsfit_multiple) object
#   $missing_method -- character scalar or NULL
#   $data           -- the ORIGINAL data.frame before row deletion
#
# The original field name `handle_missing` is retained as an alias for
# backwards compatibility (see new_hbmfit()).
# =============================================================================

#' The hbmfit S3 Class
#'
#' \code{hbmfit} is the result class returned by all model-fitting functions
#' in \pkg{hbsaems}: \code{\link{hbm}}, \code{\link{hbm_betalogitnorm}},
#' \code{\link{hbm_binlogitnorm}}, and \code{\link{hbm_lnln}}.  It wraps a
#' \code{brmsfit} object together with the original data and fitting metadata.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{model}}{A \code{brmsfit} (or \code{brmsfit_multiple}) object.}
#'   \item{\code{missing_method}}{Character scalar -- the missing-data strategy
#'     used (\code{"deleted"}, \code{"model"}, \code{"multiple"}) or
#'     \code{NULL} when the data were complete.}
#'   \item{\code{data}}{The \strong{original} \code{data.frame} passed to the
#'     fitting function before any row deletion or imputation.  Downstream
#'     functions (e.g.\ \code{\link{sae_predict}}) need all rows -- including
#'     those with missing \eqn{Y} -- to produce area-level predictions.}
#'   \item{\code{handle_missing}}{Backwards-compatibility alias for
#'     \code{missing_method}; identical value.}
#' }
#'
#' @name hbmfit-class
NULL


# -- Constructor ---------------------------------------------------------------

#' Create a New hbmfit Object
#'
#' Low-level (internal) constructor.  All model-fitting functions must use
#' this constructor rather than calling \code{structure()} directly, so the
#' class invariants are enforced in one place.
#'
#' Together with \code{\link{validate_hbmfit}} and \code{\link{hbmfit}},
#' this follows the trio-constructor pattern recommended in Hadley
#' Wickham's \emph{Advanced R} (chapter 13): \code{new_hbmfit()} is fast
#' and minimal, \code{validate_hbmfit()} is slow and thorough, and
#' \code{hbmfit()} is the user-facing helper.
#'
#' @param model          A \code{brmsfit} or \code{brmsfit_multiple} object.
#' @param missing_method Character scalar or \code{NULL}.
#' @param data           The original \code{data.frame}.
#'
#' @return An object of class \code{"hbmfit"}.
#' @keywords internal
new_hbmfit <- function(model, missing_method = NULL, data) {
  stopifnot(
    inherits(model, c("brmsfit", "brmsfit_multiple")),
    is.null(missing_method) || (is.character(missing_method) &&
                                  length(missing_method) == 1L),
    is.data.frame(data) || is.matrix(data)
  )

  structure(
    list(
      model          = model,
      missing_method = missing_method,
      # Backwards-compatibility alias: legacy code reads $handle_missing
      handle_missing = missing_method,
      data           = as.data.frame(data)
    ),
    class = "hbmfit"
  )
}


#' Validate an hbmfit Object
#'
#' Public validator for the \code{\link{hbmfit-class}{hbmfit}} S3 class.
#' Runs all invariants that the cheap constructor (\code{\link{new_hbmfit}})
#' is permitted to skip.  Useful when reconstructing an \code{hbmfit}
#' object manually, reading one back from disk, or testing custom
#' family wrappers.
#'
#' Invariants verified:
#' \enumerate{
#'   \item Object is a list with class \code{"hbmfit"}.
#'   \item Has mandatory slots: \code{model}, \code{missing_method},
#'     \code{data}.
#'   \item \code{model} inherits from \code{brmsfit} or
#'     \code{brmsfit_multiple}.
#'   \item \code{missing_method} is \code{NULL} or a single character
#'     string in \code{c("deleted", "multiple", "model")}.
#'   \item \code{data} is a \code{data.frame} with \eqn{\ge 1} row.
#'   \item The \code{handle_missing} alias (if present) equals
#'     \code{missing_method}.
#' }
#'
#' @param x An object to validate.
#'
#' @return The input \code{x}, invisibly, when all invariants hold.
#'   Otherwise raises an informative error.
#'
#' @examples
#' \donttest{
#' # Minimal example without area-level RE (fixed-effects baseline) --
#' # suppress the area-RE advisory because this 5-row toy dataset cannot
#' # meaningfully estimate a random effect.
#' fit <- suppressWarnings(
#'   hbm(brms::bf(y ~ x1), data = data.frame(y = rnorm(5), x1 = 1:5),
#'       chains = 1, iter = 200, refresh = 0)
#' )
#' validate_hbmfit(fit)
#' }
#'
#' @seealso \code{\link{new_hbmfit}}, \code{\link{hbmfit}}
#' @export
validate_hbmfit <- function(x) {

  if (!inherits(x, "hbmfit"))
    stop("Object is not of class 'hbmfit'.", call. = FALSE)
  if (!is.list(x))
    stop("'hbmfit' must be a list.", call. = FALSE)

  required <- c("model", "missing_method", "data")
  missing_slots <- setdiff(required, names(x))
  if (length(missing_slots) > 0L)
    stop("hbmfit object missing required slot(s): ",
         paste(missing_slots, collapse = ", "), call. = FALSE)

  if (!inherits(x$model, c("brmsfit", "brmsfit_multiple")))
    stop("hbmfit$model must inherit from 'brmsfit' or ",
         "'brmsfit_multiple'; got: ",
         paste(class(x$model), collapse = "/"), call. = FALSE)

  if (!is.null(x$missing_method)) {
    if (!is.character(x$missing_method) || length(x$missing_method) != 1L)
      stop("hbmfit$missing_method must be NULL or a single character string.",
           call. = FALSE)
    if (!(x$missing_method %in% c("deleted", "multiple", "model")))
      stop("hbmfit$missing_method must be one of: 'deleted', 'multiple', ",
           "'model'; got: '", x$missing_method, "'.", call. = FALSE)
  }

  if (!is.data.frame(x$data))
    stop("hbmfit$data must be a data.frame.", call. = FALSE)
  if (nrow(x$data) == 0L)
    stop("hbmfit$data must have at least one row.", call. = FALSE)

  if (!is.null(x$handle_missing) &&
      !identical(x$handle_missing, x$missing_method))
    stop("hbmfit$handle_missing alias must equal hbmfit$missing_method ",
         "(both backwards-compat slots must agree).", call. = FALSE)

  invisible(x)
}


#' User-Facing Helper to Build an hbmfit Object
#'
#' Convenience constructor that calls \code{\link{new_hbmfit}} then
#' \code{\link{validate_hbmfit}} -- the safe public entry-point if you
#' ever need to build an \code{hbmfit} object manually (e.g.\ when
#' adapting a non-\pkg{brms} model).  In normal usage you do \emph{not}
#' need to call this: \code{\link{hbm}} and the distribution-specific
#' wrappers do it for you.
#'
#' @param model          A \code{brmsfit} or \code{brmsfit_multiple} object.
#' @param data           The \code{data.frame} used to fit \code{model}.
#' @param missing_method Character scalar or \code{NULL}.  One of
#'   \code{"deleted"}, \code{"multiple"}, \code{"model"}.
#'
#' @return A validated \code{hbmfit} object.
#'
#' @examples
#' \donttest{
#' raw <- brms::brm(y ~ x1, data = data.frame(y = rnorm(10), x1 = 1:10),
#'                  chains = 1, iter = 200, refresh = 0)
#' fit <- hbmfit(model = raw,
#'               data  = data.frame(y = rnorm(10), x1 = 1:10),
#'               missing_method = NULL)
#' validate_hbmfit(fit)
#' }
#'
#' @seealso \code{\link{new_hbmfit}}, \code{\link{validate_hbmfit}},
#'   \code{\link{hbm}}
#' @export
hbmfit <- function(model, data, missing_method = NULL) {
  validate_hbmfit(new_hbmfit(model = model,
                              missing_method = missing_method,
                              data = data))
}


# -- Class predicates ----------------------------------------------------------

#' Test Whether an Object Belongs to an hbsaems Result Class
#'
#' Lightweight type-checking predicates for all five result classes produced
#' by \pkg{hbsaems}.  Each predicate returns a single logical.
#'
#' @param x Any R object.
#' @return A single logical value.
#'
#' @examples
#' is.hbmfit("not a model")   # FALSE
#'
#' @name is-hbsaems
#' @export
is.hbmfit <- function(x) UseMethod("is.hbmfit")
#' @export
is.hbmfit.default <- function(x) inherits(x, "hbmfit")

#' @rdname is-hbsaems
#' @export
is.hbcc_results <- function(x) UseMethod("is.hbcc_results")
#' @export
is.hbcc_results.default <- function(x) inherits(x, "hbcc_results")

#' @rdname is-hbsaems
#' @export
is.hbmc_results <- function(x) UseMethod("is.hbmc_results")
#' @export
is.hbmc_results.default <- function(x) inherits(x, "hbmc_results")

#' @rdname is-hbsaems
#' @export
is.hbpc_results <- function(x) UseMethod("is.hbpc_results")
#' @export
is.hbpc_results.default <- function(x) inherits(x, "hbpc_results")

#' @rdname is-hbsaems
#' @export
is.hbsae_results <- function(x) UseMethod("is.hbsae_results")
#' @export
is.hbsae_results.default <- function(x) inherits(x, "hbsae_results")
