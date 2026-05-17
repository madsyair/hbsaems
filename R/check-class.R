# R/check-class.R
# =============================================================================
# Base S3 class for pre-fit check results.
#
# All inspection functions in hbsaems return an object whose class vector
# includes "hbsaems_check" as its tail.  This allows shared S3 methods
# (is.hbsaems_check, summary fall-back) without forcing every subclass to
# re-implement them.
#
# Concrete subclasses (each adds its own name as the head of the class
# vector):
#   c("hbsaems_data_check",    "hbsaems_check")  -- from check_data()
#   c("hbsaems_spatial_check", "hbsaems_check")  -- from check_spatial_weight()
#   c("hbsaems_shiny_check",   "hbsaems_check")  -- from check_shiny_deps()
# =============================================================================


#' Test Whether an Object Is an hbsaems Check Result
#'
#' Predicate for the \code{"hbsaems_check"} base class.  All pre-fit
#' inspection functions in \pkg{hbsaems} (\code{\link{check_data}},
#' \code{\link{check_spatial_weight}}, \code{\link{check_shiny_deps}})
#' return an object that inherits from this class, enabling generic
#' handling.
#'
#' @param x Any R object.
#'
#' @return A single logical.
#'
#' @examples
#' chk <- check_data(data.frame(y = 1:5, x = 1:5),
#'                   response = "y", auxiliary = "x")
#' is.hbsaems_check(chk)              # TRUE
#' is.hbsaems_check("not a check")    # FALSE
#'
#' @seealso \code{\link{check_data}}, \code{\link{check_spatial_weight}},
#'   \code{\link{check_shiny_deps}}
#' @export
is.hbsaems_check <- function(x) inherits(x, "hbsaems_check")


#' Generic Summary Method for hbsaems Check Results
#'
#' Provides a fall-back summary that simply calls the underlying
#' \code{print()} method.  Subclasses that need a more detailed summary
#' (e.g.\ \code{summary.hbsaems_data_check}) override this.
#'
#' @param object An object inheriting from \code{"hbsaems_check"}.
#' @param ... Unused.
#' @return The input \code{object}, invisibly.
#'
#' @method summary hbsaems_check
#' @export
summary.hbsaems_check <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
