# R/utils.R
# =============================================================================
# Internal utilities shared across the package.
# =============================================================================

#' Default Value for NULL
#'
#' Equivalent to the rlang \code{\%||\%} operator.  Internal use only.
#'
#' @param x,y Any R objects.
#' @return \code{x} if non-\code{NULL}, otherwise \code{y}.
#' @keywords internal
#' @name op-null-default
`%||%` <- function(x, y) if (is.null(x)) y else x


# -- Soft deprecation warning -----------------------------------------------
# Used inside functions that have renamed an argument.  Emits a warning
# that tells the user the new argument name and when the old one will be
# removed.  Caller is responsible for actually mapping the old value to
# the new one.
#
# @noRd
.deprecate_arg <- function(old, new, removed_in) {
  warning(sprintf(
    "Argument `%s` is deprecated as of v1.0.0; use `%s` instead. ",
    old, new),
    sprintf("`%s` will be removed in %s.", old, removed_in),
    call. = FALSE)
}
