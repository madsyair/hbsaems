# R/spatial-validate.R
# =============================================================================
# Validate a spatial weight / adjacency matrix M against the theoretical
# requirements of CAR / SAR models.
#
# This catches the most common user mistakes BEFORE sending the matrix to
# Stan, where the resulting error messages are usually opaque or, worse,
# the model fits but produces incorrect inference.
#
# Implements 5 checks corresponding to standard SAE-spatial theory:
#   1. Square matrix
#   2. Zero diagonal (no self-loops)
#   3. Symmetry (required for CAR; warning for asymmetric SAR)
#   4. Style appropriate for sre_type (B for CAR, W for SAR)
#   5. Graph connectivity (warning for isolated nodes)
# =============================================================================


#' Validate a Spatial Weight Matrix Against CAR/SAR Theory
#'
#' Runs five theoretical compatibility checks on a spatial weight matrix
#' \eqn{M} and reports any deviations from the standard requirements of
#' the chosen model class.  Returns a structured object summarising the
#' results.
#'
#' @param M A square numeric matrix.
#' @param sre_type Character.  \code{"car"} or \code{"sar"} -- the model
#'   class the matrix is intended for.
#' @param verbose Logical.  When \code{TRUE} (default), prints a formatted
#'   diagnostic report.
#'
#' @return Invisibly, an object of class \code{hbsaems_spatial_check} with
#'   components:
#'   \describe{
#'     \item{\code{is_square}}{Logical.}
#'     \item{\code{has_zero_diag}}{Logical.}
#'     \item{\code{is_symmetric}}{Logical.}
#'     \item{\code{detected_style}}{Character: \code{"B"}, \code{"W"},
#'       or \code{"other"}.}
#'     \item{\code{n_isolated}}{Integer: number of areas with no neighbours.}
#'     \item{\code{n_components}}{Integer: number of connected components.}
#'     \item{\code{issues}}{Character vector of fatal errors.}
#'     \item{\code{warnings}}{Character vector of soft warnings.}
#'     \item{\code{compatible}}{Logical: TRUE if matrix is theoretically
#'       compatible with \code{sre_type}.}
#'   }
#'
#' @details
#' \subsection{Theoretical requirements}{
#' For a \strong{CAR} model (Besag 1974), the joint distribution
#' \deqn{u \sim \mathcal{N}\bigl(0,\, \sigma^2 (D - \rho W)^{-1}\bigr)}
#' is well-defined only when \eqn{W} is \strong{symmetric} with
#' \strong{zero diagonal}.  By convention, \eqn{W} is taken as the
#' \strong{binary adjacency matrix} (\code{style = "B"}) so that
#' \eqn{D = \mathrm{diag}(\text{row sums})} has integer entries.
#'
#' For a \strong{SAR} model (Whittle 1954, Anselin 1988),
#' \deqn{u = \rho W u + \varepsilon, \quad \varepsilon \sim \mathcal{N}(0, \sigma^2 I)}
#' and \eqn{W} is conventionally \strong{row-standardised}
#' (\code{style = "W"}) so that the spatial autoregressive parameter
#' \eqn{\rho} can be interpreted as a normalised correlation in
#' \eqn{(-1, 1)}.  Symmetry is \emph{not} required for SAR.
#' }
#'
#' \subsection{Style detection heuristic}{
#' \itemize{
#'   \item \code{"B"} (binary): all values in \{0, 1\}.
#'   \item \code{"W"} (row-standardised): every non-zero row sums to 1.
#'   \item \code{"other"}: neither of the above.
#' }
#' }
#'
#' @examples
#' # Build a small valid CAR matrix
#' M <- matrix(c(0, 1, 1, 0,
#'               1, 0, 0, 1,
#'               1, 0, 0, 1,
#'               0, 1, 1, 0), 4, 4)
#' check_spatial_weight(M, sre_type = "car")
#'
#' # An asymmetric matrix flagged for CAR
#' M2 <- M; M2[1, 2] <- 2
#' check_spatial_weight(M2, sre_type = "car", verbose = FALSE)$issues
#'
#' @seealso \code{\link{build_spatial_weight}}, \code{\link{hbm}}
#' @export
check_spatial_weight <- function(M,
                                  sre_type = c("car", "sar"),
                                  verbose  = TRUE) {

  sre_type <- match.arg(sre_type)

  if (!is.matrix(M))
    stop("'M' must be a matrix.", call. = FALSE)
  if (!is.numeric(M))
    stop("'M' must be numeric.", call. = FALSE)

  issues   <- character(0L)
  warns    <- character(0L)

  # -- 1. Square -------------------------------------------------------------
  is_square <- nrow(M) == ncol(M)
  if (!is_square)
    issues <- c(issues, sprintf(
      "Matrix is not square (%d x %d).", nrow(M), ncol(M)
    ))

  # -- 2. Zero diagonal ------------------------------------------------------
  has_zero_diag <- if (is_square) all(diag(M) == 0) else NA
  if (isFALSE(has_zero_diag))
    issues <- c(issues, paste0(
      "Diagonal contains non-zero entries (self-loops). ",
      "Spatial weight matrices must have all zeros on the diagonal."
    ))

  # -- 3. Symmetry -----------------------------------------------------------
  is_symmetric <- if (is_square) isSymmetric(unname(M)) else NA
  if (isFALSE(is_symmetric)) {
    if (sre_type == "car")
      issues <- c(issues, paste0(
        "CAR requires a symmetric weight matrix (Besag 1974). ",
        "Detected asymmetry. Consider symmetrising via ",
        "M <- (M + t(M)) / 2 or rebuild with build_spatial_weight(...)."
      ))
    else
      warns <- c(warns,
        "SAR matrix is asymmetric (acceptable but unconventional)."
      )
  }

  # -- 4. Detected style -----------------------------------------------------
  detected_style <- if (is_square) {
    nz_rows <- rowSums(M) > 0
    if (all(M %in% c(0, 1)))                    "B"
    else if (all(abs(rowSums(M[nz_rows, , drop = FALSE]) - 1) < 1e-8))
                                                 "W"
    else                                         "other"
  } else "unknown"

  if (sre_type == "car" && detected_style == "W")
    warns <- c(warns, paste0(
      "CAR is conventionally fit on a BINARY adjacency matrix (style = 'B'), ",
      "but a row-standardised matrix was supplied. The model will still ",
      "compile but interpretation differs from standard Besag CAR."
    ))
  if (sre_type == "sar" && detected_style == "B")
    warns <- c(warns, paste0(
      "SAR is conventionally fit on a ROW-STANDARDISED matrix (style = 'W'), ",
      "but a binary matrix was supplied. The spatial autoregressive ",
      "parameter rho may not be in (-1, 1)."
    ))

  # -- 5. Connectivity (number of components) -------------------------------
  n_isolated   <- if (is_square) sum(rowSums(M) == 0) else NA
  n_components <- if (is_square) .count_components(M) else NA

  if (isTRUE(n_isolated > 0L))
    warns <- c(warns, sprintf(paste0(
      "%d area(s) have no neighbours (isolated). For ICAR these will be ",
      "non-identified. Consider BYM2 (handles disconnected components) ",
      "or remove isolated areas."), n_isolated))
  if (isTRUE(n_components > 1L))
    warns <- c(warns, sprintf(paste0(
      "Graph has %d connected components. Standard ICAR/CAR assume a ",
      "single connected graph. Use car_type = 'bym2' or refit per ",
      "component."), n_components))

  compatible <- length(issues) == 0L

  out <- structure(
    list(
      is_square      = is_square,
      has_zero_diag  = has_zero_diag,
      is_symmetric   = is_symmetric,
      detected_style = detected_style,
      n_isolated     = n_isolated,
      n_components   = n_components,
      issues         = issues,
      warnings       = warns,
      compatible     = compatible
    ),
    class = c("hbsaems_spatial_check", "hbsaems_check")
  )

  if (verbose) print(out)
  invisible(out)
}


# ---- Helper: count connected components via BFS ----------------------------
# Pure base-R, no dependency on igraph.

.count_components <- function(M) {
  n <- nrow(M)
  visited <- logical(n)
  comps   <- 0L
  for (start in seq_len(n)) {
    if (visited[start]) next
    comps <- comps + 1L
    queue <- start
    while (length(queue) > 0L) {
      v <- queue[1L]
      queue <- queue[-1L]
      if (visited[v]) next
      visited[v] <- TRUE
      neigh <- which(M[v, ] != 0 | M[, v] != 0)
      neigh <- neigh[!visited[neigh]]
      queue <- c(queue, neigh)
    }
  }
  comps
}


#' @method print hbsaems_spatial_check
#' @export
print.hbsaems_spatial_check <- function(x, ...) {
  cat("\nSpatial Weight Matrix Diagnostic\n")
  cat("---------------------------------\n")
  cat(sprintf("  Square          : %s\n",        x$is_square))
  cat(sprintf("  Zero diagonal   : %s\n",        x$has_zero_diag))
  cat(sprintf("  Symmetric       : %s\n",        x$is_symmetric))
  cat(sprintf("  Detected style  : %s\n",        x$detected_style))
  cat(sprintf("  Isolated areas  : %s\n",        x$n_isolated))
  cat(sprintf("  Components      : %s\n",        x$n_components))

  if (length(x$issues) > 0L) {
    cat("\n  ! ISSUES (matrix is incompatible):\n")
    for (i in x$issues) cat("    -", i, "\n")
  }
  if (length(x$warnings) > 0L) {
    cat("\n  ! Warnings:\n")
    for (w in x$warnings) cat("    -", w, "\n")
  }
  if (x$compatible && length(x$warnings) == 0L)
    cat("\n  Matrix is theoretically compatible.\n")
  cat("\n")
  invisible(x)
}
