# R/spatial-weight.R
# =============================================================================
# Construct spatial weight / adjacency matrices for use as the `M` argument
# of hbm() and the distribution-specific wrappers.
#
# Both `sf` (for shapefile I/O) and `spdep` (for neighbour computation) are
# in Suggests and accessed through requireNamespace() guards.
#
# v0.4.0+: Added theoretical guidance via the `for_model` argument that
# auto-selects sensible defaults for the chosen model class (CAR vs SAR).
# =============================================================================


#' Build a Spatial Weight or Adjacency Matrix from Shapefile
#'
#' @title build_spatial_weight: Construct M for CAR / SAR models
#' @description
#' Constructs a spatial weight / adjacency matrix \eqn{M} suitable for use
#' as the \code{M} argument of \code{\link{hbm}}.  The function accepts
#' either a path to a shapefile (\code{.shp}) or an \code{sf} /
#' \code{Spatial*} object already in memory, and returns a square numeric
#' matrix that is automatically validated against the theoretical
#' requirements of the target model class via
#' \code{\link{check_spatial_weight}}.
#'
#' @details
#' \subsection{Choosing type and style for your SAE model}{
#'
#' The combination of \code{type} (neighbour definition) and \code{style}
#' (matrix coding) determines whether the resulting matrix is
#' theoretically appropriate for a CAR or SAR model.  The recommended
#' combinations are:
#'
#' \tabular{llll}{
#'   \strong{Model}    \tab \strong{type}      \tab \strong{style} \tab \strong{Reference} \cr
#'   CAR / ICAR / BYM2 \tab queen or rook      \tab B              \tab Besag (1974, 1991), Riebler et al. (2016) \cr
#'   SAR (lag/error)   \tab knn or distance    \tab W              \tab Anselin (1988), Whittle (1954)            \cr
#'   CAR (irregular)   \tab knn or distance    \tab B              \tab kNN auto-symmetrised when \code{style="B"} \cr
#' }
#'
#' Use the convenience wrapper \code{for_model = "car"} or
#' \code{for_model = "sar"} to set both \code{type} and \code{style}
#' automatically from the recommendation above.  When \code{type} or
#' \code{style} is also supplied, the explicit argument wins (the helper
#' only fills in the missing one).
#' }
#'
#' \subsection{Mathematical contracts}{
#' \describe{
#'   \item{CAR}{
#'     \eqn{M} must be \strong{binary}, \strong{symmetric}, and have
#'     \strong{zero diagonal}.  The full distribution is
#'     \eqn{u \sim \mathcal{N}\bigl(0, \sigma^2 (D - \rho W)^{-1}\bigr)}
#'     with \eqn{D = \mathrm{diag}(W \mathbf{1})}.
#'   }
#'   \item{SAR}{
#'     \eqn{M} should be \strong{row-standardised} so that \eqn{\rho}
#'     lies in \eqn{(-1, 1)}.  Symmetry is not required.
#'   }
#' }
#' All matrices are post-checked with \code{\link{check_spatial_weight}}
#' and offending matrices are flagged.
#' }
#'
#' @param shp Either a character path to a \code{.shp} file, an
#'   \code{sf} object, or a \code{Spatial*} (\pkg{sp}) object.
#' @param for_model Optional convenience argument: \code{"car"} or
#'   \code{"sar"}.  When supplied, sets sensible defaults for
#'   \code{type} and \code{style} as documented above.  When the
#'   user also supplies \code{type} or \code{style} explicitly, those
#'   take precedence.
#' @param type Character.  Neighbour definition:
#'   \describe{
#'     \item{\code{"queen"}}{Polygons sharing at least one vertex.
#'       Symmetric.}
#'     \item{\code{"rook"}}{Polygons sharing an edge.  Symmetric.}
#'     \item{\code{"knn"}}{\code{k} nearest centroids.  Asymmetric in
#'       general; auto-symmetrised when \code{style = "B"}.}
#'     \item{\code{"distance"}}{Centroids within \code{threshold}.
#'       Symmetric.}
#'   }
#' @param style Character.  \code{"B"} (binary, suitable for CAR) or
#'   \code{"W"} (row-standardised, suitable for SAR).
#' @param k Integer.  Number of nearest neighbours when
#'   \code{type = "knn"} (default \code{4}).
#' @param threshold Numeric.  Distance threshold (in CRS units) when
#'   \code{type = "distance"}.  When \code{NULL}, the maximum
#'   first-nearest-neighbour distance is used.
#' @param id_col Optional character.  Column name in the spatial
#'   object whose values become the matrix \code{dimnames}.
#' @param longlat Logical.  Whether coordinates are in longitude/latitude.
#' @param validate Logical.  Whether to run
#'   \code{\link{check_spatial_weight}} after construction (default
#'   \code{TRUE}).  Set to \code{FALSE} to skip diagnostics.
#'
#' @return A square numeric matrix with row/column names taken from
#'   \code{id_col} (if supplied) or sequential integers, plus the
#'   following attributes: \code{"hbsae_type"}, \code{"hbsae_style"},
#'   \code{"hbsae_for_model"}, \code{"hbsae_check"} (the result of
#'   \code{check_spatial_weight}).
#'
#' @section Reproducibility note:
#' KNN graphs depend on the order in which ties are broken, so when
#' several centroids are exactly equidistant the resulting matrix may
#' depend on feature ordering.
#'
#' @examples
#' \dontrun{
#' # Recommended for CAR: queen contiguity + binary style
#' M_car <- build_spatial_weight(
#'   shp        = "areas.shp",
#'   for_model  = "car",      # implies type = "queen", style = "B"
#'   id_col     = "area_id"
#' )
#'
#' # Recommended for SAR: 4 nearest neighbours + row-standardised
#' M_sar <- build_spatial_weight(
#'   shp        = "areas.shp",
#'   for_model  = "sar",      # implies type = "knn", style = "W"
#'   k          = 4
#' )
#'
#' # Explicit override
#' M_explicit <- build_spatial_weight(
#'   shp   = my_sf,
#'   type  = "rook",
#'   style = "B"
#' )
#'
#' # Pass to hbm()
#' fit <- hbm(brms::bf(y ~ x1 + x2),
#'            data = my_data,
#'            sre = "area_id",
#'            sre_type = "car",
#'            M = M_car)
#' }
#'
#' @references
#' Anselin, L. (1988). \emph{Spatial Econometrics: Methods and Models}.
#' Kluwer Academic Publishers.
#'
#' Besag, J. (1974). Spatial interaction and the statistical analysis of
#' lattice systems (with discussion). \emph{JRSS B} 36(2), 192--236.
#'
#' Bivand, R. S., Pebesma, E., & Gomez-Rubio, V. (2013). \emph{Applied
#' Spatial Data Analysis with R} (2nd ed.). Springer.
#'
#' @seealso \code{\link{check_spatial_weight}}, \code{\link{hbm}},
#'   \code{\link{adjacency_matrix_car}}, \code{\link{spatial_weight_sar}}
#'
#' @export
build_spatial_weight <- function(shp,
                                 for_model = NULL,
                                 type      = NULL,
                                 style     = NULL,
                                 k         = 4L,
                                 threshold = NULL,
                                 id_col    = NULL,
                                 longlat   = FALSE,
                                 validate  = TRUE) {

  # -- 1. Resolve type / style from for_model when not given ----------------
  if (!is.null(for_model)) {
    for_model <- match.arg(for_model, c("car", "sar"))
    if (is.null(type))
      type  <- if (for_model == "car") "queen" else "knn"
    if (is.null(style))
      style <- if (for_model == "car") "B"     else "W"
  }
  if (is.null(type))  type  <- "queen"
  if (is.null(style)) style <- "B"

  type  <- match.arg(type,  c("queen", "rook", "knn", "distance"))
  style <- match.arg(style, c("B", "W"))

  # -- 2. Dependency check ---------------------------------------------------
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required by build_spatial_weight().\n",
         "  Install with: install.packages('sf')",
         call. = FALSE)
  if (!requireNamespace("spdep", quietly = TRUE))
    stop("Package 'spdep' is required by build_spatial_weight().\n",
         "  Install with: install.packages('spdep')",
         call. = FALSE)

  # -- 3. Coerce input to sf -------------------------------------------------
  obj <- if (is.character(shp) && length(shp) == 1L) {
    if (!file.exists(shp))
      stop("Shapefile not found: ", shp, call. = FALSE)
    sf::st_read(shp, quiet = TRUE)
  } else if (inherits(shp, "sf")) {
    shp
  } else if (inherits(shp, "Spatial")) {
    sf::st_as_sf(shp)
  } else {
    stop("'shp' must be a path to a .shp file, an sf object, or a ",
         "Spatial* object.", call. = FALSE)
  }

  n <- nrow(obj)
  if (n < 2L)
    stop("Need at least 2 features; got ", n, ".", call. = FALSE)

  # -- 4. Validate id_col ----------------------------------------------------
  ids <- if (!is.null(id_col)) {
    if (!(id_col %in% names(obj)))
      stop("id_col '", id_col, "' not found in spatial object.",
           call. = FALSE)
    as.character(obj[[id_col]])
  } else {
    as.character(seq_len(n))
  }

  # -- 5. Build neighbour list ----------------------------------------------
  nb <- switch(
    type,
    queen = spdep::poly2nb(obj, queen = TRUE),
    rook  = spdep::poly2nb(obj, queen = FALSE),
    knn   = {
      coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(obj)))
      kn <- spdep::knearneigh(coords, k = k, longlat = longlat)
      # Auto-symmetrise for CAR (style "B"); leave asymmetric for SAR.
      spdep::knn2nb(kn, sym = (style == "B"))
    },
    distance = {
      coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(obj)))
      d <- if (is.null(threshold)) {
        kn1 <- spdep::knearneigh(coords, k = 1L, longlat = longlat)
        max(spdep::nbdists(spdep::knn2nb(kn1), coords,
                            longlat = longlat)[[1]]) * 1.0001
      } else threshold
      spdep::dnearneigh(coords, d1 = 0, d2 = d, longlat = longlat)
    }
  )

  # -- 6. Convert to matrix --------------------------------------------------
  M <- tryCatch(
    spdep::nb2mat(nb, style = style, zero.policy = TRUE),
    error = function(e) {
      stop("Could not convert neighbour list to matrix: ", e$message,
           call. = FALSE)
    }
  )
  rownames(M) <- ids
  colnames(M) <- ids

  # -- 7. Annotate with metadata --------------------------------------------
  attr(M, "hbsae_type")      <- type
  attr(M, "hbsae_style")     <- style
  attr(M, "hbsae_for_model") <- for_model

  # -- 8. Run theoretical validation ----------------------------------------
  if (isTRUE(validate)) {
    target_model <- for_model %||% (if (style == "B") "car" else "sar")
    chk <- check_spatial_weight(M, sre_type = target_model,
                                  verbose = FALSE)
    attr(M, "hbsae_check") <- chk

    # Surface critical issues to the user
    for (msg in chk$warnings)
      warning(msg, call. = FALSE, immediate. = TRUE)
    if (!chk$compatible)
      stop("Constructed matrix fails theoretical compatibility for ",
           target_model, ":\n  ",
           paste(chk$issues, collapse = "\n  "),
           call. = FALSE)
  }

  M
}
