# build_spatial_weight: Construct M for CAR / SAR models

Constructs a spatial weight / adjacency matrix \\M\\ suitable for use as
the `M` argument of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md). The
function accepts either a path to a shapefile (`.shp`) or an `sf` /
`Spatial*` object already in memory, and returns a square numeric matrix
that is automatically validated against the theoretical requirements of
the target model class via
[`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md).

## Usage

``` r
build_spatial_weight(
  shp,
  for_model = NULL,
  type = NULL,
  style = NULL,
  k = 4L,
  threshold = NULL,
  id_col = NULL,
  longlat = FALSE,
  validate = TRUE
)
```

## Arguments

- shp:

  Either a character path to a `.shp` file, an `sf` object, or a
  `Spatial*` (sp) object.

- for_model:

  Optional convenience argument: `"car"` or `"sar"`. When supplied, sets
  sensible defaults for `type` and `style` as documented above. When the
  user also supplies `type` or `style` explicitly, those take
  precedence.

- type:

  Character. Neighbour definition:

  `"queen"`

  :   Polygons sharing at least one vertex. Symmetric.

  `"rook"`

  :   Polygons sharing an edge. Symmetric.

  `"knn"`

  :   `k` nearest centroids. Asymmetric in general; auto-symmetrised
      when `style = "B"`.

  `"distance"`

  :   Centroids within `threshold`. Symmetric.

- style:

  Character. `"B"` (binary, suitable for CAR) or `"W"`
  (row-standardised, suitable for SAR).

- k:

  Integer. Number of nearest neighbours when `type = "knn"` (default
  `4`).

- threshold:

  Numeric. Distance threshold (in CRS units) when `type = "distance"`.
  When `NULL`, the maximum first-nearest-neighbour distance is used.

- id_col:

  Optional character. Column name in the spatial object whose values
  become the matrix `dimnames`.

- longlat:

  Logical. Whether coordinates are in longitude/latitude.

- validate:

  Logical. Whether to run
  [`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md)
  after construction (default `TRUE`). Set to `FALSE` to skip
  diagnostics.

## Value

A square numeric matrix with row/column names taken from `id_col` (if
supplied) or sequential integers, plus the following attributes:
`"hbsaems_type"`, `"hbsaems_style"`, `"hbsaems_for_model"`,
`"hbsaems_check"` (the result of `check_spatial_weight`).

## Details

Build a Spatial Weight or Adjacency Matrix from Shapefile

### Choosing type and style for your SAE model

The combination of `type` (neighbour definition) and `style` (matrix
coding) determines whether the resulting matrix is theoretically
appropriate for a CAR or SAR model. The recommended combinations are:

|  |  |  |  |
|----|----|----|----|
| **Model** | **type** | **style** | **Reference** |
| CAR / ICAR / BYM2 | queen or rook | B | Besag (1974, 1991), Riebler et al. (2016) |
| SAR (lag/error) | knn or distance | W | Anselin (1988), Whittle (1954) |
| CAR (irregular) | knn or distance | B | kNN auto-symmetrised when `style="B"` |

Use the convenience wrapper `for_model = "car"` or `for_model = "sar"`
to set both `type` and `style` automatically from the recommendation
above. When `type` or `style` is also supplied, the explicit argument
wins (the helper only fills in the missing one).

### Mathematical contracts

- CAR:

  \\M\\ must be **binary**, **symmetric**, and have **zero diagonal**.
  The full distribution is \\u \sim \mathcal{N}\bigl(0, \sigma^2 (D -
  \rho W)^{-1}\bigr)\\ with \\D = \mathrm{diag}(W \mathbf{1})\\.

- SAR:

  \\M\\ should be **row-standardised** so that \\\rho\\ lies in \\(-1,
  1)\\. Symmetry is not required.

All matrices are post-checked with
[`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md)
and offending matrices are flagged.

## Reproducibility note

KNN graphs depend on the order in which ties are broken, so when several
centroids are exactly equidistant the resulting matrix may depend on
feature ordering.

## References

Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer
Academic Publishers.

Besag, J. (1974). Spatial interaction and the statistical analysis of
lattice systems (with discussion). *JRSS B* 36(2), 192–236.

Bivand, R. S., Pebesma, E., & Gomez-Rubio, V. (2013). *Applied Spatial
Data Analysis with R* (2nd ed.). Springer.

## See also

[`check_spatial_weight`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md),
[`adjacency_matrix_car`](https://madsyair.github.io/hbsaems/reference/adjacency_matrix_car.md),
[`spatial_weight_sar`](https://madsyair.github.io/hbsaems/reference/spatial_weight_sar.md)

## Examples

``` r
# Inspect a pre-built CAR adjacency matrix shipped with the package
data("adjacency_matrix_car")
dim(adjacency_matrix_car)
#> [1] 5 5
check_spatial_weight(adjacency_matrix_car, spatial_model = "car",
                      verbose = FALSE)$compatible
#> [1] TRUE

# \donttest{
# Build a CAR matrix from an sf object (requires sf + spdep)
if (requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("spdep", quietly = TRUE)) {
  library(sf)
  # A small 2x2 grid of polygons
  g <- st_sf(
    id = LETTERS[1:4],
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
      st_polygon(list(rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1)))),
      st_polygon(list(rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))))
    )
  )
  M_car <- build_spatial_weight(g, for_model = "car")
  M_sar <- build_spatial_weight(g, for_model = "sar", k = 2L)
}
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> Warning: k greater than one-third of the number of data points
# }
```
