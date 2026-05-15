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
`"hbsae_type"`, `"hbsae_style"`, `"hbsae_for_model"`, `"hbsae_check"`
(the result of `check_spatial_weight`).

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
if (FALSE) { # \dontrun{
# Recommended for CAR: queen contiguity + binary style
M_car <- build_spatial_weight(
  shp        = "areas.shp",
  for_model  = "car",      # implies type = "queen", style = "B"
  id_col     = "area_id"
)

# Recommended for SAR: 4 nearest neighbours + row-standardised
M_sar <- build_spatial_weight(
  shp        = "areas.shp",
  for_model  = "sar",      # implies type = "knn", style = "W"
  k          = 4
)

# Explicit override
M_explicit <- build_spatial_weight(
  shp   = my_sf,
  type  = "rook",
  style = "B"
)

# Pass to hbm()
fit <- hbm(brms::bf(y ~ x1 + x2),
           data = my_data,
           sre = "area_id",
           sre_type = "car",
           M = M_car)
} # }
```
