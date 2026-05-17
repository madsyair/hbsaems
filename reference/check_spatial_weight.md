# Validate a Spatial Weight Matrix Against CAR/SAR Theory

Runs five theoretical compatibility checks on a spatial weight matrix
\\M\\ and reports any deviations from the standard requirements of the
chosen model class. Returns a structured object summarising the results.

## Usage

``` r
check_spatial_weight(
  M,
  spatial_model = c("car", "sar"),
  verbose = TRUE,
  sre_type = NULL
)
```

## Arguments

- M:

  A square numeric matrix.

- spatial_model:

  Character. `"car"` or `"sar"` – the model class the matrix is intended
  for.

- verbose:

  Logical. When `TRUE` (default), prints a formatted diagnostic report.

- sre_type:

  **Deprecated.** Use `spatial_model` instead. Kept for backward
  compatibility; will be removed in v2.0.0.

## Value

Invisibly, an object of class `hbsaems_spatial_check` with components:

- `is_square`:

  Logical.

- `has_zero_diag`:

  Logical.

- `is_symmetric`:

  Logical.

- `detected_style`:

  Character: `"B"`, `"W"`, or `"other"`.

- `n_isolated`:

  Integer: number of areas with no neighbours.

- `n_components`:

  Integer: number of connected components.

- `issues`:

  Character vector of fatal errors.

- `warnings`:

  Character vector of soft warnings.

- `compatible`:

  Logical: TRUE if matrix is theoretically compatible with
  `spatial_model`.

## Details

### Theoretical requirements

For a **CAR** model (Besag 1974), the joint distribution \$\$u \sim
\mathcal{N}\bigl(0,\\ \sigma^2 (D - \rho W)^{-1}\bigr)\$\$ is
well-defined only when \\W\\ is **symmetric** with **zero diagonal**. By
convention, \\W\\ is taken as the **binary adjacency matrix**
(`style = "B"`) so that \\D = \mathrm{diag}(\text{row sums})\\ has
integer entries.

For a **SAR** model (Whittle 1954, Anselin 1988), \$\$u = \rho W u +
\varepsilon, \quad \varepsilon \sim \mathcal{N}(0, \sigma^2 I)\$\$ and
\\W\\ is conventionally **row-standardised** (`style = "W"`) so that the
spatial autoregressive parameter \\\rho\\ can be interpreted as a
normalised correlation in \\(-1, 1)\\. Symmetry is *not* required for
SAR.

### Style detection heuristic

- `"B"` (binary): all values in {0, 1}.

- `"W"` (row-standardised): every non-zero row sums to 1.

- `"other"`: neither of the above.

## See also

[`build_spatial_weight`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# Build a small valid CAR matrix
M <- matrix(c(0, 1, 1, 0,
              1, 0, 0, 1,
              1, 0, 0, 1,
              0, 1, 1, 0), 4, 4)
check_spatial_weight(M, spatial_model = "car")
#> 
#> Spatial Weight Matrix Diagnostic
#> ---------------------------------
#>   Square          : TRUE
#>   Zero diagonal   : TRUE
#>   Symmetric       : TRUE
#>   Detected style  : B
#>   Isolated areas  : 0
#>   Components      : 1
#> 
#>   Matrix is theoretically compatible.
#> 

# An asymmetric matrix flagged for CAR
M2 <- M; M2[1, 2] <- 2
check_spatial_weight(M2, spatial_model = "car", verbose = FALSE)$issues
#> [1] "CAR requires a symmetric weight matrix (Besag 1974). Detected asymmetry. Consider symmetrising via M <- (M + t(M)) / 2 or rebuild with build_spatial_weight(...)."
```
