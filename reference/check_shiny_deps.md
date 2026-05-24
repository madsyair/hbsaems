# Check Shiny App Dependencies

Classifies and inspects the optional packages used by the Shiny
dashboard. The dashboard is launched by
[`run_sae_app`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)
and the dependencies it touches live in `Suggests`, not `Imports`, so
users who only use the modelling functions never have to install them.

## Usage

``` r
check_shiny_deps(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. When `TRUE` (default), prints a formatted summary of which
  dependencies are installed and which are missing.

## Value

Invisibly, a named list with components:

- `critical_missing`:

  Character vector of critical packages not installed. When non-empty,
  the app cannot start.

- `optional_missing`:

  Character vector of optional packages not installed. When non-empty,
  the app starts but some panels degrade.

- `install_cmd`:

  A ready-to-paste
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  call covering all missing packages, or `NULL` when none are missing.

## See also

[`run_sae_app`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md)

## Examples

``` r
check_shiny_deps()
#> 
#> hbsaems Shiny App Dependency Check
#> ====================================
#> 
#> CRITICAL (required to launch the app):
#>   [OK ] shinydashboard  -- Dashboard layout (every UI tab)
#>   [OK ] DT              -- Data tables (preview, predictions, benchmarking)
#> 
#> OPTIONAL (used by individual panels):
#>   [OK ] shinyWidgets    -- Picker inputs in Data Exploration and Diagnostics tabs (falls back to selectInput)
#>   [OK ] readxl          -- Reading .xls / .xlsx uploads (falls back to .csv only)
#>   [OK ] energy          -- Distance correlation in scatter plot (falls back to Pearson / Spearman)
#>   [OK ] minerva         -- MIC correlation in scatter plot (falls back to Pearson / Spearman)
#>   [OK ] sf              -- Build spatial weight matrix from shapefile (falls back to manual matrix upload)
#>   [OK ] spdep           -- Neighbour computation for spatial weights (paired with sf)
#>   [OK ] bridgesampling  -- Bayes factor in model_compare (LOO and WAIC remain available)
#> 
#>    All dependencies installed.  Ready to launch.
#> 
```
