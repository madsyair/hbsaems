# run_sae_app: Interactive Small Area Estimation Dashboard

Opens the interactive HBSAE dashboard in the default web browser. The
application provides a graphical interface for data upload, exploratory
analysis, model specification, fitting, and result visualisation,
covering all modelling functions in hbsaems.

## Usage

``` r
run_sae_app(check_deps = TRUE)
```

## Arguments

- check_deps:

  Logical. Whether to verify dependencies before launching (default
  `TRUE`).

## Value

Does not return a value; called for its side effect of launching a Shiny
server in the current R session.

## Details

Launch the HBSAE Shiny Application

The application is located in `inst/shiny/sae_app/app.R` within the
installed package. It depends on several packages that are listed in
`Suggests` (rather than `Imports`) so they are not required for users
who only need the modelling functions.

Two classes of dependencies are checked at launch:

- **Critical**:

  Packages without which the app cannot start (shinydashboard, DT).
  Missing critical packages raise an error.

- **Optional**:

  Packages that enable individual panels and features (shinyWidgets,
  readxl, energy, minerva, sf, spdep, bridgesampling). Missing optional
  packages produce a warning and an in-app banner; the corresponding
  feature degrades gracefully.

Use
[`check_shiny_deps()`](https://madsyair.github.io/hbsaems/reference/check_shiny_deps.md)
to inspect dependency status without launching the app.

## See also

[`check_shiny_deps`](https://madsyair.github.io/hbsaems/reference/check_shiny_deps.md)

## Examples

``` r
if (interactive()) {
  run_sae_app()
}
```
