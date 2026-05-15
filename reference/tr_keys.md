# List All Translation Keys (for a Reference Language)

Useful when adding a new language: enumerates every key that needs a
translation.

## Usage

``` r
tr_keys(lang = "en")
```

## Arguments

- lang:

  Character. Reference language (default `"en"`).

## Value

Sorted character vector of translation keys.

## Examples

``` r
head(tr_keys())
#> [1] "app_subtitle"     "app_title"        "box_benchmark"    "box_convergence" 
#> [5] "box_data_check"   "box_data_preview"
```
