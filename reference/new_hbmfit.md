# Create a New hbmfit Object

Low-level (internal) constructor. All model-fitting functions must use
this constructor rather than calling
[`structure()`](https://rdrr.io/r/base/structure.html) directly, so the
class invariants are enforced in one place.

## Usage

``` r
new_hbmfit(model, missing_method = NULL, data)
```

## Arguments

- model:

  A `brmsfit` or `brmsfit_multiple` object.

- missing_method:

  Character scalar or `NULL`.

- data:

  The original `data.frame`.

## Value

An object of class `"hbmfit"`.

## Details

Together with
[`validate_hbmfit`](https://madsyair.github.io/hbsaems/reference/validate_hbmfit.md)
and [`hbmfit`](https://madsyair.github.io/hbsaems/reference/hbmfit.md),
this follows the trio-constructor pattern recommended in Hadley
Wickham's *Advanced R* (chapter 13): `new_hbmfit()` is fast and minimal,
[`validate_hbmfit()`](https://madsyair.github.io/hbsaems/reference/validate_hbmfit.md)
is slow and thorough, and
[`hbmfit()`](https://madsyair.github.io/hbsaems/reference/hbmfit.md) is
the user-facing helper.
