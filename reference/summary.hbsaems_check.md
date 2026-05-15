# Generic Summary Method for hbsaems Check Results

Provides a fall-back summary that simply calls the underlying
[`print()`](https://rdrr.io/r/base/print.html) method. Subclasses that
need a more detailed summary (e.g.\\ `summary.hbsaems_data_check`)
override this.

## Usage

``` r
# S3 method for class 'hbsaems_check'
summary(object, ...)
```

## Arguments

- object:

  An object inheriting from `"hbsaems_check"`.

- ...:

  Unused.

## Value

The input `object`, invisibly.
