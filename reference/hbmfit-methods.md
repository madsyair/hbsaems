# Standard S3 Methods for hbmfit

These methods allow `hbmfit` objects to be used with familiar base-R
generics – [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`coef()`](https://rdrr.io/r/stats/coef.html), etc. – in the same way as
`brmsfit` objects from brms.

## Arguments

- object, x:

  An `hbmfit` object.

- type:

  Plot type. See Details for available options.

- newdata:

  Optional new data frame for predictions.

- ...:

  Additional arguments passed to the underlying brms method.

## Value

Varies by method; see brms documentation for the underlying return
types.
