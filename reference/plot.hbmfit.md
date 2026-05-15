# Plot a Fitted hbmfit Object

Wraps
[`brms::mcmc_plot()`](https://paulbuerkner.com/brms/reference/mcmc_plot.brmsfit.html)
and
[`brms::pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
with named plot types.

## Usage

``` r
# S3 method for class 'hbmfit'
plot(
  x,
  type = c("trace", "density", "acf", "nuts_energy", "rhat", "neff", "pp_check"),
  ...
)
```

## Arguments

- x:

  An `hbmfit` object.

- type:

  Plot type, one of: `"trace"` (trace plots), `"density"` (posterior
  densities), `"acf"` (autocorrelation), `"nuts_energy"` (NUTS energy),
  `"rhat"` (R-hat distribution), `"neff"` (effective sample size),
  `"pp_check"` (posterior predictive check).

- ...:

  Additional arguments passed to the underlying brms plotting function.

## Value

A `ggplot` or `bayesplot` object.
