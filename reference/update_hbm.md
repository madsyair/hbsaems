# Update a Fitted HBM

Refits an `hbmfit` model with one or more arguments changed. Useful for
re-running with longer chains, more iterations, or new data without
retyping the full
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) call.

## Usage

``` r
update_hbm(
  object,
  newdata = NULL,
  formula. = NULL,
  iter = NULL,
  warmup = NULL,
  chains = NULL,
  cores = NULL,
  control = NULL,
  ...
)
```

## Arguments

- object:

  An `hbmfit` object.

- newdata:

  Optional replacement `data.frame`.

- formula.:

  Optional new formula (note the trailing dot, following
  [`stats::update`](https://rdrr.io/r/stats/update.html)). Pass
  `. ~ . + new_predictor` to add a term.

- iter:

  Optional new total number of iterations.

- warmup:

  Optional new warm-up length.

- chains:

  Optional new number of MCMC chains.

- cores:

  Optional new number of cores.

- control:

  Optional new control list (e.g.\\ `list(adapt_delta = 0.99)`).

- ...:

  Additional arguments forwarded to
  [`update.brmsfit`](https://paulbuerkner.com/brms/reference/update.brmsfit.html).

## Value

An updated `hbmfit` object.

## Auto-fallback for new formula terms

When you supply a new `formula.` that references variables not in the
original model frame, brms's default `update.brmsfit` refuses with *"New
variables found ...; supply data again via newdata"*. `update_hbm`
catches this specific error and automatically retries with
`newdata = object$data` (the data frame stored on the original
`hbmfit`). Pass an explicit `newdata` to override this behaviour.

## Fixed-parameter columns

Models fitted with `sampling_variance`, `n` + `deff` (in
`hbm_betalogitnorm`), or `fixed_params` carry hidden offset columns
named `.hbsaems_<par>_fixed` in their data frames. When `update_hbm`
receives a `newdata` that *does not* have these columns, brms refuses to
refit with *"variables can neither be found in 'data' nor in 'data2'"*.
`update_hbm` now detects this case and:

- warns the user when offset columns are present in the original data
  but missing in `newdata`;

- either copies the columns over from the original `object$data` (when
  `nrow(newdata) == nrow(object$data)`, which is the typical "same
  areas, updated covariates" case), or

- raises an informative error pointing the user to either supply the
  columns in `newdata` explicitly or refit from scratch with a fresh
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) call.

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | regency),    # area-level random effect
             chains = 2, iter = 1000, warmup = 500, cores = 1,
             seed = 1, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Eigen not found; call install.packages('RcppEigen')

# Re-run with more iterations (no data change needed)
model2 <- update_hbm(model, iter = 4000, warmup = 2000)
#> Error: object 'model' not found

# Add a predictor: the auto-fallback transparently retries with the
# stored data frame.  Equivalent to passing newdata = data_fhnorm.
model3 <- update_hbm(model, formula. = . ~ . + x2)
#> Error: object 'model' not found
# }
```
