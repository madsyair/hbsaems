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

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | group),    # area-level random effect
             chains = 2, iter = 1000, warmup = 500, cores = 1,
             seed = 1, refresh = 0)
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')

# Re-run with more iterations
model2 <- update_hbm(model, iter = 4000, warmup = 2000)
#> Error: object 'model' not found

# Add a predictor
model3 <- update_hbm(model, formula. = . ~ . + x2)
#> Error: object 'model' not found
# }
```
