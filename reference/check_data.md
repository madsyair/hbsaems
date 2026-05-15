# Inspect Data Before Fitting an HBSAE Model

Performs three independent checks on the supplied dataset and returns a
structured `hbsaems_data_check` object that summarises the results. This
function is intended to be called **before**
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) or any of
the distribution-specific wrappers.

## Usage

``` r
check_data(
  data,
  response,
  predictors,
  group = NULL,
  sre = NULL,
  M = NULL,
  trials = NULL,
  n_var = NULL
)
```

## Arguments

- data:

  A `data.frame`.

- response:

  Character. Name of the response variable in `data`.

- predictors:

  Character vector. Names of the predictor (auxiliary) variables.

- group:

  Optional character. Name of the random-effect grouping variable.

- sre:

  Optional character. Name of the spatial-grouping variable.

- M:

  Optional spatial weight matrix to dimension-check against `data`.

- trials:

  Optional character. Name of the trials variable (binomial models).

- n_var:

  Optional character. Name of the sample-size variable (beta / lognormal
  direct-estimator models).

## Value

An object of class `hbsaems_data_check` with components:

- `n_obs`:

  Number of rows in the data.

- `missing_summary`:

  Named integer vector: per-variable count of `NA`.

- `missing_pattern`:

  Character: `"none"`, `"y_only"`, `"x_only"`, or `"both"`.

- `dimension_check`:

  Named list of dimension diagnostics.

- `non_sample_warning`:

  Character or `NULL` – a hint to investigate whether NA-Y rows are
  non-sample (out-of-sample) areas.

- `recommended_method`:

  Character: suggested `handle_missing` value (`"deleted"`,
  `"multiple"`, `"model"`, or `NA` when no missing values are present).

- `recommendation_text`:

  Human-readable rationale.

- `issues`:

  Character vector of fatal errors (length 0 if OK).

## Details

### 1. Variable presence

Verifies that `response`, every name in `predictors`, and the optional
`group`/`sre`/`trials`/`n_var` columns exist in `data`. Missing
variables are reported in `$issues`.

### 2. Missing-value pattern

The pattern is one of:

- `"none"`:

  All listed columns are complete.

- `"y_only"`:

  Only the response has NAs.

- `"x_only"`:

  Only the predictors have NAs.

- `"both"`:

  Both Y and X have NAs.

Based on the pattern, a strategy is recommended:

- `"y_only"`:

  **First, check whether the NA-Y rows are non-sample (out-of-sample)
  areas** – domains for which a prediction is desired but no direct
  estimate exists. If yes, do *not* delete them; fit on the complete-Y
  subset and pass the NA-Y rows to
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
  via the `newdata` argument. If they are merely missing observations
  within sampled areas, use `handle_missing = "deleted"`.

- `"x_only"`:

  `handle_missing = "multiple"` – multiple imputation via mice.

- `"both"` (continuous outcome):

  `handle_missing = "model"` – joint Bayesian imputation via
  [`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html).

- `"both"` (discrete outcome, binomial):

  `handle_missing = "multiple"`.

### 3. Dimension check

When `M` is supplied, verifies that it is square and that `nrow(M)`
matches the number of *distinct* levels in `data[[sre]]` (or
`nrow(data)` when `sre` is `NULL`).

## See also

[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md),
[`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)

## Examples

``` r
data("data_fhnorm")

# 1. Complete data -> no warnings, no recommendation
chk <- check_data(data_fhnorm,
                  response   = "y",
                  predictors = c("x1", "x2", "x3"))
print(chk)
#> 
#> HBSAE Data Check  [hbsaems_data_check]
#> ---------------------------------------
#>  Observations    : 100 
#>  Missing pattern : none (data complete) 
#>  Issues          : none
#> 
#>  - No missing values detected. handle_missing can stay NULL. 
#> 
#> Use summary() for full details.
#> 

# 2. Missing-Y pattern -> recommends checking for non-sample areas
d <- data_fhnorm
d$y[1:5] <- NA
chk2 <- check_data(d, response = "y",
                      predictors = c("x1", "x2", "x3"))
summary(chk2)
#> 
#> ===== HBSAE Data Check Summary =====
#> 
#> Observations: 100 
#> 
#> Missing values per variable:
#>  Variable NA_count    Pct
#>         y        5   5.0%
#>        x1        0   0.0%
#>        x2        0   0.0%
#>        x3        0   0.0%
#> 
#> Non-sample-area note:
#> Response 'y' has 5 missing value(s) (5.0% of rows).
#>   IMPORTANT: Inspect these rows -- are they NON-SAMPLE areas
#>   (out-of-sample domains for which you want PREDICTIONS rather
#>   than estimates)?
#>     * If YES (non-sample): keep these rows in the data, fit the
#>       model on the complete-Y subset, and use sae_predict() with
#>       newdata = <NA-Y rows> to obtain area-level predictions.
#>     * If NO  (truly missing within sampled areas): use
#>       handle_missing = 'deleted' (or 'model' for continuous Y
#>       and 'multiple' for discrete Y).
#> 
#> Recommendation:
#>   handle_missing =‘deleted’
#>   Only the response is missing. Recommended: handle_missing = 'deleted'. BUT first verify whether the NA-Y rows are non-sample
#>   areas (see non_sample_warning).

# 3. Missing-X-only -> recommends multiple imputation
d2 <- data_fhnorm
d2$x1[10:15] <- NA
chk3 <- check_data(d2, response = "y",
                      predictors = c("x1", "x2", "x3"))
chk3$recommended_method
#> [1] "multiple"

# 4. Spatial dimension check
data("adjacency_matrix_car")
chk4 <- check_data(data_fhnorm[1:5, ],
                   response   = "y",
                   predictors = c("x1", "x2", "x3"),
                   M          = adjacency_matrix_car)
chk4$dimension_check
#> $n_areas_data
#> [1] 5
#> 
#> $n_areas_M
#> [1] 5
#> 
#> $M_is_square
#> [1] TRUE
#> 
#> $dim_match
#> [1] TRUE
#> 
```
