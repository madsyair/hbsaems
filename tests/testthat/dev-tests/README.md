# Development Tests (`tests/testthat/dev-tests/`)

Heavy integration tests for `hbsaems`.  Each file fits real Stan models
through `brms` and takes anywhere from 5 to 60 seconds.  They are **not**
run on CRAN, and the entire folder is **excluded from the package
tarball** via `.Rbuildignore` (`^tests/testthat/dev-tests$`).

These tests live in source control so that maintainers can verify the
end-to-end behaviour of the modelling functions before each release,
without paying the price during routine `R CMD check`.

## Running the development tests

There are three ways to run them.

### 1. From R, in the package source directory

```r
Sys.setenv(NOT_CRAN = "true")
testthat::test_dir("tests/testthat/dev-tests")
```

### 2. From the shell, after installing the package

```bash
NOT_CRAN=true R -e 'testthat::test_dir("tests/testthat/dev-tests")'
```

### 3. With `devtools`

```r
devtools::load_all()
devtools::test(filter = "dev-tests")
```

Tests are gated by `helper-dev-setup.R::.dev_skip()`, which:

1. Calls `testthat::skip_on_cran()` -- guarantees skip on CRAN even if
   someone bundles the folder by mistake.
2. Additionally requires `Sys.getenv("NOT_CRAN") == "true"` or an
   interactive session, so they do not run during `R CMD check` either.

## File inventory

| File                         | Lines | Functions covered                              |
|------------------------------|------:|------------------------------------------------|
| `test-hbm.R`                 |   406 | `hbm()` -- formula validation, priors, RE, spatial, missing data |
| `test-hbm-lnln.R`            |   448 | `hbm_lnln()` -- lognormal-lognormal end-to-end |
| `test-hbm-binlogitnorm.R`    |   434 | `hbm_binlogitnorm()` -- binomial + logit       |
| `test-hbm-betalogitnorm.R`   |   453 | `hbm_betalogitnorm()` -- Beta + auxiliary phi  |
| `test-hbcc.R`                |    43 | `convergence_check()` -- post-fit diagnostics  |
| `test-hbmc.R`                |    83 | `model_compare()` -- LOO / WAIC / BF, reloo    |
| `test-hbpc.R`                |   155 | `prior_check()` -- prior predictive            |
| `test-hbsae.R`               |    98 | `sae_predict()` -- predictions, newdata        |
| `test-update_hbm.R`          |    84 | `update_hbm()` -- formula / data updates       |
| `test-run_sae_app.R`         |    45 | `run_sae_app()` -- Shiny launcher              |

## Why a separate folder?

CRAN imposes a hard 10-minute budget for `R CMD check` and discourages
tests that compile Stan models.  By isolating these heavy tests into
`dev-tests/`:

* `R CMD check` finishes in well under 60 seconds locally.
* The CRAN tarball stays small.
* Developers who want full integration coverage can opt in with one
  environment variable.

## Datasets used

The dev-tests use the package's built-in datasets (already bundled in
`data/`):

* `data_fhnorm` -- Fay-Herriot Normal
* `data_binlogitnorm` -- Binomial Logit-Normal
* `data_betalogitnorm` -- Beta Logit-Normal
* `data_lnln` -- Lognormal-Lognormal
* `adjacency_matrix_car` -- adjacency matrix for CAR tests
* `spatial_weight_sar` -- weight matrix for SAR tests

No additional data files live in this folder.

## Adding a new dev-test

1. Create `test-<feature>.R` in this folder.
2. At the top of each `test_that()` block, call `.dev_skip()`.
3. Keep Stan settings light:  `iter = 100, chains = 2, warmup = 50`.
4. Wrap fit calls in `suppressWarnings()` to silence brms's expected
   "small ESS" notes when iter is intentionally low.
