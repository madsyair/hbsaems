# tests/testthat/dev-tests/test-hbpc.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

pdf(NULL)
on.exit(dev.off(), add = TRUE)

sample_data <- data_fhnorm 
fit <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2), data = sample_data))

# hbpc
test_that("hbpc stops when automatically determined response variable not found in data", {
  .dev_skip()
  
  fit_valid <- suppressWarnings(
    hbm(
      formula = brms::bf(y ~ x1 + x2),
      data = sample_data,
      sample_prior = "only",
      prior = c(
        brms::prior("normal(0, 5)", class = "b"),
        brms::prior("normal(0, 5)", class = "Intercept")
      ),
      iter = 500,
      chains = 2
    )
  )
  
  # Hilangkan kolom 'y' dari data
  data_no_y <- sample_data
  data_no_y$y <- NULL
  
  expect_error(
    hbpc(model = fit_valid, data = data_no_y),
    "Automatically determined response variable 'y' not found in the provided data"
  )
})


# === Migrated back from main tests (calls brms::brm directly) ===
test_that("hbpc handles all-NA y values for prior predictive check", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  library(ggplot2)
  
  # Data dummy dengan semua nilai y = NA
  data_na_y <- data.frame(
    y = NA_real_,
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  
  # Fit model dengan mi(y), hanya prior predictive
  fit <- suppressWarnings(
    brms::brm(
      formula = brms::bf(y|mi() ~ x1 + x2),
      data = data_na_y,
      family = gaussian(),
      sample_prior = "only",
      prior = c(
        brms::prior("normal(0, 1)", class = "b"),
        brms::prior("normal(0, 1)", class = "Intercept"),
        brms::prior("exponential(1)", class = "sigma")
      ),
      iter = 500,
      chains = 2,
      refresh = 0
    )
  )
  
  results <- suppressWarnings(hbpc(fit, data = data_na_y, response_var = "y"))
  expect_s3_class(results$prior_predictive_plot, "gg")  # bukan langsung 'results
})


# === Re-migrated from main (require real fits) ===
test_that("hbpc returns expected structure and classes", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  result <- suppressWarnings(hbpc(model = fit, data = sample_data, response_var = "y"))
  
  expect_s3_class(result, "hbpc_results")
  expect_named(result, c("prior_predictive_plot", "prior_draws", "observed"))
  expect_s3_class(result$prior_predictive_plot, "gg")
})

test_that("hbpc automatically detects response variable and data", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  result_auto <- suppressWarnings(hbpc(model = fit))
  
  expect_s3_class(result_auto, "hbpc_results")
  expect_s3_class(result_auto$prior_predictive_plot, "gg")
  expect_false(is.null(result_auto$prior_draws))
})

test_that("hbpc throws error for non-brms/hbmfit model", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(
    hbpc(model = lm(y ~ x1, data = sample_data), data = sample_data, response_var = "y"),
    "'model' must be an hbmfit or brmsfit object."
  )
})

test_that("hbpc throws error for invalid response variable name", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  expect_error(
    hbpc(model = fit, data = sample_data, response_var = "not_y"),
    "not found in 'data'"
  )
})

test_that("hbpc errors when model was not fit with sample_prior = 'only'", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  dummy_model <- list(
    formula = brms::bf(y ~ x1 + x2),
    algorithm = "meanfield",
    sample_prior = "no",
    data = sample_data[, c("x1", "x2")], # data tanpa 'y'
    version = "2.20.0"
  )
  class(dummy_model) <- "brmsfit"

  # prior_check() cannot draw prior-predictive samples from a model that was
  # not fit with sample_prior = "only"; it raises an informative error.
  expect_error(
    suppressWarnings(hbpc(dummy_model, data = sample_data, response_var = "y"))
  )
})

test_that("hbpc stops when response_var cannot be determined from model formula", {
  .dev_skip()
  testthat::skip_if_not_installed("brms")
  dummy_model <- list(
    formula = brms::bf(~ x1 + x2),
    algorithm = "sampling",          
    sample_prior = "no",             
    data = sample_data                
  )
  class(dummy_model) <- "brmsfit"
  
  expect_error(
    hbpc(dummy_model),
    "The 'response_var' argument is required and could not be automatically determined"
  )
})
