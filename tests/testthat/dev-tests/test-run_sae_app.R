# tests/testthat/dev-tests/test-run_sae_app.R
# =============================================================================
# Heavy integration test (requires Stan, ~10-60 seconds).
# Not run on CRAN; not bundled in the package tarball (see .Rbuildignore).
# Gated centrally by .dev_skip() from helper-dev-setup.R.
# =============================================================================

test_that("run_sae_app throws an error when app directory is not found", {
  .dev_skip()
  
  # Mock system.file to return an empty string to simulate the missing app directory
  skip_if_not_installed("mockery")
  if (!require("mockery")) install.packages("mockery")
  library(mockery)
  mockery::stub(run_sae_app, "system.file", "")


  
  # Expect an error when the app directory is not found
  expect_error(
    run_sae_app(),
    "directory was not found"
  )
})

# Check if the function run_sae_app() is available in the hbsaems package
test_that("run_sae_app() is exported and callable", {
  .dev_skip()
  # Check that the function is available in the hbsaems package
  expect_true("run_sae_app" %in% ls("package:hbsaems"))
})

test_that("run_sae_app launches correctly or throws an error when app_dir is not found", {
  .dev_skip()
  # Simulate the environment by modifying the result of system.file
  app_dir <- system.file("shiny/sae_app", package = "hbsaems")
  
  # Check that the path exists (or is not empty)
  expect_true(nzchar(app_dir), info = "Shiny app directory should exist in the package")
  stub(run_sae_app, "system.file", function(...) app_dir)
  stub(run_sae_app, "shiny::runApp", function(appDir, ...) TRUE)
  
  # Expect the function to return TRUE (from stubbed runApp)
  expect_true(run_sae_app()) 
})
