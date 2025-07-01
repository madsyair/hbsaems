pdf(NULL)
on.exit(dev.off(), add = TRUE)

data_dummy <- data_fhnorm
fit <- suppressWarnings(hbm(brms::bf(y ~ x1 + x2), data = data_dummy))

# Test hbcc
test_that("hbcc runs and returns invisibly for valid hbmfit", {
  skip_on_cran()
  
  result <- suppressWarnings(hbcc(model = fit))
  expect_type(result, "list")
})

test_that("hbcc throws error with invalid model input", {
  skip_on_cran()
  
  expect_error(hbcc(model = "not_a_model"))
  expect_error(hbcc(model = NULL))
})

test_that("hbcc throws error when plot_types is invalid", {
  skip_on_cran()
  
  # plot_types not known
  expect_error(suppressWarnings(hbcc(model = fit, plot_types = "invalid_plot")))
})

test_that("hbcc throws error when diag_tests is invalid", {
  skip_on_cran()
  
  # diag_tests not valid
  expect_error(suppressWarnings(hbcc(model = fit, diag_tests = "wrong_diag")))
})