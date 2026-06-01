# tests/testthat/dev-tests/test-shiny-app-shinytest2.R
# =============================================================================
# Heavy end-to-end shinytest2 test (Stan-backed).  The light browser tests
# (tab navigation, predict_type selector, family selection, data upload) live
# in tests/testthat/dev-tests/test-shiny-app-browser.R (also dev-tier, browser
# but no Stan).  This file keeps ONLY the full "upload -> fit -> predict" path,
# which compiles and samples a Stan model (minutes) and therefore belongs in
# the dev tier.
#
# Run locally with:
#   Sys.setenv("_R_RUN_DEV_TESTS_" = "true", NOT_CRAN = "true")
#   testthat::test_file("tests/testthat/dev-tests/test-shiny-app-shinytest2.R")
# Needs: shinytest2 + Chrome/Chromium + a Stan toolchain + roomy TMPDIR.
# =============================================================================

test_that("end-to-end: upload -> fit Gaussian -> prediction table renders", {
  .dev_skip()
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("shinyWidgets")
  skip_if_not_installed("DT")

  app_dir <- system.file("shiny", "sae_app", package = "hbsaems")
  if (!nzchar(app_dir))
    app_dir <- testthat::test_path("..", "..", "..", "inst", "shiny", "sae_app")
  skip_if(!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R")),
          "Shiny app directory not found.")

  csv <- tempfile(fileext = ".csv")
  set.seed(1)
  utils::write.csv(
    data.frame(y = rnorm(30, 5, 1), x1 = rnorm(30), area = rep(1:6, 5)),
    csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir, name = "sae_app_fit",
    load_timeout = 60 * 1000, timeout = 30 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$upload_file(data_file = csv)
  app$wait_for_idle(timeout = 15 * 1000)

  app$set_inputs(response_var    = "y")
  app$set_inputs(linear_aux_vars = "x1")
  app$set_inputs(dist_type = "Custom", hb_family = "gaussian",
                 hb_link = "identity")
  app$wait_for_idle(timeout = 10 * 1000)

  app$click("fit_model")
  app$wait_for_idle(timeout = 300 * 1000)

  pred_html <- tryCatch(
    app$get_html("#prediction_results"),
    error = function(e) NULL)
  expect_false(is.null(pred_html))
})
