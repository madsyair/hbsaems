# tests/testthat/dev-tests/test-shiny-app-browser.R
# =============================================================================
# DEV-tier light browser tests (shinytest2, NO Stan fit -> fast, ~seconds
# each).  These verify that every menu/tab opens and the v1.1.0 UI surface is
# wired correctly.  They are guarded so they NEVER run on CRAN or where the
# browser / UI stack is unavailable:
#   * skip_on_cran()
#   * skip_if_not_installed(shinytest2 / shinydashboard / shinyWidgets / DT)
#
# The heavy end-to-end "upload -> fit -> predict" path stays in dev-tests
# (tests/testthat/dev-tests/test-shiny-app-shinytest2.R) because it compiles
# and samples a Stan model (minutes), which is unsuitable for the main suite.
# =============================================================================

.app_dir_main <- function() {
  d <- system.file("shiny", "sae_app", package = "hbsaems")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "..", "inst", "shiny", "sae_app")
  d
}

.skip_unless_shiny_browser <- function() {
  .dev_skip()
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("shinydashboard")
  testthat::skip_if_not_installed("shinyWidgets")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("chromote")
  # A headless browser must actually be launchable; on check machines and
  # CRAN's build farm Chrome is typically absent, and AppDriver$new() would
  # abort rather than skip.  Probe for a usable Chrome before proceeding.
  chrome_ok <- tryCatch(
    !is.null(chromote::find_chrome()),
    error = function(e) FALSE
  )
  if (!isTRUE(chrome_ok))
    testthat::skip("No headless Chrome available for shinytest2.")
  app_dir <- .app_dir_main()
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R")))
    testthat::skip("Shiny app directory not found.")
  invisible(app_dir)
}

.new_app <- function(app_dir, nm) {
  shinytest2::AppDriver$new(
    app_dir, name = nm, load_timeout = 60 * 1000, timeout = 30 * 1000)
}

test_that("every sidebar menu/tab can be opened", {
  app_dir <- .skip_unless_shiny_browser()
  app <- .new_app(app_dir, "sae_app_nav")
  on.exit(app$stop(), add = TRUE)

  tabs <- c("home", "data_upload", "data_exploration", "spatial_setup",
            "modeling", "update_model", "results")
  for (tb in tabs) {
    app$set_inputs(sidebar_menu = tb)
    app$wait_for_idle(timeout = 10 * 1000)
    expect_equal(app$get_value(input = "sidebar_menu"), tb,
                 info = paste("tab failed to open:", tb))
  }
})

test_that("prediction-target selector exists and defaults to epred", {
  app_dir <- .skip_unless_shiny_browser()
  app <- .new_app(app_dir, "sae_app_predtype")
  on.exit(app$stop(), add = TRUE)

  vals <- app$get_values(input = TRUE)
  expect_true("predict_type" %in% names(vals$input))
  expect_equal(vals$input$predict_type, "epred")
  app$set_inputs(predict_type = "proportion")
  expect_equal(app$get_value(input = "predict_type"), "proportion")
})

test_that("HB Family offers the new 'student' family and accepts it", {
  app_dir <- .skip_unless_shiny_browser()
  app <- .new_app(app_dir, "sae_app_student")
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(dist_type = "Custom")
  app$set_inputs(hb_family = "student")
  expect_equal(app$get_value(input = "hb_family"), "student")
})

test_that("uploading a CSV populates the response selector", {
  app_dir <- .skip_unless_shiny_browser()

  csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(y = rnorm(20), x1 = rnorm(20), area = rep(1:5, 4)),
    csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)

  app <- .new_app(app_dir, "sae_app_upload_main")
  on.exit(app$stop(), add = TRUE)

  app$upload_file(data_file = csv)
  app$wait_for_idle(timeout = 15 * 1000)
  expect_false(is.null(app$get_value(input = "response_var")))
})

test_that("data exploration selectors populate after upload", {
  app_dir <- .skip_unless_shiny_browser()

  csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(y = rnorm(20), x1 = rnorm(20), area = rep(1:5, 4)),
    csv, row.names = FALSE)
  on.exit(unlink(csv), add = TRUE)

  app <- .new_app(app_dir, "sae_app_explore")
  on.exit(app$stop(), add = TRUE)

  app$upload_file(data_file = csv)
  app$wait_for_idle(timeout = 15 * 1000)
  app$set_inputs(sidebar_menu = "data_exploration")
  app$wait_for_idle(timeout = 10 * 1000)

  # Numeric-variable selectors for summary/histogram should be populated.
  expect_false(is.null(app$get_value(input = "explore_var_summary")))
  expect_false(is.null(app$get_value(input = "explore_var_hist")))
})

test_that("spatial setup reveals CAR/SAR controls when a type is chosen", {
  app_dir <- .skip_unless_shiny_browser()
  app <- .new_app(app_dir, "sae_app_spatial")
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(sidebar_menu = "spatial_setup")
  app$wait_for_idle(timeout = 10 * 1000)
  # Selecting a spatial type should be accepted by the server.
  app$set_inputs(sre_type = "car")
  expect_equal(app$get_value(input = "sre_type"), "car")
  app$set_inputs(sre_type = "sar")
  expect_equal(app$get_value(input = "sre_type"), "sar")
})
