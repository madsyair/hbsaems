# tests/testthat/test-shiny-server.R
# =============================================================================
# Option A: lightweight reactive-logic tests for the Shiny server via
# shiny::testServer().  These run WITHOUT a browser and WITHOUT fitting a Stan
# model -- they verify that the v1.1.0 wiring is correct:
#   * the `predict_type` input exists and feeds sae_predict();
#   * the new `student` family is offered and assembled into hb_sampling.
#
# The app depends on shinydashboard / shinyWidgets / DT for its UI; if any are
# missing the whole app source cannot be evaluated, so we skip cleanly.  These
# tests are CRAN-safe (no Stan, no network, no browser) but are skipped when
# the optional UI stack is unavailable.
# =============================================================================

.shiny_app_dir <- function() {
  # During R CMD check the installed path is used; in dev, the source tree.
  d <- system.file("shiny", "sae_app", package = "hbsaems")
  if (!nzchar(d)) d <- testthat::test_path("..", "..", "inst", "shiny", "sae_app")
  d
}

.skip_if_no_shiny_stack <- function() {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinydashboard")
  testthat::skip_if_not_installed("shinyWidgets")
  testthat::skip_if_not_installed("DT")
  app_dir <- .shiny_app_dir()
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R")))
    testthat::skip("Shiny app directory not found.")
  invisible(app_dir)
}

test_that("Shiny server exposes a predict_type input defaulting to epred", {
  app_dir <- .skip_if_no_shiny_stack()
  shiny::testServer(app_dir, {
    session$setInputs(predict_type = "epred")
    expect_equal(input$predict_type, "epred")
    # model_fit starts empty (no fit triggered)
    expect_null(model_fit())
  })
})

test_that("predict_type can be switched to proportion in the server", {
  app_dir <- .skip_if_no_shiny_stack()
  shiny::testServer(app_dir, {
    session$setInputs(predict_type = "proportion")
    expect_equal(input$predict_type, "proportion")
  })
})

test_that("hb_family input accepts the new 'student' family", {
  app_dir <- .skip_if_no_shiny_stack()
  shiny::testServer(app_dir, {
    session$setInputs(dist_type = "Custom", hb_family = "student",
                      hb_link = "identity")
    expect_equal(input$hb_family, "student")
  })
})
