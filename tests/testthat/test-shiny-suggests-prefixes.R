# tests/testthat/test-shiny-suggests-prefixes.R
# =============================================================================
# Regression test: catch unprefixed calls to Suggests-package functions in
# the Shiny app.  This prevents bugs like the v1.0.0 release where
# `tabBox()` was called without `shinydashboard::` prefix, which crashed
# the spatial-setup tab when shinydashboard was installed but not
# attached.
# =============================================================================

test_that("no unprefixed shinydashboard helper calls in app.R", {
  app_path <- system.file("shiny", "sae_app", "app.R",
                           package = "hbsaems")
  testthat::skip_if(app_path == "", "app.R not installed")

  app_lines <- readLines(app_path)

  # The shinydashboard UI helpers that are commonly used but must be
  # prefixed because the app only calls `library(shiny)` -- not
  # `library(shinydashboard)`.  Keep this list in sync with the package's
  # actual usage of shinydashboard.
  helpers <- c(
    "dashboardPage", "dashboardHeader", "dashboardSidebar",
    "dashboardBody", "sidebarMenu", "menuItem", "menuItemOutput",
    "menuSubItem", "tabItems", "tabItem", "tabBox", "box",
    "valueBox", "valueBoxOutput", "renderValueBox",
    "infoBox", "infoBoxOutput", "renderInfoBox"
  )

  issues <- character()
  for (fn in helpers) {
    # Match the function name followed by ( but NOT preceded by `::`
    pat <- paste0("(^|[^a-zA-Z0-9_.:])", fn, "[[:space:]]*\\(")
    hits <- grep(pat, app_lines, perl = TRUE)
    for (line_no in hits) {
      line <- app_lines[line_no]
      # Locate the match in the line; check if it's preceded by `::`
      m <- regexpr(paste0("(?<![a-zA-Z0-9_.:])", fn, "[[:space:]]*\\("),
                    line, perl = TRUE)
      if (m == -1L) next
      prefix <- substr(line, max(1L, m - 2L), m - 1L)
      if (identical(prefix, "::")) next     # already prefixed
      issues <- c(issues, sprintf("line %d: %s",
                                    line_no, trimws(line)))
    }
  }

  expect_length(issues, 0L)
})

test_that("no unprefixed DT calls in app.R", {
  app_path <- system.file("shiny", "sae_app", "app.R",
                           package = "hbsaems")
  testthat::skip_if(app_path == "", "app.R not installed")

  app_lines <- readLines(app_path)

  # DT functions that need DT:: prefix
  helpers <- c("datatable", "DTOutput", "renderDT",
                "dataTableOutput", "renderDataTable")

  issues <- character()
  for (fn in helpers) {
    pat <- paste0("(^|[^a-zA-Z0-9_.:])", fn, "[[:space:]]*\\(")
    hits <- grep(pat, app_lines, perl = TRUE)
    for (line_no in hits) {
      line <- app_lines[line_no]
      m <- regexpr(paste0("(?<![a-zA-Z0-9_.:])", fn, "[[:space:]]*\\("),
                    line, perl = TRUE)
      if (m == -1L) next
      prefix <- substr(line, max(1L, m - 2L), m - 1L)
      if (identical(prefix, "::")) next
      issues <- c(issues, sprintf("line %d: %s",
                                    line_no, trimws(line)))
    }
  }

  expect_length(issues, 0L)
})

test_that("no unprefixed shinyWidgets calls in app.R", {
  app_path <- system.file("shiny", "sae_app", "app.R",
                           package = "hbsaems")
  testthat::skip_if(app_path == "", "app.R not installed")

  app_lines <- readLines(app_path)

  # shinyWidgets functions that need shinyWidgets:: prefix
  helpers <- c("pickerInput", "switchInput", "materialSwitch",
                "prettyCheckbox", "airDatepickerInput",
                "chooseSliderSkin", "searchInput",
                "numericRangeInput", "pickerOptions",
                "awesomeRadio", "awesomeCheckbox", "actionBttn")

  issues <- character()
  for (fn in helpers) {
    pat <- paste0("(^|[^a-zA-Z0-9_.:])", fn, "[[:space:]]*\\(")
    hits <- grep(pat, app_lines, perl = TRUE)
    for (line_no in hits) {
      line <- app_lines[line_no]
      m <- regexpr(paste0("(?<![a-zA-Z0-9_.:])", fn, "[[:space:]]*\\("),
                    line, perl = TRUE)
      if (m == -1L) next
      prefix <- substr(line, max(1L, m - 2L), m - 1L)
      if (identical(prefix, "::")) next
      issues <- c(issues, sprintf("line %d: %s",
                                    line_no, trimws(line)))
    }
  }

  expect_length(issues, 0L)
})

test_that("no unprefixed readxl calls in app.R", {
  app_path <- system.file("shiny", "sae_app", "app.R",
                           package = "hbsaems")
  testthat::skip_if(app_path == "", "app.R not installed")

  app_lines <- readLines(app_path)

  helpers <- c("read_excel", "read_xls", "read_xlsx", "excel_sheets")

  issues <- character()
  for (fn in helpers) {
    pat <- paste0("(^|[^a-zA-Z0-9_.:])", fn, "[[:space:]]*\\(")
    hits <- grep(pat, app_lines, perl = TRUE)
    for (line_no in hits) {
      line <- app_lines[line_no]
      m <- regexpr(paste0("(?<![a-zA-Z0-9_.:])", fn, "[[:space:]]*\\("),
                    line, perl = TRUE)
      if (m == -1L) next
      prefix <- substr(line, max(1L, m - 2L), m - 1L)
      if (identical(prefix, "::")) next
      issues <- c(issues, sprintf("line %d: %s",
                                    line_no, trimws(line)))
    }
  }

  expect_length(issues, 0L)
})
