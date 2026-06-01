# tests/testthat/test-check-shiny-deps.R
# =============================================================================
# Unit tests for check_shiny_deps().  Pure-R; no Shiny needed.
# =============================================================================


# -- structure of returned object --------------------------------------------

test_that("check_shiny_deps returns expected components", {
  res <- check_shiny_deps(verbose = FALSE)
  expect_type(res, "list")
  expect_named(res, c("critical_missing", "optional_missing", "install_cmd"))
  expect_true(is.character(res$critical_missing))
  expect_true(is.character(res$optional_missing))
})

test_that("install_cmd is NULL when nothing missing, character otherwise", {
  res <- check_shiny_deps(verbose = FALSE)
  if (length(res$critical_missing) + length(res$optional_missing) == 0L) {
    expect_null(res$install_cmd)
  } else {
    expect_true(is.character(res$install_cmd))
    expect_true(grepl("^install.packages", res$install_cmd))
  }
})


# -- verbose output ----------------------------------------------------------

test_that("verbose=TRUE prints to console", {
  expect_output(check_shiny_deps(verbose = TRUE),
                 "Shiny App Dependency Check")
  expect_output(check_shiny_deps(verbose = TRUE),
                 "CRITICAL")
  expect_output(check_shiny_deps(verbose = TRUE),
                 "OPTIONAL")
})

test_that("verbose=FALSE produces no output", {
  expect_silent(invisible(check_shiny_deps(verbose = FALSE)))
})


# -- internal classifier ----------------------------------------------------

test_that(".shiny_app_deps lists at least the documented critical pkgs", {
  cls <- hbsaems:::.shiny_app_deps()
  expect_named(cls, c("critical", "optional"))
  expect_true("shinydashboard" %in% names(cls$critical))
  expect_true("DT"             %in% names(cls$critical))
})

test_that(".shiny_app_deps lists key optional pkgs introduced in v1.0.0", {
  opt <- names(hbsaems:::.shiny_app_deps()$optional)
  expect_true("sf"   %in% opt)
  expect_true("spdep" %in% opt)
})


# -- mocking missing packages -----------------------------------------------
# Note: we don't mock base::requireNamespace because that triggers infinite
# recursion (the mock itself calls requireNamespace internally).  Instead we
# verify the classifier output directly against the .shiny_app_deps()
# specification.

test_that("install_cmd properly quotes package names", {
  # Test the command builder directly with controlled input so the test is
  # deterministic regardless of which packages are installed locally.
  cmd <- hbsaems:::.build_install_cmd(c("pkgA", "pkgB"))
  expect_type(cmd, "character")
  expect_true(grepl('"pkgA"', cmd, fixed = TRUE))
  expect_true(grepl('"pkgB"', cmd, fixed = TRUE))
  expect_true(grepl("install.packages(c(", cmd, fixed = TRUE))
  # Names with a dot (real CRAN package style) are quoted too.
  expect_true(grepl('"my.pkg"',
                    hbsaems:::.build_install_cmd("my.pkg"), fixed = TRUE))
  # Nothing missing -> NULL, no command.
  expect_null(hbsaems:::.build_install_cmd(character(0)))

  # Integration: if real packages are missing in this environment, the live
  # command must also quote each one.
  res <- check_shiny_deps(verbose = FALSE)
  if (!is.null(res$install_cmd)) {
    for (p in c(res$critical_missing, res$optional_missing)) {
      expect_true(
        grepl(sprintf('"%s"', p), res$install_cmd, fixed = TRUE),
        info = sprintf("Package '%s' not properly quoted in install_cmd", p)
      )
    }
  }
})

test_that("missing pkg classification matches deps spec", {
  res <- check_shiny_deps(verbose = FALSE)
  spec <- hbsaems:::.shiny_app_deps()

  # Every "critical_missing" entry must come from the critical spec
  expect_true(all(res$critical_missing %in% names(spec$critical)))
  # Every "optional_missing" entry must come from the optional spec
  expect_true(all(res$optional_missing %in% names(spec$optional)))
  # No package should be both critical and optional
  expect_length(intersect(res$critical_missing, res$optional_missing), 0L)
})
