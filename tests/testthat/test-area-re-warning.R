# tests/testthat/test-area-re-warning.R
# =============================================================================
# Verify the v0.5.1 sanity check that warns when hbm() is called without
# any area-level random effects (neither `re` nor `sre_type`).
#
# These tests do NOT compile Stan models -- they stub brms::brm() so the
# call exits before Stan touches the formula.  The Section 9b warning fires
# BEFORE the brm() call, so it survives the stub.
# =============================================================================


# Stub: throw a recognisable error so we can detect we reached the brm call
.brm_stub <- function(...) stop("__BRM_STUB__", call. = FALSE)


test_that("warning fires when re=NULL and sre_type=NULL", {
  d <- data.frame(y = rnorm(20), x = runif(20))

  # Local mock so the stub is reverted at end of test
  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d,
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL  # ignore the stub error
  )

  matched <- grepl("area-level random effects", warns)
  expect_true(any(matched),
              info = paste("Got warnings:",
                            paste(warns, collapse = " | ")))
})


test_that("NO warning when re is supplied", {
  d <- data.frame(y = rnorm(20), x = runif(20),
                  grp = factor(rep(1:4, 5)))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d, re = ~ (1 | grp),
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  matched <- grepl("area-level random effects", warns)
  expect_false(any(matched),
                info = paste("Unexpected warning:",
                              paste(warns[matched], collapse = " | ")))
})


test_that("NO warning when sre_type='car' is supplied", {
  # Binary symmetric W with zero diagonal: CAR-compatible
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), nrow = 4)
  d <- data.frame(y = rnorm(4), x = 1:4, area = factor(1:4))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d,
          sre = "area", sre_type = "car", M = W,
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  matched <- grepl("area-level random effects", warns)
  expect_false(any(matched),
                info = paste("Unexpected warning:",
                              paste(warns[matched], collapse = " | ")))
})


test_that("NO warning when sre_type='sar' is supplied", {
  # Row-standardised W: SAR-compatible
  W <- matrix(c(0,  .5, .5, 0,
                .5, 0,  0,  .5,
                .5, 0,  0,  .5,
                0,  .5, .5, 0), nrow = 4)
  d <- data.frame(y = rnorm(4), x = 1:4, area = factor(1:4))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d,
          sre = "area", sre_type = "sar", M = W,
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  matched <- grepl("area-level random effects", warns)
  expect_false(any(matched),
                info = paste("Unexpected warning:",
                              paste(warns[matched], collapse = " | ")))
})


test_that("warning text contains all three suggested patterns", {
  d <- data.frame(y = rnorm(20), x = runif(20))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d,
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  msg <- paste(warns, collapse = " ")
  expect_true(grepl("\\(1 \\| area_id\\)",                msg))  # IID
  expect_true(grepl("sre_type = 'car'",                    msg))  # CAR
  expect_true(grepl("sre_type = 'sar'",                    msg))  # SAR
  expect_true(grepl("suppressWarnings",                    msg))  # escape hatch
})


test_that("suppressWarnings silences the area-RE warning", {
  d <- data.frame(y = rnorm(20), x = runif(20))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      suppressWarnings(
        hbm(brms::bf(y ~ x), data = d,
            chains = 1, iter = 10, warmup = 5, refresh = 0)
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  matched <- grepl("area-level random effects", warns)
  expect_false(any(matched),
                info = paste("suppressWarnings did not silence warning:",
                              paste(warns[matched], collapse = " | ")))
})


# ===========================================================================
# v0.5.1: Consistency check for sre / sre_type
# (sre and sre_type must be supplied together or not at all)
# ===========================================================================

test_that("error when sre is supplied but sre_type is NULL", {
  d <- data.frame(y = rnorm(20), x = runif(20),
                  area = factor(rep(1:5, 4)))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  err <- tryCatch(
    hbm(brms::bf(y ~ x), data = d, sre = "area",
        chains = 1, iter = 10, warmup = 5, refresh = 0),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("was supplied but `sre_type` is NULL", err),
              info = paste("Got error:", err))
  expect_true(grepl("SPATIAL CAR", err))
  expect_true(grepl("SPATIAL SAR", err))
  expect_true(grepl("IID area RE", err))
})


test_that("error when sre_type is supplied but sre is NULL", {
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), nrow = 4)
  d <- data.frame(y = rnorm(4), x = 1:4)

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  err <- tryCatch(
    hbm(brms::bf(y ~ x), data = d, sre_type = "car", M = W,
        chains = 1, iter = 10, warmup = 5, refresh = 0),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("was supplied but `sre` is NULL", err),
              info = paste("Got error:", err))
  expect_true(grepl("specify the column", err))
})


test_that("no error when both sre and sre_type are supplied", {
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), nrow = 4)
  d <- data.frame(y = rnorm(4), x = 1:4, area = factor(1:4))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  # Should not error from the consistency check (the brm_stub
  # may still error, but with a different message)
  err <- tryCatch(
    hbm(brms::bf(y ~ x), data = d,
        sre = "area", sre_type = "car", M = W,
        chains = 1, iter = 10, warmup = 5, refresh = 0),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl("was supplied but", err),
                info = paste("Unexpected consistency error:", err))
})


test_that("BYM message when re and sre on same column with CAR", {
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), nrow = 4)
  d <- data.frame(y = rnorm(20), x = runif(20),
                  area = factor(rep(1:4, 5)))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  msgs <- character(0)
  warns <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d,
          re = ~ (1 | area),
          sre = "area", sre_type = "car", M = W,
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  # The BYM advisory message should fire
  bym_msg <- grepl("BYM", msgs)
  expect_true(any(bym_msg),
              info = paste("Got messages:", paste(msgs, collapse = " | ")))
  expect_true(grepl("bym2", paste(msgs, collapse = " ")))
})


test_that("no BYM message when re and sre on different columns", {
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 0, 1,
                1, 0, 0, 1,
                0, 1, 1, 0), nrow = 4)
  d <- data.frame(y = rnorm(20), x = runif(20),
                  area  = factor(rep(1:4, 5)),
                  group = factor(rep(1:5, 4)))

  testthat::local_mocked_bindings(brm = .brm_stub, .package = "brms")

  msgs <- character(0)
  tryCatch(
    withCallingHandlers(
      hbm(brms::bf(y ~ x), data = d,
          re = ~ (1 | group),
          sre = "area", sre_type = "car", M = W,
          chains = 1, iter = 10, warmup = 5, refresh = 0),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) NULL
  )

  # The BYM advisory message should NOT fire (different columns)
  bym_msg <- grepl("BYM", msgs)
  expect_false(any(bym_msg),
                info = paste("Unexpected BYM message:",
                              paste(msgs[bym_msg], collapse = " | ")))
})
