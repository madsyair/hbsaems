# tests/testthat/test-spatial-arg-validation.R
# Regression tests for spatial-argument validation (hbm.R section 8b/8b-ii)
# Added in v1.0.0 to catch typos like `car_type = "bym2"` without
# `spatial_model = "car"`.

test_that("car_type without spatial_model='car' is rejected", {
  data(data_fhnorm)
  expect_error(
    hbm(formula = brms::bf(y ~ x1 + x2),
        car_type = "bym2",
        data    = data_fhnorm),
    regexp = "`car_type = \"bym2\"`.*was supplied but `spatial_model` is NULL"
  )
})

test_that("car_type with spatial_model='sar' is rejected", {
  data(data_fhnorm)
  data(spatial_weight_sar)
  expect_error(
    hbm(formula        = brms::bf(y ~ x1 + x2),
        spatial_var    = "regency",
        spatial_model  = "sar",
        car_type       = "bym2",
        M              = spatial_weight_sar,
        data           = data_fhnorm),
    regexp = "`car_type = \"bym2\"`.*`spatial_model = \"sar\"`"
  )
})

test_that("sar_type without spatial_model='sar' is rejected", {
  data(data_fhnorm)
  expect_error(
    hbm(formula  = brms::bf(y ~ x1 + x2),
        sar_type = "lag",
        data     = data_fhnorm),
    regexp = "`sar_type = \"lag\"`.*was supplied but `spatial_model` is NULL"
  )
})

test_that("sar_type with spatial_model='car' is rejected", {
  data(data_fhnorm)
  data(adjacency_matrix_car)
  expect_error(
    hbm(formula        = brms::bf(y ~ x1 + x2),
        spatial_var    = "province",
        spatial_model  = "car",
        sar_type       = "error",
        M              = adjacency_matrix_car,
        data           = data_fhnorm),
    regexp = "`sar_type = \"error\"`.*`spatial_model = \"car\"`"
  )
})

test_that("spatial_var alone (no spatial_model) is rejected", {
  data(data_fhnorm)
  expect_error(
    hbm(formula     = brms::bf(y ~ x1 + x2),
        spatial_var = "province",
        data        = data_fhnorm),
    regexp = "`spatial_var = \"province\"`.*was supplied but `spatial_model` is NULL"
  )
})

test_that("spatial_model alone (no spatial_var) is rejected", {
  data(data_fhnorm)
  data(adjacency_matrix_car)
  expect_error(
    hbm(formula       = brms::bf(y ~ x1 + x2),
        spatial_model = "car",
        M             = adjacency_matrix_car,
        data          = data_fhnorm),
    regexp = "`spatial_model = \"car\"`.*was supplied but `spatial_var` is NULL"
  )
})


# ============================================================================
# Deprecation tests: old `sre`, `sre_type`, `group` aliases still work but
# emit a deprecation warning.
# ============================================================================

test_that("Old `sre` argument emits deprecation warning and maps to spatial_var", {
  data(data_fhnorm)
  expect_warning(
    expect_error(
      hbm(formula = brms::bf(y ~ x1 + x2),
          sre    = "province",       # deprecated
          data   = data_fhnorm),
      # should still error because spatial_model is missing
      regexp = "`spatial_var = \"province\"`.*spatial_model"
    ),
    regexp = "Argument `sre` is deprecated"
  )
})

test_that("Old `sre_type` argument emits deprecation warning", {
  data(data_fhnorm)
  data(adjacency_matrix_car)
  expect_warning(
    expect_error(
      hbm(formula  = brms::bf(y ~ x1 + x2),
          sre_type = "car",          # deprecated
          M        = adjacency_matrix_car,
          data     = data_fhnorm),
      # should still error because spatial_var is missing
      regexp = "spatial_var"
    ),
    regexp = "Argument `sre_type` is deprecated"
  )
})

test_that("Passing both `sre` and `spatial_var` errors", {
  data(data_fhnorm)
  expect_error(
    hbm(formula     = brms::bf(y ~ x1 + x2),
        sre         = "province",
        spatial_var = "province",
        data        = data_fhnorm),
    regexp = "Both `spatial_var` and the deprecated alias `sre`"
  )
})

test_that("Passing both `sre_type` and `spatial_model` errors", {
  data(data_fhnorm)
  expect_error(
    hbm(formula       = brms::bf(y ~ x1 + x2),
        sre_type      = "car",
        spatial_model = "car",
        data          = data_fhnorm),
    regexp = "Both `spatial_model` and the deprecated alias `sre_type`"
  )
})


# ============================================================================
# Wrapper-level `group` deprecation
# ============================================================================

test_that("Old `group` arg in wrapper emits deprecation warning", {
  data(data_lnln)
  # We expect either the deprecation warning OR an immediate error
  # because the actual Stan compile won't run in this test environment.
  # Just check that the deprecation message appears in the captured warnings.
  warnings_captured <- character()
  result <- tryCatch(
    withCallingHandlers(
      hbm_lnln(response  = "y_obs",
               auxiliary = c("x1", "x2"),
               group     = "district",   # deprecated
               data      = data_lnln,
               chains = 1, iter = 10, refresh = 0,
               sample_prior = "only"),
      warning = function(w) {
        warnings_captured <<- c(warnings_captured, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(any(grepl("Argument `group` is deprecated", warnings_captured)))
})

test_that("Passing both `group` and `area_var` errors in wrapper", {
  data(data_lnln)
  expect_error(
    hbm_lnln(response  = "y_obs",
             auxiliary = c("x1", "x2"),
             group     = "district",
             area_var  = "district",
             data      = data_lnln),
    regexp = "Pass either `area_var`.*or `group`"
  )
})
