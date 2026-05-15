# tests/testthat/test-deprecated.R
# =============================================================================
# All four deprecated functions must emit a .Deprecated() warning and
# forward to their replacement.  CRAN-safe (we tryCatch any downstream Stan
# errors -- only the warning matters).
# =============================================================================

# Helper: check that calling fn produces a deprecation warning matching `pat`.
.expect_deprecation <- function(fn_call, pat) {
  expect_warning(
    tryCatch(fn_call, error = function(e) NULL),
    regexp = pat,
    ignore.case = TRUE
  )
}

test_that("hbcc emits deprecation warning", {
  .expect_deprecation(
    hbcc(structure(list(), class = "hbmfit")),
    "deprecated|convergence_check"
  )
})

test_that("hbmc emits deprecation warning", {
  .expect_deprecation(
    hbmc(structure(list(), class = "hbmfit")),
    "deprecated|model_compare"
  )
})

test_that("hbpc emits deprecation warning", {
  .expect_deprecation(
    hbpc(structure(list(), class = "hbmfit"),
         data         = data.frame(y = 1),
         response_var = "y"),
    "deprecated|prior_check"
  )
})

test_that("hbsae emits deprecation warning", {
  .expect_deprecation(
    hbsae(structure(list(), class = "hbmfit")),
    "deprecated|sae_predict"
  )
})
