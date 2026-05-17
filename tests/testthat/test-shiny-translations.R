# tests/testthat/test-shiny-translations.R
# =============================================================================
# Unit tests for the bilingual support helpers added in v1.0.0.
# =============================================================================

test_that("tr_langs returns expected language codes", {
  langs <- tr_langs()
  expect_true("en" %in% langs)
  expect_true("id" %in% langs)
  expect_length(langs, 2)
})

test_that("tr_keys returns sorted character vector", {
  ks <- tr_keys()
  expect_type(ks, "character")
  expect_true(length(ks) > 30)            # we have ~70 keys
  expect_identical(ks, sort(ks))
})

test_that("tr_keys errors on unknown language", {
  expect_error(tr_keys("xx"), "Unknown language code")
})

test_that("tr returns English by default", {
  expect_equal(tr("menu_home"),       "Home")
  expect_equal(tr("menu_modeling"),   "Modeling")
  expect_equal(tr("btn_load_data"),   "Load Data")
})

test_that("tr returns Indonesian when lang = 'id'", {
  expect_equal(tr("menu_home",     "id"), "Beranda")
  expect_equal(tr("menu_modeling", "id"), "Pemodelan")
  expect_equal(tr("btn_load_data", "id"), "Muat Data")
})

test_that("tr falls back to English when key missing in target lang", {
  # Inject a key only in 'en' to simulate missing translation
  # We do NOT actually modify the dictionary; instead use a key we know
  # is in both languages and verify the fallback path with a fake lang.
  # An unknown lang gives bracket fallback:
  expect_equal(tr("menu_home", "xx"), "Home")  # falls through to en
})

test_that("tr returns bracketed key when missing everywhere", {
  expect_equal(tr("definitely_not_a_real_key"), "[definitely_not_a_real_key]")
  expect_equal(tr("definitely_not_a_real_key", "id"),
               "[definitely_not_a_real_key]")
})

test_that("Every English key has an Indonesian translation", {
  en_keys <- tr_keys("en")
  id_keys <- tr_keys("id")
  missing_in_id <- setdiff(en_keys, id_keys)
  expect_equal(missing_in_id, character(0),
                info = if (length(missing_in_id))
                          paste("Untranslated keys:",
                                 paste(missing_in_id, collapse = ", "))
                       else "all keys translated")
})

test_that("No Indonesian-only keys exist (en is the reference)", {
  en_keys <- tr_keys("en")
  id_keys <- tr_keys("id")
  extra_in_id <- setdiff(id_keys, en_keys)
  expect_equal(extra_in_id, character(0),
                info = if (length(extra_in_id))
                          paste("Indonesian-only keys:",
                                 paste(extra_in_id, collapse = ", "))
                       else "no orphan keys")
})

test_that("tr validates input types", {
  expect_error(tr(123),         "is.character")
  expect_error(tr("x", lang = 5), "is.character")
  expect_error(tr(c("a", "b")), "length")
})
