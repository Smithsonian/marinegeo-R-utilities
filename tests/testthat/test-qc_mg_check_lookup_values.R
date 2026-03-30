test_that("return value has correct structure", {
  df <- data.frame(partner_code = "USA-IRL", stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL", "AUS-GBR"))
  result <- qc_check_lookup_values(df, lookups)

  expect_named(result, c("test", "status", "message", "summary", "failures"))
  expect_equal(result$test, "qc_check_lookup_values")
  expect_true(result$status %in% c("pass", "fail"))
  expect_type(result$message, "character")
  expect_s3_class(result$summary, "data.frame")
  expect_named(result$summary, c("column_name", "lookup_source", "n_violations"))
})

test_that("all values recognized -> pass with NULL failures", {
  df <- data.frame(partner_code = c("USA-IRL", "AUS-GBR"), stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL", "AUS-GBR"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "pass")
  expect_null(result$failures)
  expect_equal(result$summary$n_violations, 0L)
})

test_that("unrecognized partner_code -> fail", {
  df <- data.frame(partner_code = c("USA-IRL", "FAKE-CODE"), stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL", "AUS-GBR"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_violations, 1L)
  expect_false(is.null(result$failures))
  expect_equal(result$failures$value, "FAKE-CODE")
})

test_that("unrecognized scientific_name -> fail", {
  df <- data.frame(
    scientific_name = c("Zostera marina", "Not A Species"),
    stringsAsFactors = FALSE
  )
  lookups <- list(scientific_name = c("Zostera marina", "Thalassia testudinum"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_violations, 1L)
  expect_equal(result$failures$value, "Not A Species")
})

test_that("NA values in data column are not treated as violations", {
  df <- data.frame(partner_code = c("USA-IRL", NA), stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL", "AUS-GBR"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "pass")
  expect_null(result$failures)
})

test_that("columns in lookups not present in data are skipped without error", {
  df <- data.frame(other_col = c("a", "b"), stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL"), site_name = c("Carkeek Park"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "pass")
  expect_equal(nrow(result$summary), 0L)
})

test_that("no matching columns -> pass with empty summary", {
  df <- data.frame(species = "fish", stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "pass")
  expect_equal(nrow(result$summary), 0L)
  expect_null(result$failures)
})

test_that("violations in one column only, other column passes", {
  df <- data.frame(
    partner_code = c("USA-IRL", "FAKE"),
    site_name    = c("Carkeek Park", "Bodega Bay"),
    stringsAsFactors = FALSE
  )
  lookups <- list(
    partner_code = c("USA-IRL", "AUS-GBR"),
    site_name    = c("Carkeek Park", "Bodega Bay")
  )
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "fail")
  pc_row <- result$summary[result$summary$column_name == "partner_code", ]
  sn_row <- result$summary[result$summary$column_name == "site_name", ]
  expect_equal(pc_row$n_violations, 1L)
  expect_equal(sn_row$n_violations, 0L)
})

test_that("violations in multiple columns", {
  df <- data.frame(
    partner_code = c("FAKE1"),
    site_name    = c("FAKE-SITE"),
    stringsAsFactors = FALSE
  )
  lookups <- list(
    partner_code = c("USA-IRL"),
    site_name    = c("Carkeek Park")
  )
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "fail")
  expect_equal(sum(result$summary$n_violations), 2L)
  expect_equal(nrow(result$failures), 2L)
})

test_that("detail = FALSE -> failures is NULL even with violations", {
  df <- data.frame(partner_code = "FAKE", stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL"))
  result <- qc_check_lookup_values(df, lookups, detail = FALSE)

  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

test_that("summary has correct column names", {
  df <- data.frame(partner_code = "USA-IRL", stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL"))
  result <- qc_check_lookup_values(df, lookups)

  expect_named(result$summary, c("column_name", "lookup_source", "n_violations"))
})

test_that("failures has correct column names", {
  df <- data.frame(partner_code = "FAKE", stringsAsFactors = FALSE)
  lookups <- list(partner_code = c("USA-IRL"))
  result <- qc_check_lookup_values(df, lookups)

  expect_named(result$failures, c("row_index", "col_index", "column_name", "value"))
})

test_that("row_index and col_index are correct 1-based positions", {
  df <- data.frame(
    other       = c("x", "y", "z"),
    partner_code = c("USA-IRL", "FAKE", "USA-IRL"),
    stringsAsFactors = FALSE
  )
  lookups <- list(partner_code = c("USA-IRL"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$failures$row_index, 2L)
  expect_equal(result$failures$col_index, 2L)  # partner_code is second column
})

test_that("lookup_source values are correct for known columns", {
  df <- data.frame(
    partner_code    = "USA-IRL",
    site_name       = "Carkeek Park",
    scientific_name = "Zostera marina",
    stringsAsFactors = FALSE
  )
  lookups <- list(
    partner_code    = c("USA-IRL"),
    site_name       = c("Carkeek Park"),
    scientific_name = c("Zostera marina")
  )
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(
    result$summary$lookup_source[result$summary$column_name == "partner_code"],
    "partner_codes"
  )
  expect_equal(
    result$summary$lookup_source[result$summary$column_name == "site_name"],
    "site_codes"
  )
  expect_equal(
    result$summary$lookup_source[result$summary$column_name == "scientific_name"],
    "observation_lookup"
  )
})

test_that("input validation: non-data-frame data -> stop", {
  expect_error(
    qc_check_lookup_values("not a df", list(x = "a")),
    "`data` must be a data frame"
  )
})

test_that("input validation: non-list lookups -> stop", {
  df <- data.frame(x = 1)
  expect_error(
    qc_check_lookup_values(df, "not a list"),
    "`lookups` must be a named list"
  )
})

test_that("input validation: unnamed lookups -> stop", {
  df <- data.frame(x = 1)
  expect_error(
    qc_check_lookup_values(df, list("USA-IRL")),
    "fully named list"
  )
})

test_that("input validation: non-logical detail -> stop", {
  df <- data.frame(x = 1)
  expect_error(
    qc_check_lookup_values(df, list(x = "a"), detail = "yes"),
    "`detail` must be a single logical"
  )
})

test_that("lookup_source for site_code column is 'site_codes'", {
  df <- data.frame(
    site_code = "BIS-001",
    stringsAsFactors = FALSE
  )
  lookups <- list(site_code = c("BIS-001", "CCN-001"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "pass")
  expect_equal(
    result$summary$lookup_source[result$summary$column_name == "site_code"],
    "site_codes"
  )
})

test_that("unrecognized site_code -> fail", {
  df <- data.frame(site_code = c("BIS-001", "FAKE-999"), stringsAsFactors = FALSE)
  lookups <- list(site_code = c("BIS-001", "CCN-001"))
  result <- qc_check_lookup_values(df, lookups)

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_violations, 1L)
  expect_equal(result$failures$value, "FAKE-999")
})
