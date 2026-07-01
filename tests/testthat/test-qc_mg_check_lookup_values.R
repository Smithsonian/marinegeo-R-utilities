test_that("returns a well-formed qc_issues table", {
  df <- data.frame(partner_code = "USA-IRL", stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(
    df,
    list(partner_code = c("USA-IRL", "AUS-GBR"))
  )
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("all values recognized -> zero issues", {
  df <- data.frame(
    partner_code = c("USA-IRL", "AUS-GBR"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    list(partner_code = c("USA-IRL", "AUS-GBR"))
  )
  expect_equal(nrow(result), 0L)
})

test_that("unrecognized value -> fail row", {
  df <- data.frame(
    partner_code = c("USA-IRL", "FAKE-CODE"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    list(partner_code = c("USA-IRL", "AUS-GBR"))
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$value, "FAKE-CODE")
  expect_equal(result$issue, "unknown_lookup")
  expect_equal(result$check, "qc_check_lookup_values")
})

test_that("registry source named in message for known columns", {
  df <- data.frame(partner_code = "FAKE", stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(df, list(partner_code = c("USA-IRL")))
  expect_match(result$message, "partner_codes")
})

test_that("NA values are not violations", {
  df <- data.frame(partner_code = c("USA-IRL", NA), stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(
    df,
    list(partner_code = c("USA-IRL", "AUS-GBR"))
  )
  expect_equal(nrow(result), 0L)
})

test_that("columns in lookups absent from data are skipped", {
  df <- data.frame(other_col = c("a", "b"), stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(
    df,
    list(partner_code = c("USA-IRL"), site_name = c("Carkeek Park"))
  )
  expect_equal(nrow(result), 0L)
})

test_that("violations in multiple columns both appear", {
  df <- data.frame(
    partner_code = "FAKE1",
    site_name = "FAKE-SITE",
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    list(partner_code = c("USA-IRL"), site_name = c("Carkeek Park"))
  )
  expect_equal(qc_status(result), "fail")
  expect_setequal(result$column, c("partner_code", "site_name"))
})

test_that("row and col_index are correct 1-based positions", {
  df <- data.frame(
    other = c("x", "y", "z"),
    partner_code = c("USA-IRL", "FAKE", "USA-IRL"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(df, list(partner_code = c("USA-IRL")))
  expect_equal(result$row, 2L)
  expect_equal(result$col_index, 2L)
})

test_that("scientific_name abbreviations pass when bare genus is in registry", {
  df <- data.frame(scientific_name = "Halodule sp.", stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(
    df,
    list(scientific_name = c("Halodule", "Zostera marina"))
  )
  expect_equal(nrow(result), 0L)
})

test_that("scientific_name abbreviation fails when genus not in registry; value keeps original form", {
  df <- data.frame(scientific_name = "Cymodocea sp.", stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(
    df,
    list(scientific_name = c("Zostera marina"))
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$value, "Cymodocea sp.")
})

test_that("abbreviation stripping is not applied to non-scientific_name columns", {
  df <- data.frame(partner_code = "USA sp.", stringsAsFactors = FALSE)
  result <- qc_check_lookup_values(df, list(partner_code = c("USA sp.")))
  expect_equal(nrow(result), 0L)
})

test_that("non-data-frame data stops with informative error", {
  expect_error(
    qc_check_lookup_values("not a df", list(x = "a")),
    "`data` must be a data frame"
  )
})

test_that("non-list lookups stops with informative error", {
  expect_error(
    qc_check_lookup_values(data.frame(x = 1), "not a list"),
    "`lookups` must be a named list"
  )
})

test_that("unnamed lookups stops with informative error", {
  expect_error(
    qc_check_lookup_values(data.frame(x = 1), list("USA-IRL")),
    "fully named list"
  )
})
