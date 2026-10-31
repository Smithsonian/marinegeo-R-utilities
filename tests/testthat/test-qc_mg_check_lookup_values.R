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

# --- scopes -----------------------------------------------------------------
#
# A scoped column has two ways to be wrong, and the whole point of the second
# issue code is that a submitter looking at `out_of_labelset` does not go
# hunting for a misspelling in a name that is spelled correctly.

test_that("omitting scopes leaves the result unchanged", {
  df <- data.frame(
    scientific_name = c("Zostera marina", "Nothing Real"),
    stringsAsFactors = FALSE
  )
  lookups <- list(
    scientific_name = c("Zostera marina", "Crassostrea virginica")
  )
  expect_identical(
    qc_check_lookup_values(df, lookups),
    qc_check_lookup_values(df, lookups, scopes = list())
  )
})

test_that("a registered value outside the scope -> out_of_labelset", {
  df <- data.frame(
    scientific_name = c("Zostera marina", "Crassostrea virginica"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    lookups = list(
      scientific_name = c("Zostera marina", "Crassostrea virginica")
    ),
    scopes = list(scientific_name = "Zostera marina")
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$row, 2L)
  expect_equal(result$value, "Crassostrea virginica")
  expect_equal(result$issue, "out_of_labelset")
  expect_equal(result$severity, "fail")
  expect_equal(qc_status(result), "fail")
})

test_that("an unregistered value is unknown_lookup, not out_of_labelset", {
  df <- data.frame(
    scientific_name = c("Zostera marinaa", "Crassostrea virginica"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    lookups = list(
      scientific_name = c("Zostera marina", "Crassostrea virginica")
    ),
    scopes = list(scientific_name = "Zostera marina")
  )
  expect_equal(nrow(result), 2L)
  expect_equal(
    result$issue[match(c(1L, 2L), result$row)],
    c("unknown_lookup", "out_of_labelset")
  )
})

test_that("a scoped column reports each bad cell exactly once", {
  df <- data.frame(
    scientific_name = c("Nope", "Nope"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    lookups = list(scientific_name = "Zostera marina"),
    scopes = list(scientific_name = "Zostera marina")
  )
  expect_equal(nrow(result), 2L)
  expect_equal(unique(result$issue), "unknown_lookup")
})

test_that("abbreviation stripping applies to the scope too", {
  df <- data.frame(
    scientific_name = c("Zostera sp.", "Crassostrea spp."),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    lookups = list(scientific_name = c("Zostera", "Crassostrea")),
    scopes = list(scientific_name = "Zostera")
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$value, "Crassostrea spp.")
  expect_equal(result$issue, "out_of_labelset")
})

test_that("NA is not an out_of_labelset violation", {
  df <- data.frame(
    scientific_name = c("Zostera marina", NA),
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    lookups = list(scientific_name = "Zostera marina"),
    scopes = list(scientific_name = "Zostera marina")
  )
  expect_equal(nrow(result), 0L)
})

test_that("a scope on a column with no registry binding is ignored", {
  df <- data.frame(
    scientific_name = "Crassostrea virginica",
    stringsAsFactors = FALSE
  )
  result <- qc_check_lookup_values(
    df,
    lookups = list(),
    scopes = list(scientific_name = "Zostera marina")
  )
  expect_equal(nrow(result), 0L)
})

test_that("non-list scopes stops with informative error", {
  expect_error(
    qc_check_lookup_values(
      data.frame(x = 1),
      list(x = "a"),
      scopes = "not a list"
    ),
    "`scopes` must be a named list"
  )
})

test_that("unnamed scopes stops with informative error", {
  expect_error(
    qc_check_lookup_values(
      data.frame(x = 1),
      list(x = "a"),
      scopes = list("Zostera marina")
    ),
    "fully named list"
  )
})
