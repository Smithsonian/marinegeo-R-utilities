# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("result is a named list with required elements", {
  df     <- data.frame(a = "x", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(a = "STRING"))

  expect_type(result, "list")
  expect_true(all(c("test", "status", "message", "summary", "failures") %in% names(result)))
})

test_that("test element is always 'qc_check_data_types'", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_equal(qc_check_data_types(df, c(a = "STRING"))$test, "qc_check_data_types")
})

test_that("summary has n_checked, n_type_mismatches, and n_type_warnings columns", {
  df     <- data.frame(a = "x", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(a = "STRING"))
  s      <- result$summary

  expect_s3_class(s, "data.frame")
  expect_true(all(c("n_checked", "n_type_mismatches", "n_type_warnings") %in% colnames(s)))
})

# ---------------------------------------------------------------------------
# Happy path: correct types -> pass
# ---------------------------------------------------------------------------

test_that("STRING column that is character returns pass", {
  df <- data.frame(site = "A", stringsAsFactors = FALSE)
  expect_equal(qc_check_data_types(df, c(site = "STRING"))$status, "pass")
})

test_that("INT column that is integer returns pass", {
  df <- data.frame(count = 5L)
  expect_equal(qc_check_data_types(df, c(count = "INT"))$status, "pass")
})

test_that("INT column that is numeric (double) returns pass", {
  df <- data.frame(count = 5.0)
  expect_equal(qc_check_data_types(df, c(count = "INT"))$status, "pass")
})

test_that("TINYINT column that is integer returns pass", {
  df <- data.frame(flag = 1L)
  expect_equal(qc_check_data_types(df, c(flag = "TINYINT"))$status, "pass")
})

test_that("DOUBLE column that is numeric returns pass", {
  df <- data.frame(cover = 0.75)
  expect_equal(qc_check_data_types(df, c(cover = "DOUBLE"))$status, "pass")
})

test_that("DATE column that is Date class returns pass", {
  df <- data.frame(date = as.Date("2024-01-01"))
  expect_equal(qc_check_data_types(df, c(date = "DATE"))$status, "pass")
})

test_that("DATE column that is POSIXct returns pass", {
  df <- data.frame(date = as.POSIXct("2024-01-01"))
  expect_equal(qc_check_data_types(df, c(date = "DATE"))$status, "pass")
})

test_that("BOOL column that is logical returns pass", {
  df <- data.frame(present = TRUE)
  expect_equal(qc_check_data_types(df, c(present = "BOOL"))$status, "pass")
})

test_that("all columns correct returns pass with zero mismatches", {
  df <- data.frame(
    site  = "A",
    count = 5L,
    cover = 0.5,
    stringsAsFactors = FALSE
  )
  result <- qc_check_data_types(df, c(site = "STRING", count = "INT", cover = "DOUBLE"))

  expect_equal(result$status, "pass")
  expect_null(result$failures)
  expect_equal(result$summary$n_checked, 3)
  expect_equal(result$summary$n_type_mismatches, 0)
})

# ---------------------------------------------------------------------------
# Type mismatches -> fail
# ---------------------------------------------------------------------------

test_that("character where DOUBLE expected returns fail", {
  df <- data.frame(cover = "0.75", stringsAsFactors = FALSE)
  expect_equal(qc_check_data_types(df, c(cover = "DOUBLE"))$status, "fail")
})

test_that("character where INT expected returns fail", {
  df <- data.frame(count = "5", stringsAsFactors = FALSE)
  expect_equal(qc_check_data_types(df, c(count = "INT"))$status, "fail")
})

test_that("numeric where STRING expected returns fail", {
  df <- data.frame(site = 42)
  expect_equal(qc_check_data_types(df, c(site = "STRING"))$status, "fail")
})

test_that("character where DATE expected returns fail", {
  df <- data.frame(date = "2024-01-01", stringsAsFactors = FALSE)
  expect_equal(qc_check_data_types(df, c(date = "DATE"))$status, "fail")
})

test_that("numeric where BOOL expected returns fail", {
  df <- data.frame(present = 1L)
  expect_equal(qc_check_data_types(df, c(present = "BOOL"))$status, "fail")
})

test_that("failures data frame lists mismatched column with expected and actual types", {
  df <- data.frame(cover = "0.75", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(cover = "DOUBLE"))

  expect_false(is.null(result$failures))
  expect_true(all(c("column_name", "expected_type", "actual_type", "issue", "severity") %in%
                    colnames(result$failures)))
  expect_equal(result$failures$column_name, "cover")
  expect_equal(result$failures$expected_type, "DOUBLE")
  expect_equal(result$failures$actual_type, "character")
  expect_equal(result$failures$severity, "fail")
})

test_that("multiple type mismatches all appear in failures", {
  df <- data.frame(
    site  = 1,
    cover = "0.5",
    stringsAsFactors = FALSE
  )
  result <- qc_check_data_types(df, c(site = "STRING", cover = "DOUBLE"))

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_type_mismatches, 2)
  expect_setequal(result$failures$column_name, c("site", "cover"))
})

# ---------------------------------------------------------------------------
# Skipped columns
# ---------------------------------------------------------------------------

test_that("columns in data but not in type_map are silently skipped", {
  df <- data.frame(a = "x", b = 99, stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(a = "STRING"))

  expect_equal(result$status, "pass")
  expect_equal(result$summary$n_checked, 1)
})

test_that("columns in type_map but missing from data are silently skipped", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(a = "STRING", b = "INT"))

  expect_equal(result$status, "pass")
  expect_equal(result$summary$n_checked, 1)
})

test_that("unknown SQL type is silently skipped (returns pass)", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(a = "GEOMETRY"))

  expect_equal(result$status, "pass")
})

# ---------------------------------------------------------------------------
# detail = FALSE suppresses failures
# ---------------------------------------------------------------------------

test_that("detail = FALSE suppresses failures on fail", {
  df <- data.frame(cover = "bad", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(cover = "DOUBLE"), detail = FALSE)

  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Edge cases: empty data, all-NA columns
# ---------------------------------------------------------------------------

test_that("empty data frame with type_map returns pass with zero checked", {
  df <- data.frame(a = character(0))
  result <- qc_check_data_types(df, c(a = "STRING"))

  expect_equal(result$status, "pass")
  expect_equal(result$summary$n_checked, 1)
})

test_that("all-NA logical column satisfies BOOL type", {
  df <- data.frame(present = NA)
  expect_equal(qc_check_data_types(df, c(present = "BOOL"))$status, "pass")
})

# ---------------------------------------------------------------------------
# All-NA logical column -> warn (read_csv/read_excel artifact)
# ---------------------------------------------------------------------------

test_that("all-NA logical column with STRING expected returns warn", {
  df <- data.frame(site = NA)  # logical NA — simulates read_csv artifact
  result <- qc_check_data_types(df, c(site = "STRING"))

  expect_equal(result$status, "warn")
  expect_false(is.null(result$failures))
  expect_equal(result$failures$issue, "all_na_inferred_type")
  expect_equal(result$failures$severity, "warn")
  expect_equal(result$summary$n_type_warnings, 1L)
  expect_equal(result$summary$n_type_mismatches, 0L)
})

test_that("all-NA logical column with DOUBLE expected returns warn", {
  df <- data.frame(cover = NA)
  expect_equal(qc_check_data_types(df, c(cover = "DOUBLE"))$status, "warn")
})

test_that("all-NA logical column with DATE expected returns warn", {
  df <- data.frame(date = NA)
  expect_equal(qc_check_data_types(df, c(date = "DATE"))$status, "warn")
})

test_that("all-NA logical column with BOOL expected returns pass", {
  df <- data.frame(present = NA)
  result <- qc_check_data_types(df, c(present = "BOOL"))
  expect_equal(result$status, "pass")
  expect_null(result$failures)
})

test_that("all-NA warn column plus real type mismatch returns fail", {
  df <- data.frame(
    site  = NA,          # all-NA logical — warn
    cover = "bad",       # character where DOUBLE expected — fail
    stringsAsFactors = FALSE
  )
  result <- qc_check_data_types(df, c(site = "STRING", cover = "DOUBLE"))

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_type_mismatches, 1L)
  expect_equal(result$summary$n_type_warnings, 1L)
  expect_true("site" %in% result$failures$column_name)
  expect_true("cover" %in% result$failures$column_name)
  expect_equal(result$failures$severity[result$failures$column_name == "site"], "warn")
  expect_equal(result$failures$severity[result$failures$column_name == "cover"], "fail")
})

test_that("detail = FALSE suppresses failures for warn status", {
  df <- data.frame(site = NA)
  result <- qc_check_data_types(df, c(site = "STRING"), detail = FALSE)

  expect_equal(result$status, "warn")
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-data-frame data stops with informative error", {
  expect_error(
    qc_check_data_types(list(a = 1), c(a = "STRING")),
    "`data` must be a data frame"
  )
})

test_that("unnamed type_map stops with informative error", {
  df <- data.frame(a = 1)
  expect_error(
    qc_check_data_types(df, c("STRING")),
    "`type_map` must be a named character vector"
  )
})

test_that("non-character type_map stops with informative error", {
  df <- data.frame(a = 1)
  expect_error(
    qc_check_data_types(df, 42),
    "`type_map` must be a named character vector"
  )
})

test_that("invalid detail argument stops with informative error", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_error(
    qc_check_data_types(df, c(a = "STRING"), detail = "yes"),
    "`detail` must be a single logical value"
  )
})
