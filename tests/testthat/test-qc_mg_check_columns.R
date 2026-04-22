# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("result is a named list with required elements", {
  df     <- data.frame(a = 1, b = 2)
  result <- qc_check_columns(df, c("a", "b"))

  expect_type(result, "list")
  expect_true(all(c("test", "status", "message", "summary", "failures") %in% names(result)))
})

test_that("test element is always 'qc_check_columns'", {
  df <- data.frame(a = 1)
  expect_equal(qc_check_columns(df, "a")$test, "qc_check_columns")
})

test_that("summary has expected columns and types", {
  df     <- data.frame(a = 1, b = 2)
  result <- qc_check_columns(df, c("a", "b"))
  s      <- result$summary

  expect_s3_class(s, "data.frame")
  expect_true(all(c("n_expected", "n_present", "n_missing", "n_extra", "order_correct") %in% colnames(s)))
})

# ---------------------------------------------------------------------------
# Happy path: all columns present and in order -> pass
# ---------------------------------------------------------------------------

test_that("all columns present in correct order returns pass", {
  df     <- data.frame(x = 1, y = "a", z = TRUE)
  result <- qc_check_columns(df, c("x", "y", "z"))

  expect_equal(result$status, "pass")
  expect_null(result$failures)
})

test_that("pass summary reports correct counts", {
  df     <- data.frame(x = 1, y = 2, z = 3)
  result <- qc_check_columns(df, c("x", "y", "z"))

  expect_equal(result$summary$n_expected, 3)
  expect_equal(result$summary$n_present, 3)
  expect_equal(result$summary$n_missing, 0)
  expect_true(result$summary$order_correct)
})

test_that("extra columns in data beyond expected produce fail status", {
  df     <- data.frame(a = 1, b = 2, extra = 99)
  result <- qc_check_columns(df, c("a", "b"))

  expect_equal(result$status, "fail")
})

test_that("extra column appears in failures with issue = 'extra'", {
  df     <- data.frame(a = 1, b = 2, extra = 99)
  result <- qc_check_columns(df, c("a", "b"))

  expect_false(is.null(result$failures))
  expect_true("extra" %in% result$failures$column_name)
  expect_true(all(result$failures$issue[result$failures$column_name == "extra"] == "extra"))
})

test_that("both missing and extra columns are reported in a single fail result", {
  df     <- data.frame(a = 1, unexpected = 99)
  result <- qc_check_columns(df, c("a", "b"))

  expect_equal(result$status, "fail")
  expect_true("b"          %in% result$failures$column_name)
  expect_true("unexpected" %in% result$failures$column_name)
  expect_true("missing" %in% result$failures$issue)
  expect_true("extra"   %in% result$failures$issue)
})

test_that("summary n_extra reflects number of extra columns", {
  df     <- data.frame(a = 1, b = 2, extra = 99)
  result <- qc_check_columns(df, c("a", "b"))

  expect_equal(result$summary$n_extra, 1)
})

test_that("detail = FALSE suppresses failures for extra columns", {
  df     <- data.frame(a = 1, extra = 99)
  result <- qc_check_columns(df, "a", detail = FALSE)

  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Missing columns -> fail
# ---------------------------------------------------------------------------

test_that("missing column produces fail status", {
  df     <- data.frame(a = 1, b = 2)
  result <- qc_check_columns(df, c("a", "b", "c"))

  expect_equal(result$status, "fail")
})

test_that("failures data frame lists missing column names", {
  df     <- data.frame(a = 1)
  result <- qc_check_columns(df, c("a", "b", "c"))

  expect_false(is.null(result$failures))
  expect_setequal(result$failures$column_name, c("b", "c"))
  expect_true(all(result$failures$issue == "missing"))
})

test_that("summary counts match number of missing columns", {
  df     <- data.frame(a = 1)
  result <- qc_check_columns(df, c("a", "b", "c"))

  expect_equal(result$summary$n_expected, 3)
  expect_equal(result$summary$n_present, 1)
  expect_equal(result$summary$n_missing, 2)
})

test_that("all columns missing produces fail with all in failures", {
  df     <- data.frame(x = 1)
  result <- qc_check_columns(df, c("a", "b", "c"))

  expect_equal(result$status, "fail")
  expect_setequal(result$failures$column_name, c("a", "b", "c", "x"))
})

# ---------------------------------------------------------------------------
# Wrong order -> warn
# ---------------------------------------------------------------------------

test_that("all columns present but in wrong order returns warn", {
  df     <- data.frame(b = 2, a = 1, c = 3)
  result <- qc_check_columns(df, c("a", "b", "c"))

  expect_equal(result$status, "warn")
})

test_that("warn failures data frame has position, expected_column, actual_column columns", {
  df     <- data.frame(b = 2, a = 1, c = 3)
  result <- qc_check_columns(df, c("a", "b", "c"))

  expect_false(is.null(result$failures))
  expect_true(all(c("position", "expected_column", "actual_column", "issue") %in%
    colnames(result$failures)))
  expect_true(all(result$failures$issue == "wrong_order"))
})

test_that("order_correct is FALSE when order is wrong", {
  df     <- data.frame(b = 1, a = 2)
  result <- qc_check_columns(df, c("a", "b"))

  expect_false(result$summary$order_correct)
})

# ---------------------------------------------------------------------------
# detail = FALSE suppresses failures
# ---------------------------------------------------------------------------

test_that("detail = FALSE suppresses failures on fail", {
  df     <- data.frame(a = 1)
  result <- qc_check_columns(df, c("a", "b"), detail = FALSE)

  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

test_that("detail = FALSE suppresses failures on warn", {
  df     <- data.frame(b = 1, a = 2)
  result <- qc_check_columns(df, c("a", "b"), detail = FALSE)

  expect_equal(result$status, "warn")
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("empty expected_columns vector with populated data returns fail", {
  df     <- data.frame(a = 1, b = 2)
  result <- qc_check_columns(df, character(0))

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_extra, 2)
})

test_that("empty data frame with empty expected_columns returns pass", {
  df     <- data.frame()
  result <- qc_check_columns(df, character(0))

  expect_equal(result$status, "pass")
})

test_that("empty data frame with non-empty expected_columns returns fail", {
  df     <- data.frame()
  result <- qc_check_columns(df, c("a", "b"))

  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_missing, 2)
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-data-frame input stops with informative error", {
  expect_error(
    qc_check_columns(list(a = 1), "a"),
    "`data` must be a data frame"
  )
})

test_that("non-character expected_columns stops with informative error", {
  df <- data.frame(a = 1)
  expect_error(
    qc_check_columns(df, 1:3),
    "`expected_columns` must be a character vector"
  )
})

test_that("invalid detail argument stops with informative error", {
  df <- data.frame(a = 1)
  expect_error(
    qc_check_columns(df, "a", detail = "yes"),
    "`detail` must be a single logical value"
  )
})
