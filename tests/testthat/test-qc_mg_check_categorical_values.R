# Shared test rules used throughout
make_rules <- function(...) {
  entries <- list(...)
  data.frame(
    column_name      = names(entries),
    value            = unlist(entries, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

# Helper: build a rules data frame where one column has multiple allowed values
make_multi_rules <- function(col, values) {
  data.frame(
    column_name      = rep(col, length(values)),
    value            = values,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("result is a named list with required elements", {
  df    <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  result <- qc_check_categorical_values(df, rules)

  expect_type(result, "list")
  expect_true(all(c("test", "status", "message", "summary", "failures") %in% names(result)))
})

test_that("test element is always 'qc_check_categorical_values'", {
  df    <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  expect_equal(
    qc_check_categorical_values(df, rules)$test,
    "qc_check_categorical_values"
  )
})

test_that("summary has column_name, n_allowed_values, n_violations columns", {
  df    <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  s     <- qc_check_categorical_values(df, rules)$summary

  expect_s3_class(s, "data.frame")
  expect_true(all(c("column_name", "n_allowed_values", "n_violations") %in% colnames(s)))
})

# ---------------------------------------------------------------------------
# Happy path: all values valid -> pass
# ---------------------------------------------------------------------------

test_that("all values in allowed set returns pass", {
  df <- data.frame(
    habitat = c("seagrass", "coral", "seagrass"),
    stringsAsFactors = FALSE
  )
  rules <- make_multi_rules("habitat", c("seagrass", "coral"))
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
  expect_null(result$failures)
})

test_that("pass: summary shows zero violations", {
  df    <- data.frame(type = "A", stringsAsFactors = FALSE)
  rules <- make_multi_rules("type", c("A", "B"))
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$summary$n_violations, 0)
})

test_that("multiple categorical columns, all valid, returns pass", {
  df <- data.frame(
    habitat = "seagrass",
    method  = "point_intercept",
    stringsAsFactors = FALSE
  )
  rules <- rbind(
    make_multi_rules("habitat", c("seagrass", "coral")),
    make_multi_rules("method", c("point_intercept", "belt_transect"))
  )
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
  expect_equal(nrow(result$summary), 2)
})

# ---------------------------------------------------------------------------
# Invalid values -> fail
# ---------------------------------------------------------------------------

test_that("invalid value produces fail status", {
  df    <- data.frame(habitat = c("seagrass", "unknown"), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", c("seagrass", "coral"))
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "fail")
})

test_that("failures data frame has row_index, column_name, value columns", {
  df    <- data.frame(habitat = c("seagrass", "bad"), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  result <- qc_check_categorical_values(df, rules)

  expect_false(is.null(result$failures))
  expect_true(all(c("row_index", "col_index", "column_name", "value") %in% colnames(result$failures)))
})

test_that("failures row_index points to correct rows", {
  df    <- data.frame(habitat = c("ok", "bad", "ok", "also_bad"), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "ok")
  result <- qc_check_categorical_values(df, rules)

  expect_setequal(result$failures$row_index, c(2L, 4L))
})

test_that("failures value column contains the offending values", {
  df    <- data.frame(habitat = c("seagrass", "BAD"), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$failures$value, "BAD")
})

test_that("summary n_violations counts correctly", {
  df    <- data.frame(habitat = c("a", "b", "c"), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "a")
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$summary$n_violations[result$summary$column_name == "habitat"], 2)
})

test_that("invalid values across multiple columns both appear in failures", {
  df <- data.frame(
    habitat = c("seagrass", "bad_habitat"),
    method  = c("good_method", "bad_method"),
    stringsAsFactors = FALSE
  )
  rules <- rbind(
    make_multi_rules("habitat", c("seagrass", "coral")),
    make_multi_rules("method", "good_method")
  )
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "fail")
  expect_setequal(result$failures$column_name, c("habitat", "method"))
})

# ---------------------------------------------------------------------------
# NA values are ignored
# ---------------------------------------------------------------------------

test_that("NA value in categorical column is not treated as a violation", {
  df    <- data.frame(habitat = c("seagrass", NA_character_), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# detail = FALSE suppresses failures
# ---------------------------------------------------------------------------

test_that("detail = FALSE suppresses failures on fail", {
  df    <- data.frame(habitat = "bad", stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", "seagrass")
  result <- qc_check_categorical_values(df, rules, detail = FALSE)

  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Columns in rules but not in data are silently skipped
# ---------------------------------------------------------------------------

test_that("columns in rules but absent from data are skipped", {
  df    <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- rbind(
    make_multi_rules("habitat", "seagrass"),
    make_multi_rules("missing_col", c("x", "y"))
  )
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
  # Only the present column appears in summary
  expect_equal(result$summary$column_name, "habitat")
})

# ---------------------------------------------------------------------------
# Empty rules -> pass (no columns to validate)
# ---------------------------------------------------------------------------

test_that("empty rules data frame returns pass", {
  df    <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- data.frame(
    column_name = character(0),
    value       = character(0),
    stringsAsFactors = FALSE
  )
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
  expect_equal(nrow(result$summary), 0)
  expect_null(result$failures)
})

test_that("rules with no columns matching data returns pass", {
  df    <- data.frame(x = "a", stringsAsFactors = FALSE)
  rules <- make_multi_rules("y", c("a", "b"))
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
})

# ---------------------------------------------------------------------------
# Edge cases: empty data frame
# ---------------------------------------------------------------------------

test_that("zero-row data frame returns pass with zero violations", {
  df    <- data.frame(habitat = character(0), stringsAsFactors = FALSE)
  rules <- make_multi_rules("habitat", c("seagrass", "coral"))
  result <- qc_check_categorical_values(df, rules)

  expect_equal(result$status, "pass")
  expect_equal(result$summary$n_violations, 0)
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-data-frame data stops with informative error", {
  rules <- make_multi_rules("a", "x")
  expect_error(
    qc_check_categorical_values(list(a = "x"), rules),
    "`data` must be a data frame"
  )
})

test_that("non-data-frame rules stops with informative error", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_error(
    qc_check_categorical_values(df, c(a = "x")),
    "`rules` must be a data frame"
  )
})

test_that("rules without required columns stops with informative error", {
  df    <- data.frame(a = "x", stringsAsFactors = FALSE)
  rules <- data.frame(col = "a", allowed = "x", stringsAsFactors = FALSE)
  expect_error(
    qc_check_categorical_values(df, rules),
    "`rules` must have columns"
  )
})

test_that("invalid detail argument stops with informative error", {
  df    <- data.frame(a = "x", stringsAsFactors = FALSE)
  rules <- make_multi_rules("a", "x")
  expect_error(
    qc_check_categorical_values(df, rules, detail = "yes"),
    "`detail` must be a single logical value"
  )
})

# ---------------------------------------------------------------------------
# col_index
# ---------------------------------------------------------------------------

test_that("failures col_index is correct 1-based column position in data", {
  df <- data.frame(
    site    = c("A", "A"),
    habitat = c("seagrass", "bad"),
    stringsAsFactors = FALSE
  )
  rules  <- data.frame(column_name = "habitat", value = "seagrass", stringsAsFactors = FALSE)
  result <- qc_check_categorical_values(df, rules)
  expect_equal(result$failures$col_index, 2L)  # habitat is column 2
})

test_that("col_index reflects position in data for each failing column independently", {
  df <- data.frame(
    id      = 1:2,
    habitat = c("seagrass", "bad_habitat"),
    notes   = c("ok", "ok"),
    method  = c("good_method", "bad_method"),
    stringsAsFactors = FALSE
  )
  rules <- data.frame(
    column_name = c("habitat", "habitat", "method"),
    value       = c("seagrass", "coral", "good_method"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_categorical_values(df, rules)
  expect_equal(result$failures$col_index[result$failures$column_name == "habitat"], 2L)
  expect_equal(result$failures$col_index[result$failures$column_name == "method"], 4L)
})

test_that("col_index is integer type", {
  df    <- data.frame(x = "bad", stringsAsFactors = FALSE)
  rules <- data.frame(column_name = "x", value = "good", stringsAsFactors = FALSE)
  result <- qc_check_categorical_values(df, rules)
  expect_type(result$failures$col_index, "integer")
})

test_that("col_index is 1-based (first column is 1, not 0)", {
  df    <- data.frame(habitat = "bad", stringsAsFactors = FALSE)
  rules <- data.frame(column_name = "habitat", value = "seagrass", stringsAsFactors = FALSE)
  result <- qc_check_categorical_values(df, rules)
  expect_equal(result$failures$col_index, 1L)
})
