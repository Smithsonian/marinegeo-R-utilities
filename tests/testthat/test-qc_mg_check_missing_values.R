test_that("returns a well-formed qc_issues table", {
  df <- data.frame(x = c(1, 2, 3))
  rules <- data.frame(
    column_name = "x",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("no NAs -> zero issues", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  rules <- data.frame(
    column_name = c("x", "y"),
    missing_values = c("enforce", "warn"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(qc_check_missing_values(df, rules)), 0L)
})

test_that("enforce column with NA -> fail row, value is NA", {
  df <- data.frame(x = c(1, NA, 3))
  rules <- data.frame(
    column_name = "x",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(qc_status(result), "fail")
  expect_equal(result$row, 2L)
  expect_equal(result$severity, "fail")
  expect_equal(result$issue, "missing_value")
  expect_true(is.na(result$value))
})

test_that("warn column with NA -> warn row", {
  df <- data.frame(x = c(1, NA, 3))
  rules <- data.frame(
    column_name = "x",
    missing_values = "warn",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(qc_status(result), "warn")
  expect_equal(result$row, 2L)
  expect_equal(result$severity, "warn")
})

test_that("enforce + warn violations -> fail overall, both rows present", {
  df <- data.frame(x = c(NA, 2), y = c("a", NA))
  rules <- data.frame(
    column_name = c("x", "y"),
    missing_values = c("enforce", "warn"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(qc_status(result), "fail")
  expect_equal(nrow(result), 2L)
  expect_setequal(result$severity, c("fail", "warn"))
})

test_that("allow and NA rules are ignored", {
  df <- data.frame(x = c(1, NA, 3), y = c(NA, NA, NA))
  rules <- data.frame(
    column_name = c("x", "y"),
    missing_values = c("enforce", "allow"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(qc_status(result), "fail")
  expect_equal(unique(result$column), "x")
})

test_that("columns in rules not present in data are skipped", {
  df <- data.frame(x = c(1, 2, 3))
  rules <- data.frame(
    column_name = c("x", "z"),
    missing_values = c("enforce", "enforce"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(qc_check_missing_values(df, rules)), 0L)
})

test_that("col_index reflects 1-based position in data", {
  df <- data.frame(a = 1:2, x = c(NA, 2))
  rules <- data.frame(
    column_name = "x",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(result$col_index, 2L)
})

test_that("non-data-frame data throws error", {
  expect_error(
    qc_check_missing_values(
      "not a df",
      data.frame(column_name = "x", missing_values = "enforce")
    ),
    "`data` must be a data frame"
  )
})

test_that("non-data-frame rules throws error", {
  expect_error(
    qc_check_missing_values(data.frame(x = 1), "not a df"),
    "`rules` must be a data frame"
  )
})

test_that("rules missing required columns throws error", {
  expect_error(
    qc_check_missing_values(data.frame(x = 1), data.frame(column_name = "x")),
    "must have columns"
  )
})
