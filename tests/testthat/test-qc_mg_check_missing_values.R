test_that("return value has all required elements", {
  df    <- data.frame(x = c(1, 2, 3))
  rules <- data.frame(
    column_name    = "x",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_named(result, c("test", "status", "message", "summary", "failures"))
  expect_equal(result$test, "qc_check_missing_values")
  expect_true(result$status %in% c("pass", "warn", "fail"))
  expect_true(is.character(result$message))
  expect_true(is.data.frame(result$summary))
  expect_true(all(c("column_name", "missing_rule", "n_missing") %in%
                    colnames(result$summary)))
})

test_that("no NAs in any column -> pass", {
  df    <- data.frame(x = 1:3, y = letters[1:3])
  rules <- data.frame(
    column_name    = c("x", "y"),
    missing_values = c("enforce", "warn"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(result$status, "pass")
  expect_null(result$failures)
  expect_equal(nrow(result$summary), 2)
  expect_true(all(result$summary$n_missing == 0))
})

test_that("enforce column with NAs -> fail", {
  df    <- data.frame(x = c(1, NA, 3))
  rules <- data.frame(
    column_name    = "x",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_missing, 1)
  expect_equal(result$failures$row_index, 2)
  expect_equal(result$failures$missing_rule, "enforce")
})

test_that("warn column with NAs -> warn", {
  df    <- data.frame(x = c(1, NA, 3))
  rules <- data.frame(
    column_name    = "x",
    missing_values = "warn",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(result$status, "warn")
  expect_equal(result$summary$n_missing, 1)
  expect_equal(result$failures$row_index, 2)
})

test_that("enforce + warn violations -> fail takes precedence", {
  df    <- data.frame(x = c(NA, 2), y = c("a", NA))
  rules <- data.frame(
    column_name    = c("x", "y"),
    missing_values = c("enforce", "warn"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(result$status, "fail")
  expect_true(nrow(result$failures) == 2)
})

test_that("allow rows in rules are ignored", {
  df    <- data.frame(x = c(1, NA, 3), y = c(NA, NA, NA))
  rules <- data.frame(
    column_name    = c("x", "y"),
    missing_values = c("enforce", "allow"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  # Only x is checked; y (allow) ignored
  expect_equal(result$status, "fail")
  expect_equal(nrow(result$summary), 1)
  expect_equal(result$summary$column_name, "x")
})

test_that("NA in missing_values rule is ignored", {
  df    <- data.frame(x = c(NA, 2), y = c(NA, NA))
  rules <- data.frame(
    column_name    = c("x", "y"),
    missing_values = c("enforce", NA),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(nrow(result$summary), 1)
  expect_equal(result$summary$column_name, "x")
})

test_that("columns in rules not present in data are skipped", {
  df    <- data.frame(x = c(1, 2, 3))
  rules <- data.frame(
    column_name    = c("x", "z"),
    missing_values = c("enforce", "enforce"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(nrow(result$summary), 1)
  expect_equal(result$summary$column_name, "x")
})

test_that("no columns to validate -> pass with empty summary", {
  df    <- data.frame(x = 1:3)
  rules <- data.frame(
    column_name    = "z",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules)
  expect_equal(result$status, "pass")
  expect_equal(nrow(result$summary), 0)
  expect_null(result$failures)
})

test_that("detail = FALSE -> failures is NULL even when NAs present", {
  df    <- data.frame(x = c(NA, 2, 3))
  rules <- data.frame(
    column_name    = "x",
    missing_values = "enforce",
    stringsAsFactors = FALSE
  )
  result <- qc_check_missing_values(df, rules, detail = FALSE)
  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

test_that("non-data-frame data throws error", {
  expect_error(
    qc_check_missing_values("not a df", data.frame(column_name = "x", missing_values = "enforce")),
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
    qc_check_missing_values(
      data.frame(x = 1),
      data.frame(column_name = "x")
    ),
    "must have columns"
  )
})

test_that("non-logical detail throws error", {
  rules <- data.frame(column_name = "x", missing_values = "enforce",
                      stringsAsFactors = FALSE)
  expect_error(
    qc_check_missing_values(data.frame(x = 1), rules, detail = "yes"),
    "`detail` must be a single logical"
  )
})
