test_that("return value has all required elements", {
  df    <- data.frame(x = c(1, 2, 3))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 10,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_named(result, c("test", "status", "message", "summary", "failures"))
  expect_equal(result$test, "qc_check_numeric_ranges")
  expect_true(result$status %in% c("pass", "warn", "fail"))
  expect_true(is.character(result$message))
  expect_true(is.data.frame(result$summary))
  expect_true(all(c("column_name", "n_fail", "n_warn") %in% colnames(result$summary)))
})

test_that("all values in range -> pass", {
  df    <- data.frame(x = c(1, 5, 10))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 10,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "pass")
  expect_null(result$failures)
  expect_equal(result$summary$n_fail, 0)
  expect_equal(result$summary$n_warn, 0)
})

test_that("inclusive: value > max_fail -> fail", {
  df    <- data.frame(x = c(5, 101))
  rules <- data.frame(
    column_name = "x",
    min_fail    = NA_real_,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail, 1)
  expect_equal(result$failures$row_index, 2)
  expect_equal(result$failures$severity, "fail")
})

test_that("inclusive: value < min_fail -> fail", {
  df    <- data.frame(x = c(-1, 5))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = NA_real_,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail, 1)
  expect_equal(result$failures$row_index, 1)
})

test_that("exclusive: value >= max_fail -> fail", {
  df    <- data.frame(x = c(5, 100))
  rules <- data.frame(
    column_name = "x",
    min_fail    = NA_real_,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "exclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail, 1)
  expect_equal(result$failures$row_index, 2)
})

test_that("exclusive: value <= min_fail -> fail", {
  df    <- data.frame(x = c(0, 5))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = NA_real_,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "exclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail, 1)
  expect_equal(result$failures$row_index, 1)
})

test_that("warn threshold breached, fail not -> warn", {
  df    <- data.frame(x = c(5, 85))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = 80,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "warn")
  expect_equal(result$summary$n_warn, 1)
  expect_equal(result$summary$n_fail, 0)
  expect_equal(result$failures$severity, "warn")
})

test_that("both fail and warn violations -> fail status; failures contain both severities", {
  df    <- data.frame(x = c(5, 85, 110))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = 80,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail, 1)
  expect_equal(result$summary$n_warn, 1)
  expect_true("fail" %in% result$failures$severity)
  expect_true("warn" %in% result$failures$severity)
})

test_that("NA values in data column are ignored", {
  df    <- data.frame(x = c(NA, 5, NA))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 10,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "pass")
  expect_equal(result$summary$n_fail, 0)
})

test_that("NA bound values -> only applicable bound checked", {
  # Only max_fail is set; value below a hypothetical min_fail should not fail
  df    <- data.frame(x = c(-999, 5, 200))
  rules <- data.frame(
    column_name = "x",
    min_fail    = NA_real_,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail, 1)       # only 200 fails (> 100)
  expect_equal(result$failures$row_index, 3)   # -999 not flagged
})

test_that("rows with NA range_type in rules are skipped", {
  df    <- data.frame(x = c(200, 300))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = NA_character_,
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(result$status, "pass")
  expect_equal(nrow(result$summary), 0)
})

test_that("columns in rules not in data are skipped", {
  df    <- data.frame(x = c(1, 2))
  rules <- data.frame(
    column_name = c("x", "z"),
    min_fail    = c(0, 0),
    max_fail    = c(10, 10),
    min_warn    = c(NA_real_, NA_real_),
    max_warn    = c(NA_real_, NA_real_),
    range_type  = c("inclusive", "inclusive"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(nrow(result$summary), 1)
  expect_equal(result$summary$column_name, "x")
})

test_that("multi-column test: summary has one row per column", {
  df    <- data.frame(x = c(1, 5), y = c(0.5, 1.5))
  rules <- data.frame(
    column_name = c("x", "y"),
    min_fail    = c(0, 0),
    max_fail    = c(10, 1),
    min_warn    = c(NA_real_, NA_real_),
    max_warn    = c(NA_real_, NA_real_),
    range_type  = c("inclusive", "inclusive"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules)
  expect_equal(nrow(result$summary), 2)
  # y row 2 (1.5 > 1) fails
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_fail[result$summary$column_name == "y"], 1)
  expect_equal(result$summary$n_fail[result$summary$column_name == "x"], 0)
})

test_that("detail = FALSE -> failures is NULL even with violations", {
  df    <- data.frame(x = c(200))
  rules <- data.frame(
    column_name = "x",
    min_fail    = 0,
    max_fail    = 100,
    min_warn    = NA_real_,
    max_warn    = NA_real_,
    range_type  = "inclusive",
    stringsAsFactors = FALSE
  )
  result <- qc_check_numeric_ranges(df, rules, detail = FALSE)
  expect_equal(result$status, "fail")
  expect_null(result$failures)
})

test_that("non-data-frame data throws error", {
  rules <- data.frame(
    column_name = "x", min_fail = 0, max_fail = 10,
    min_warn = NA_real_, max_warn = NA_real_, range_type = "inclusive",
    stringsAsFactors = FALSE
  )
  expect_error(
    qc_check_numeric_ranges("not a df", rules),
    "`data` must be a data frame"
  )
})

test_that("non-data-frame rules throws error", {
  expect_error(
    qc_check_numeric_ranges(data.frame(x = 1), "not a df"),
    "`rules` must be a data frame"
  )
})

test_that("rules missing required columns throws error", {
  expect_error(
    qc_check_numeric_ranges(
      data.frame(x = 1),
      data.frame(column_name = "x", max_fail = 10)
    ),
    "must have columns"
  )
})

test_that("non-logical detail throws error", {
  rules <- data.frame(
    column_name = "x", min_fail = 0, max_fail = 10,
    min_warn = NA_real_, max_warn = NA_real_, range_type = "inclusive",
    stringsAsFactors = FALSE
  )
  expect_error(
    qc_check_numeric_ranges(data.frame(x = 1), rules, detail = "yes"),
    "`detail` must be a single logical"
  )
})
