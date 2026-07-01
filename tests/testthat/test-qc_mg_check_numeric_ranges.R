make_range_rules <- function(
  column_name = "x",
  min_fail = 0,
  max_fail = 10,
  min_warn = NA_real_,
  max_warn = NA_real_,
  range_type = "inclusive"
) {
  data.frame(
    column_name = column_name,
    min_fail = min_fail,
    max_fail = max_fail,
    min_warn = min_warn,
    max_warn = max_warn,
    range_type = range_type,
    stringsAsFactors = FALSE
  )
}

test_that("returns a well-formed qc_issues table", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(1, 2, 3)),
    make_range_rules()
  )
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("all values in range -> zero issues, pass", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(1, 5, 10)),
    make_range_rules()
  )
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("inclusive: value > max_fail -> fail row", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(5, 101)),
    make_range_rules(min_fail = NA_real_, max_fail = 100)
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$row, 2L)
  expect_equal(result$severity, "fail")
  expect_equal(result$issue, "out_of_range")
  expect_equal(result$check, "qc_check_numeric_ranges")
})

test_that("inclusive: value < min_fail -> fail row", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(-1, 5)),
    make_range_rules(min_fail = 0, max_fail = NA_real_)
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$row, 1L)
})

test_that("exclusive: value >= max_fail -> fail row", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(5, 100)),
    make_range_rules(
      min_fail = NA_real_,
      max_fail = 100,
      range_type = "exclusive"
    )
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$row, 2L)
})

test_that("exclusive: value <= min_fail -> fail row", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(0, 5)),
    make_range_rules(
      min_fail = 0,
      max_fail = NA_real_,
      range_type = "exclusive"
    )
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$row, 1L)
})

test_that("warn threshold breached, fail not -> warn row", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(5, 85)),
    make_range_rules(min_fail = 0, max_fail = 100, max_warn = 80)
  )
  expect_equal(qc_status(result), "warn")
  expect_equal(result$severity, "warn")
  expect_equal(result$row, 2L)
})

test_that("both fail and warn violations -> fail status, both severities present", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(5, 85, 110)),
    make_range_rules(min_fail = 0, max_fail = 100, max_warn = 80)
  )
  expect_equal(qc_status(result), "fail")
  expect_setequal(result$severity, c("warn", "fail"))
  expect_equal(sum(result$severity == "fail"), 1L)
  expect_equal(sum(result$severity == "warn"), 1L)
})

test_that("NA values in data column are ignored", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(NA, 5, NA)),
    make_range_rules(min_fail = 0, max_fail = 10)
  )
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("NA bound values -> only applicable bound checked", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(-999, 5, 200)),
    make_range_rules(min_fail = NA_real_, max_fail = 100)
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(result$row, 3L) # only 200 fails; -999 not flagged
})

test_that("rows with NA range_type in rules are skipped", {
  result <- qc_check_numeric_ranges(
    data.frame(x = c(200, 300)),
    make_range_rules(min_fail = 0, max_fail = 100, range_type = NA_character_)
  )
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("columns in rules not in data are skipped", {
  rules <- make_range_rules(
    column_name = c("x", "z"),
    min_fail = c(0, 0),
    max_fail = c(10, 10),
    min_warn = c(NA_real_, NA_real_),
    max_warn = c(NA_real_, NA_real_),
    range_type = c("inclusive", "inclusive")
  )
  result <- qc_check_numeric_ranges(data.frame(x = c(1, 2)), rules)
  expect_equal(nrow(result), 0L)
})

test_that("multi-column: only offending column produces rows", {
  rules <- make_range_rules(
    column_name = c("x", "y"),
    min_fail = c(0, 0),
    max_fail = c(10, 1),
    min_warn = c(NA_real_, NA_real_),
    max_warn = c(NA_real_, NA_real_),
    range_type = c("inclusive", "inclusive")
  )
  result <- qc_check_numeric_ranges(
    data.frame(x = c(1, 5), y = c(0.5, 1.5)),
    rules
  )
  expect_equal(qc_status(result), "fail")
  expect_equal(unique(result$column), "y")
  expect_equal(result$row, 2L)
})

test_that("non-data-frame data throws error", {
  expect_error(
    qc_check_numeric_ranges("not a df", make_range_rules()),
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
