df <- data.frame(site = "A", date = "2024-01-01", cover = 0.5)

test_that("returns a well-formed qc_issues table", {
  result <- qc_check_columns(df, c("site", "date", "cover"))
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("all columns present and in order -> zero issues", {
  result <- qc_check_columns(df, c("site", "date", "cover"))
  expect_equal(nrow(result), 0L)
})

test_that("missing column -> fail row", {
  result <- qc_check_columns(df, c("site", "date", "cover", "species"))
  expect_equal(qc_status(result), "fail")
  expect_equal(result$issue, "missing_column")
  expect_equal(result$column, "species")
  expect_equal(result$severity, "fail")
  expect_true(is.na(result$row))
  expect_equal(result$check, "qc_check_columns")
})

test_that("extra column -> fail row", {
  result <- qc_check_columns(df, c("site", "date"))
  expect_equal(qc_status(result), "fail")
  expect_equal(result$issue, "unexpected_column")
  expect_equal(result$column, "cover")
})

test_that("both missing and extra -> two fail rows", {
  result <- qc_check_columns(df, c("site", "date", "species"))
  expect_equal(qc_status(result), "fail")
  expect_setequal(result$issue, c("missing_column", "unexpected_column"))
  expect_setequal(result$column, c("species", "cover"))
})

test_that("all columns present but wrong order -> warn rows", {
  result <- qc_check_columns(df, c("date", "site", "cover"))
  expect_equal(qc_status(result), "warn")
  expect_true(all(result$severity == "warn"))
  expect_true(all(result$issue == "wrong_order"))
  expect_true(all(!is.na(result$col_index)))
})

test_that("empty expected_columns with populated data -> extra fail rows", {
  result <- qc_check_columns(df, character(0))
  expect_equal(qc_status(result), "fail")
  expect_equal(nrow(result), 3L)
  expect_true(all(result$issue == "unexpected_column"))
})

test_that("empty data with empty expected -> zero issues", {
  result <- qc_check_columns(data.frame(), character(0))
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("non-data-frame data stops with informative error", {
  expect_error(
    qc_check_columns(list(a = 1), "a"),
    "`data` must be a data frame"
  )
})

test_that("non-character expected_columns stops with informative error", {
  expect_error(
    qc_check_columns(df, 1:3),
    "`expected_columns` must be a character vector"
  )
})
