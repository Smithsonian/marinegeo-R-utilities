test_that("returns a well-formed qc_issues table", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(a = "STRING"))
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("correct types -> zero issues", {
  df <- data.frame(
    site = "A",
    count = 5L,
    cover = 0.5,
    stringsAsFactors = FALSE
  )
  result <- qc_check_data_types(
    df,
    c(site = "STRING", count = "INT", cover = "DOUBLE")
  )
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("INT accepts integer and double; DATE accepts Date/POSIXct; BOOL accepts logical", {
  expect_equal(
    nrow(qc_check_data_types(data.frame(count = 5L), c(count = "INT"))),
    0L
  )
  expect_equal(
    nrow(qc_check_data_types(data.frame(count = 5.0), c(count = "INT"))),
    0L
  )
  expect_equal(
    nrow(qc_check_data_types(
      data.frame(date = as.Date("2024-01-01")),
      c(date = "DATE")
    )),
    0L
  )
  expect_equal(
    nrow(qc_check_data_types(
      data.frame(date = as.POSIXct("2024-01-01")),
      c(date = "DATE")
    )),
    0L
  )
  expect_equal(
    nrow(qc_check_data_types(data.frame(present = TRUE), c(present = "BOOL"))),
    0L
  )
})

test_that("character where DOUBLE expected -> fail row with observed type in value", {
  df <- data.frame(cover = "0.75", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(cover = "DOUBLE"))
  expect_equal(qc_status(result), "fail")
  expect_equal(result$issue, "type_mismatch")
  expect_equal(result$column, "cover")
  expect_equal(result$value, "character")
  expect_true(is.na(result$row))
})

test_that("multiple type mismatches all appear", {
  df <- data.frame(site = 1, cover = "0.5", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(site = "STRING", cover = "DOUBLE"))
  expect_equal(qc_status(result), "fail")
  expect_setequal(result$column, c("site", "cover"))
})

test_that("columns absent from type_map or data are skipped", {
  df <- data.frame(a = "x", b = 99, stringsAsFactors = FALSE)
  expect_equal(nrow(qc_check_data_types(df, c(a = "STRING"))), 0L)
  expect_equal(
    nrow(qc_check_data_types(
      data.frame(a = "x", stringsAsFactors = FALSE),
      c(a = "STRING", b = "INT")
    )),
    0L
  )
})

test_that("unknown SQL type is silently skipped", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_equal(nrow(qc_check_data_types(df, c(a = "GEOMETRY"))), 0L)
})

test_that("all-NA logical column with non-BOOL expected -> warn row", {
  df <- data.frame(site = NA) # logical NA — read_csv artifact
  result <- qc_check_data_types(df, c(site = "STRING"))
  expect_equal(qc_status(result), "warn")
  expect_equal(result$issue, "all_na_type")
  expect_equal(result$severity, "warn")
})

test_that("all-NA logical column with BOOL expected -> zero issues", {
  df <- data.frame(present = NA)
  result <- qc_check_data_types(df, c(present = "BOOL"))
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("all-NA warn column plus real mismatch -> fail overall, both rows present", {
  df <- data.frame(site = NA, cover = "bad", stringsAsFactors = FALSE)
  result <- qc_check_data_types(df, c(site = "STRING", cover = "DOUBLE"))
  expect_equal(qc_status(result), "fail")
  expect_equal(result$severity[result$column == "site"], "warn")
  expect_equal(result$severity[result$column == "cover"], "fail")
})

test_that("non-data-frame data stops with informative error", {
  expect_error(
    qc_check_data_types(list(a = 1), c(a = "STRING")),
    "`data` must be a data frame"
  )
})

test_that("unnamed or non-character type_map stops with informative error", {
  df <- data.frame(a = 1)
  expect_error(qc_check_data_types(df, c("STRING")), "named character vector")
  expect_error(qc_check_data_types(df, 42), "named character vector")
})
