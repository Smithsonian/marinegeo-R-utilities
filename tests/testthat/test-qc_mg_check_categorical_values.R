# Build a rules data frame where one column has multiple allowed values
make_multi_rules <- function(col, values) {
  data.frame(
    column_name = rep(col, length(values)),
    value = values,
    stringsAsFactors = FALSE
  )
}

test_that("returns a well-formed qc_issues table", {
  df <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  result <- qc_check_categorical_values(
    df,
    make_multi_rules("habitat", "seagrass")
  )
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("all values valid -> zero issues", {
  df <- data.frame(
    habitat = c("seagrass", "coral", "seagrass"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_categorical_values(
    df,
    make_multi_rules("habitat", c("seagrass", "coral"))
  )
  expect_equal(nrow(result), 0L)
})

test_that("invalid value -> fail row at correct position", {
  df <- data.frame(
    habitat = c("ok", "bad", "ok", "also_bad"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_categorical_values(df, make_multi_rules("habitat", "ok"))
  expect_equal(qc_status(result), "fail")
  expect_setequal(result$row, c(2L, 4L))
  expect_setequal(result$value, c("bad", "also_bad"))
  expect_true(all(result$issue == "invalid_category"))
  expect_true(all(result$check == "qc_check_categorical_values"))
})

test_that("invalid values across columns both appear", {
  df <- data.frame(
    habitat = c("seagrass", "bad_habitat"),
    method = c("good_method", "bad_method"),
    stringsAsFactors = FALSE
  )
  rules <- rbind(
    make_multi_rules("habitat", c("seagrass", "coral")),
    make_multi_rules("method", "good_method")
  )
  result <- qc_check_categorical_values(df, rules)
  expect_equal(qc_status(result), "fail")
  expect_setequal(result$column, c("habitat", "method"))
})

test_that("NA value is not a violation", {
  df <- data.frame(
    habitat = c("seagrass", NA_character_),
    stringsAsFactors = FALSE
  )
  result <- qc_check_categorical_values(
    df,
    make_multi_rules("habitat", "seagrass")
  )
  expect_equal(nrow(result), 0L)
})

test_that("columns in rules absent from data are skipped", {
  df <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- rbind(
    make_multi_rules("habitat", "seagrass"),
    make_multi_rules("missing_col", c("x", "y"))
  )
  expect_equal(nrow(qc_check_categorical_values(df, rules)), 0L)
})

test_that("empty rules -> zero issues", {
  df <- data.frame(habitat = "seagrass", stringsAsFactors = FALSE)
  rules <- data.frame(
    column_name = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(qc_check_categorical_values(df, rules)), 0L)
})

test_that("col_index is the 1-based position in data", {
  df <- data.frame(
    site = c("A", "A"),
    habitat = c("seagrass", "bad"),
    stringsAsFactors = FALSE
  )
  result <- qc_check_categorical_values(
    df,
    data.frame(
      column_name = "habitat",
      value = "seagrass",
      stringsAsFactors = FALSE
    )
  )
  expect_equal(result$col_index, 2L)
  expect_type(result$col_index, "integer")
})

test_that("non-data-frame data stops with informative error", {
  expect_error(
    qc_check_categorical_values(list(a = "x"), make_multi_rules("a", "x")),
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
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  rules <- data.frame(col = "a", allowed = "x", stringsAsFactors = FALSE)
  expect_error(
    qc_check_categorical_values(df, rules),
    "`rules` must have columns"
  )
})
