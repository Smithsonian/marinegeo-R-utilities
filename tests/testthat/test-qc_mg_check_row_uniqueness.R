# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.id_cols <- c("site_code", "transect_id")

# All-unique rows
.df_unique <- data.frame(
  site_code   = c("BIS-001", "BIS-001", "CCN-001"),
  transect_id = c(1L, 2L, 1L),
  cover       = c(45.2, 30.1, 60.0),
  stringsAsFactors = FALSE
)

# One duplicate group: rows 1 and 2 share the same identity
.df_one_dup <- data.frame(
  site_code   = c("BIS-001", "BIS-001", "CCN-001"),
  transect_id = c(1L, 1L, 1L),
  cover       = c(45.2, 30.1, 60.0),
  stringsAsFactors = FALSE
)

# Two duplicate groups: rows 1+2 share one identity, rows 3+4 share another
.df_two_dup_groups <- data.frame(
  site_code   = c("BIS-001", "BIS-001", "CCN-001", "CCN-001"),
  transect_id = c(1L, 1L, 2L, 2L),
  cover       = c(45.2, 30.1, 60.0, 55.0),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Happy path — all rows unique
# ---------------------------------------------------------------------------

test_that("all-unique data returns status 'pass'", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_equal(result$status, "pass")
})

test_that("all-unique data returns NULL failures", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_null(result$failures)
})

test_that("all-unique data summary has zero duplicate counts", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_equal(result$summary$n_duplicate_rows, 0L)
  expect_equal(result$summary$n_duplicate_groups, 0L)
})

# ---------------------------------------------------------------------------
# Fail path — one duplicate group
# ---------------------------------------------------------------------------

test_that("one duplicate group returns status 'fail'", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols)
  expect_equal(result$status, "fail")
})

test_that("one duplicate group: failures has both duplicate rows", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols)
  expect_equal(sort(result$failures$row_index), c(1L, 2L))
})

test_that("one duplicate group: failures includes identity column values", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols)
  expect_true("site_code" %in% colnames(result$failures))
  expect_true("transect_id" %in% colnames(result$failures))
})

test_that("one duplicate group: summary counts are correct", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols)
  expect_equal(result$summary$n_duplicate_rows, 2L)
  expect_equal(result$summary$n_duplicate_groups, 1L)
})

# ---------------------------------------------------------------------------
# Multiple duplicate groups
# ---------------------------------------------------------------------------

test_that("two duplicate groups: n_duplicate_groups == 2", {
  result <- qc_check_row_uniqueness(.df_two_dup_groups, .id_cols)
  expect_equal(result$summary$n_duplicate_groups, 2L)
})

test_that("two duplicate groups: n_duplicate_rows == 4", {
  result <- qc_check_row_uniqueness(.df_two_dup_groups, .id_cols)
  expect_equal(result$summary$n_duplicate_rows, 4L)
})

# ---------------------------------------------------------------------------
# detail = FALSE
# ---------------------------------------------------------------------------

test_that("detail = FALSE returns NULL failures even when duplicates exist", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols, detail = FALSE)
  expect_null(result$failures)
  expect_equal(result$status, "fail")
})

test_that("detail = FALSE returns NULL failures when data is unique", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols, detail = FALSE)
  expect_null(result$failures)
})

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("return value has required names", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_named(result, c("test", "status", "message", "summary", "failures"))
})

test_that("test field is always 'qc_check_row_uniqueness'", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_equal(result$test, "qc_check_row_uniqueness")
})

test_that("summary has expected columns", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_named(
    result$summary,
    c("n_rows", "n_id_cols", "n_duplicate_rows", "n_duplicate_groups")
  )
})

test_that("summary n_rows and n_id_cols are correct", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_equal(result$summary$n_rows, nrow(.df_unique))
  expect_equal(result$summary$n_id_cols, length(.id_cols))
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("empty data frame returns 'pass'", {
  df_empty <- .df_unique[0L, ]
  result <- qc_check_row_uniqueness(df_empty, .id_cols)
  expect_equal(result$status, "pass")
  expect_equal(result$summary$n_rows, 0L)
})

test_that("single-row data frame returns 'pass'", {
  result <- qc_check_row_uniqueness(.df_unique[1L, ], .id_cols)
  expect_equal(result$status, "pass")
})

test_that("single id_col works correctly", {
  df <- data.frame(site_code = c("A", "A", "B"), cover = c(1, 2, 3))
  result <- qc_check_row_uniqueness(df, id_cols = "site_code")
  expect_equal(result$status, "fail")
  expect_equal(result$summary$n_duplicate_rows, 2L)
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-data-frame data -> stop", {
  expect_error(
    qc_check_row_uniqueness("not a df", .id_cols),
    "`data` must be a data frame"
  )
})

test_that("non-character id_cols -> stop", {
  expect_error(
    qc_check_row_uniqueness(.df_unique, 123L),
    "`id_cols` must be a non-empty character vector"
  )
})

test_that("empty id_cols -> stop", {
  expect_error(
    qc_check_row_uniqueness(.df_unique, character(0)),
    "`id_cols` must be a non-empty character vector"
  )
})

test_that("id_col missing from data -> skip result with column name in message", {
  result <- qc_check_row_uniqueness(.df_unique, c("site_code", "nonexistent_col"))
  expect_equal(result$test, "qc_check_row_uniqueness")
  expect_equal(result$status, "skip")
  expect_match(result$message, '"nonexistent_col"')
  expect_null(result$summary)
  expect_null(result$failures)
})

test_that("non-logical detail -> stop", {
  expect_error(
    qc_check_row_uniqueness(.df_unique, .id_cols, detail = "yes"),
    "`detail` must be a single logical value"
  )
})

test_that("NA detail -> stop", {
  expect_error(
    qc_check_row_uniqueness(.df_unique, .id_cols, detail = NA),
    "`detail` must be a single logical value"
  )
})
