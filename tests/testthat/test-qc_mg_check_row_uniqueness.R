# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.id_cols <- c("site_code", "transect_id")

# All-unique rows
.df_unique <- data.frame(
  site_code = c("BIS-001", "BIS-001", "CCN-001"),
  transect_id = c(1L, 2L, 1L),
  cover = c(45.2, 30.1, 60.0),
  stringsAsFactors = FALSE
)

# One duplicate group: rows 1 and 2 share the same identity
.df_one_dup <- data.frame(
  site_code = c("BIS-001", "BIS-001", "CCN-001"),
  transect_id = c(1L, 1L, 1L),
  cover = c(45.2, 30.1, 60.0),
  stringsAsFactors = FALSE
)

# Two duplicate groups: rows 1+2 share one identity, rows 3+4 share another
.df_two_dup_groups <- data.frame(
  site_code = c("BIS-001", "BIS-001", "CCN-001", "CCN-001"),
  transect_id = c(1L, 1L, 2L, 2L),
  cover = c(45.2, 30.1, 60.0, 55.0),
  stringsAsFactors = FALSE
)

test_that("all-unique data -> zero issues, pass", {
  result <- qc_check_row_uniqueness(.df_unique, .id_cols)
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("one duplicate group -> fail rows for both members", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols)
  expect_equal(qc_status(result), "fail")
  expect_equal(sort(result$row), c(1L, 2L))
  expect_true(all(result$issue == "duplicate_row"))
  expect_true(all(result$severity == "fail"))
})

test_that("duplicate identity key is recorded in value and message", {
  result <- qc_check_row_uniqueness(.df_one_dup, .id_cols)
  expect_true(all(grepl("site_code=BIS-001", result$value)))
  expect_true(all(grepl("transect_id=1", result$value)))
  expect_true(all(grepl("Duplicate identity", result$message)))
})

test_that("two duplicate groups -> four fail rows", {
  result <- qc_check_row_uniqueness(.df_two_dup_groups, .id_cols)
  expect_equal(nrow(result), 4L)
  expect_setequal(result$row, 1:4)
})

test_that("single id_col works correctly", {
  df <- data.frame(site_code = c("A", "A", "B"), cover = c(1, 2, 3))
  result <- qc_check_row_uniqueness(df, id_cols = "site_code")
  expect_equal(qc_status(result), "fail")
  expect_equal(sort(result$row), c(1L, 2L))
})

test_that("empty data -> zero issues", {
  result <- qc_check_row_uniqueness(.df_unique[0L, ], .id_cols)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("id_col missing from data -> zero issues (deferred to qc_check_columns)", {
  result <- qc_check_row_uniqueness(
    .df_unique,
    c("site_code", "nonexistent_col")
  )
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(qc_status(result), "pass")
})

test_that("non-data-frame data stops with informative error", {
  expect_error(
    qc_check_row_uniqueness("not a df", .id_cols),
    "`data` must be a data frame"
  )
})

test_that("non-character or empty id_cols stops with informative error", {
  expect_error(
    qc_check_row_uniqueness(.df_unique, 123L),
    "`id_cols` must be a non-empty character vector"
  )
  expect_error(
    qc_check_row_uniqueness(.df_unique, character(0)),
    "`id_cols` must be a non-empty character vector"
  )
})
