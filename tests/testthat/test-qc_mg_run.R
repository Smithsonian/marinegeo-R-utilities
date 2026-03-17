# ---------------------------------------------------------------------------
# Shared mock metadata helpers
# ---------------------------------------------------------------------------

# Minimal database_structure for a mock table
mock_db_struct <- data.frame(
  table_id    = c("test_table_v1", "test_table_v1", "test_table_v1"),
  column_name = c("site_name", "survey_date", "cover"),
  data_type   = c("STRING", "DATE", "DOUBLE"),
  stringsAsFactors = FALSE
)

# Categorical rules for the same mock table
mock_cat_vals <- data.frame(
  table_id    = c("test_table_v1", "test_table_v1"),
  column_name = c("site_name", "site_name"),
  value       = c("Site A", "Site B"),
  stringsAsFactors = FALSE
)

# A data frame that should produce a clean pass against mock_db_struct
good_df <- data.frame(
  site_name   = c("Site A", "Site B"),
  survey_date = as.Date(c("2024-06-01", "2024-06-02")),
  cover       = c(20.5, 35.0),
  stringsAsFactors = FALSE
)

make_mock_metadata <- function(
  db_struct = mock_db_struct,
  cat_vals  = mock_cat_vals
) {
  list(
    database_structure = db_struct,
    categorical_values = cat_vals
  )
}

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("qc_run returns a named list with required top-level elements", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_type(result, "list")
  expect_true(all(c("table_id", "status", "n_rows", "tests") %in% names(result)))
})

test_that("table_id is echoed back in the result", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_equal(result$table_id, "test_table_v1")
})

test_that("n_rows matches the number of rows in the input data", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_equal(result$n_rows, nrow(good_df))
})

test_that("tests is a named list with expected test names", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_type(result$tests, "list")
  expect_true("qc_check_columns" %in% names(result$tests))
  expect_true("qc_check_data_types" %in% names(result$tests))
  expect_true("qc_check_categorical_values" %in% names(result$tests))
})

# ---------------------------------------------------------------------------
# Happy path: clean data -> pass
# ---------------------------------------------------------------------------

test_that("clean data against known schema returns top-level pass", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_equal(result$status, "pass")
})

# ---------------------------------------------------------------------------
# Status aggregation: fail > warn > pass
# ---------------------------------------------------------------------------

test_that("missing column in data causes top-level fail", {
  df <- good_df[, c("survey_date", "cover")]  # remove site_name
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(cat_vals = data.frame(
      table_id = character(0), column_name = character(0), value = character(0),
      stringsAsFactors = FALSE
    )),
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(result$status, "fail")
})

test_that("bad categorical value causes top-level fail", {
  df <- good_df
  df$site_name <- c("Site A", "NOT_VALID")

  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(result$status, "fail")
})

test_that("wrong column order causes top-level warn", {
  df <- good_df[, c("cover", "survey_date", "site_name")]
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(cat_vals = data.frame(
      table_id = character(0), column_name = character(0), value = character(0),
      stringsAsFactors = FALSE
    )),
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(result$status, "warn")
})

# ---------------------------------------------------------------------------
# detail = FALSE suppresses row-level failures
# ---------------------------------------------------------------------------

test_that("detail = FALSE: fail status but no failures in any test", {
  df <- good_df
  df$cover <- "bad_type"

  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1", detail = FALSE)

  expect_equal(result$status, "fail")
  for (test_result in result$tests) {
    expect_null(test_result$failures)
  }
})

# ---------------------------------------------------------------------------
# Unknown table_id: no metadata -> warning + empty tests
# ---------------------------------------------------------------------------

test_that("unknown table_id issues a warning and returns empty tests", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  expect_warning(
    result <- qc_run(good_df, table_id = "nonexistent_table"),
    "No metadata found for table_id"
  )
  expect_equal(length(result$tests), 0)
  expect_equal(result$status, "pass")
})

# ---------------------------------------------------------------------------
# Only database_structure metadata (no categorical_values)
# ---------------------------------------------------------------------------

test_that("table with no categorical metadata skips categorical test", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(
      cat_vals = data.frame(
        table_id = character(0), column_name = character(0), value = character(0),
        stringsAsFactors = FALSE
      )
    ),
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_false("qc_check_categorical_values" %in% names(result$tests))
})

# ---------------------------------------------------------------------------
# File path input: CSV
# ---------------------------------------------------------------------------

test_that("CSV file path produces same result as passing data frame directly", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )

  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path))
  readr::write_csv(good_df, csv_path)

  result_df  <- qc_run(good_df, table_id = "test_table_v1")
  result_csv <- qc_run(csv_path, table_id = "test_table_v1")

  expect_equal(result_df$status, result_csv$status)
  expect_equal(result_df$n_rows, result_csv$n_rows)
})

test_that("non-existent file path stops with informative error", {
  expect_error(
    qc_run("/path/that/does/not/exist.csv", table_id = "test_table_v1"),
    "File not found"
  )
})

test_that("unsupported file extension stops with informative error", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  writeLines('{"a": 1}', tmp)

  expect_error(
    qc_run(tmp, table_id = "test_table_v1"),
    "Unsupported file extension"
  )
})

# ---------------------------------------------------------------------------
# Input validation errors
# ---------------------------------------------------------------------------

test_that("non-character table_id stops with informative error", {
  expect_error(
    qc_run(good_df, table_id = 123),
    "`table_id` must be a single character string"
  )
})

test_that("length-2 table_id stops with informative error", {
  expect_error(
    qc_run(good_df, table_id = c("a", "b")),
    "`table_id` must be a single character string"
  )
})

test_that("invalid detail argument stops with informative error", {
  expect_error(
    qc_run(good_df, table_id = "test_table_v1", detail = "yes"),
    "`detail` must be a single logical value"
  )
})

test_that("unsupported x class stops with informative error", {
  expect_error(
    qc_run(42L, table_id = "test_table_v1"),
    "`x` must be a data frame"
  )
})
