# ---------------------------------------------------------------------------
# Shared mock metadata helpers
# ---------------------------------------------------------------------------

# Minimal database_structure for a mock table
mock_db_struct <- data.frame(
  table_id = c("test_table_v1", "test_table_v1", "test_table_v1"),
  column_name = c("site_name", "survey_date", "cover"),
  data_type = c("STRING", "DATE", "DOUBLE"),
  stringsAsFactors = FALSE
)

# Categorical rules for the same mock table
mock_cat_vals <- data.frame(
  table_id = c("test_table_v1", "test_table_v1"),
  column_name = c("site_name", "site_name"),
  value = c("Site A", "Site B"),
  stringsAsFactors = FALSE
)

# A data frame that should produce a clean pass against mock_db_struct
good_df <- data.frame(
  site_name = c("Site A", "Site B"),
  survey_date = as.Date(c("2024-06-01", "2024-06-02")),
  cover = c(20.5, 35.0),
  stringsAsFactors = FALSE
)

make_mock_metadata <- function(
  db_struct = mock_db_struct,
  cat_vals = mock_cat_vals
) {
  list(
    database_structure = db_struct,
    categorical_values = cat_vals
  )
}

empty_cat_vals <- function() {
  data.frame(
    table_id = character(0),
    column_name = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
}

# qc_run resolves observation_lookup via the live fetcher (.mg_fetch_registry).
# Stub it so these tests never touch the network; the test data carries no
# scientific_name column, so an empty lookup is sufficient.
.stub_fetch_obs <- function(url) {
  data.frame(
    scientific_name = character(0),
    scientific_id = character(0),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("qc_run returns a qc_issues table with run metadata attributes", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_qc_issues(result)
  expect_equal(attr(result, "table_id"), "test_table_v1")
  expect_equal(attr(result, "n_rows"), nrow(good_df))
  expect_true(!is.null(attr(result, "status")))
})

test_that("checks_run records which checks executed", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  checks <- attr(result, "checks_run")
  expect_true("qc_check_columns" %in% checks)
  expect_true("qc_check_data_types" %in% checks)
  expect_true("qc_check_categorical_values" %in% checks)
})

# ---------------------------------------------------------------------------
# Status aggregation: fail > warn > pass
# ---------------------------------------------------------------------------

test_that("clean data against known schema -> status pass, zero issues", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_equal(attr(result, "status"), "pass")
  expect_equal(nrow(result), 0L)
})

test_that("missing column in data -> status fail", {
  df <- good_df[, c("survey_date", "cover")] # remove site_name
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(cat_vals = empty_cat_vals()),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(attr(result, "status"), "fail")
  expect_true(any(
    result$check == "qc_check_columns" & result$severity == "fail"
  ))
})

test_that("bad categorical value -> status fail", {
  df <- good_df
  df$site_name <- c("Site A", "NOT_VALID")

  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(attr(result, "status"), "fail")
  expect_true(any(result$check == "qc_check_categorical_values"))
})

test_that("wrong column order -> status warn", {
  df <- good_df[, c("cover", "survey_date", "site_name")]
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(cat_vals = empty_cat_vals()),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(attr(result, "status"), "warn")
})

# ---------------------------------------------------------------------------
# Unknown table_id: no metadata -> warning + empty issues
# ---------------------------------------------------------------------------

test_that("unknown table_id issues a warning and returns empty issues", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  expect_warning(
    result <- qc_run(good_df, table_id = "nonexistent_table"),
    "No metadata found for table_id"
  )
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(length(attr(result, "checks_run")), 0L)
  expect_equal(attr(result, "status"), "pass")
})

# ---------------------------------------------------------------------------
# Conditional dispatch
# ---------------------------------------------------------------------------

test_that("table with no categorical metadata skips categorical check", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(cat_vals = empty_cat_vals()),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )
  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_false("qc_check_categorical_values" %in% attr(result, "checks_run"))
})

# ---------------------------------------------------------------------------
# File path input: CSV
# ---------------------------------------------------------------------------

test_that("CSV file path produces same result as passing a data frame", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_registry = .stub_fetch_obs,
    .package = "marinegeo.utils"
  )

  csv_path <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_path))
  readr::write_csv(good_df, csv_path)

  result_df <- qc_run(good_df, table_id = "test_table_v1")
  result_csv <- qc_run(csv_path, table_id = "test_table_v1")

  expect_equal(attr(result_df, "status"), attr(result_csv, "status"))
  expect_equal(attr(result_df, "n_rows"), attr(result_csv, "n_rows"))
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

test_that("non-character or length-2 table_id stops with informative error", {
  expect_error(
    qc_run(good_df, table_id = 123),
    "`table_id` must be a single character string"
  )
  expect_error(
    qc_run(good_df, table_id = c("a", "b")),
    "`table_id` must be a single character string"
  )
})

test_that("unsupported x class stops with informative error", {
  expect_error(
    qc_run(42L, table_id = "test_table_v1"),
    "`x` must be a data frame"
  )
})
