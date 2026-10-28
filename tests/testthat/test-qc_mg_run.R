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

# `numeric_ranges` is omitted unless asked for, so the default mock keeps
# exercising the "metadata table absent entirely" path.
make_mock_metadata <- function(
  db_struct = mock_db_struct,
  cat_vals = mock_cat_vals,
  num_ranges = NULL
) {
  out <- list(
    database_structure = db_struct,
    categorical_values = cat_vals
  )
  if (!is.null(num_ranges)) {
    out$numeric_ranges <- num_ranges
  }
  out
}

# A fuller structure mock carrying the missing-value and identity flags that
# `mock_db_struct` leaves out, for the partial-table tests below.
mock_db_struct_full <- data.frame(
  table_id = rep("test_table_v1", 3),
  column_name = c("site_name", "survey_date", "cover"),
  data_type = c("STRING", "DATE", "DOUBLE"),
  missing_values = c("fail", "warn", "allow"),
  uuid_identity = c(TRUE, TRUE, NA),
  stringsAsFactors = FALSE
)

mock_num_ranges <- data.frame(
  table_id = "test_table_v1",
  column_name = "cover",
  range_type = "inclusive",
  max_fail = 100,
  min_fail = 0,
  max_warn = 80,
  min_warn = NA_real_,
  stringsAsFactors = FALSE
)

empty_cat_vals <- function() {
  data.frame(
    table_id = character(0),
    column_name = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("qc_run returns a qc_issues table with run metadata attributes", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
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
    .package = "marinegeo.utils"
  )

  csv_path <- tempfile(fileext = ".csv")
  # A bare on.exit() here would discard local_mocked_bindings()'s restore
  # handler, so the cleanup is deferred instead.
  withr::defer(unlink(csv_path))
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
  withr::defer(unlink(tmp))
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

# ---------------------------------------------------------------------------
# partial = TRUE: column-subset validation
# ---------------------------------------------------------------------------

local_partial_metadata <- function(env = parent.frame()) {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(
      db_struct = mock_db_struct_full,
      num_ranges = mock_num_ranges
    ),
    .package = "marinegeo.utils",
    .env = env
  )
}

test_that("partial = TRUE on a column subset emits no qc_check_columns rows", {
  local_partial_metadata()
  df <- good_df[, "site_name", drop = FALSE]

  result <- qc_run(df, table_id = "test_table_v1", partial = TRUE)

  expect_qc_issues(result)
  expect_false("qc_check_columns" %in% result$check)
  expect_false("qc_check_columns" %in% attr(result, "checks_run"))
  expect_equal(attr(result, "status"), "pass")
})

test_that("partial = FALSE on the same subset still fails on missing columns", {
  local_partial_metadata()
  df <- good_df[, "site_name", drop = FALSE]

  result <- qc_run(df, table_id = "test_table_v1")

  expect_equal(attr(result, "status"), "fail")
  expect_true(any(
    result$check == "qc_check_columns" & result$issue == "missing_column"
  ))
})

test_that("partial = TRUE skips the three table-scoped checks", {
  local_partial_metadata()
  df <- good_df[, "site_name", drop = FALSE]

  result <- qc_run(df, table_id = "test_table_v1", partial = TRUE)

  expect_setequal(
    attr(result, "checks_skipped"),
    c(
      "qc_check_columns",
      "qc_check_row_uniqueness"
    )
  )
})

test_that("partial = FALSE leaves checks_skipped empty", {
  local_partial_metadata()

  result <- qc_run(good_df, table_id = "test_table_v1")

  expect_equal(attr(result, "checks_skipped"), character(0))
})

test_that("partial = TRUE records only checks with an applicable rule", {
  local_partial_metadata()
  # site_name has a type, a controlled vocabulary and an "fail" rule, but no
  # numeric range -- only `cover` does.
  result <- qc_run(
    good_df[, "site_name", drop = FALSE],
    table_id = "test_table_v1",
    partial = TRUE
  )

  checks <- attr(result, "checks_run")
  expect_true("qc_check_data_types" %in% checks)
  expect_true("qc_check_categorical_values" %in% checks)
  expect_true("qc_check_missing_values" %in% checks)
  expect_false("qc_check_numeric_ranges" %in% checks)
})

test_that("partial = TRUE runs the numeric range check when the column is supplied", {
  local_partial_metadata()
  df <- data.frame(cover = c(20.5, 150))

  result <- qc_run(df, table_id = "test_table_v1", partial = TRUE)

  expect_true("qc_check_numeric_ranges" %in% attr(result, "checks_run"))
  expect_true(any(
    result$check == "qc_check_numeric_ranges" &
      result$severity == "fail" &
      result$issue == "out_of_range"
  ))
})

test_that("columns attribute records what was validated", {
  local_partial_metadata()
  df <- good_df[, c("site_name", "cover")]

  result <- qc_run(df, table_id = "test_table_v1", partial = TRUE)

  expect_equal(attr(result, "columns"), c("site_name", "cover"))
  expect_equal(attr(result, "n_rows"), nrow(df))
})

test_that("partial = TRUE warns when no supplied column is known to the table", {
  local_partial_metadata()
  df <- data.frame(not_a_marinegeo_column = 1:3)

  expect_warning(
    result <- qc_run(df, table_id = "test_table_v1", partial = TRUE),
    "are defined for table_id"
  )
  expect_qc_issues(result)
  expect_equal(nrow(result), 0L)
  expect_equal(length(attr(result, "checks_run")), 0L)
})

test_that("non-logical partial stops with an informative error", {
  expect_error(
    qc_run(good_df, table_id = "test_table_v1", partial = "yes"),
    "`partial` must be a single `TRUE` or `FALSE`"
  )
  expect_error(
    qc_run(good_df, table_id = "test_table_v1", partial = NA),
    "`partial` must be a single `TRUE` or `FALSE`"
  )
})

# ---------------------------------------------------------------------------
# Console report
# ---------------------------------------------------------------------------

test_that(".qc_report_lines reports every check that ran as PASS on a clean run", {
  result <- new_qc_issues(
    NULL,
    table_id = "test_table_v1",
    n_rows = 142L,
    checks_run = c("qc_check_columns", "qc_check_data_types")
  )
  lines <- .qc_report_lines(result)

  expect_equal(lines[[1]], "QC: test_table_v1 (142 rows, 2 checks)")
  # The core claim: a check with zero issue rows is still reported, as a pass.
  expect_match(lines[[2]], "^  PASS  qc_check_columns$")
  expect_match(lines[[3]], "^  PASS  qc_check_data_types$")
  expect_equal(lines[[length(lines)]], "Status: PASS - no issues")
  # A passing check carries no issue count.
  expect_false(any(grepl("issue", lines[2:3])))
})

test_that(".qc_report_lines labels each check with its own worst severity", {
  rows <- dplyr::bind_rows(
    .qc_issue("qc_check_missing_values", "warn", "missing_value", row = 1:3),
    .qc_issue("qc_check_numeric_ranges", "fail", "out_of_range", row = 1:12)
  )
  result <- new_qc_issues(
    rows,
    table_id = "test_table_v1",
    n_rows = 142L,
    checks_run = c(
      "qc_check_columns",
      "qc_check_missing_values",
      "qc_check_numeric_ranges"
    )
  )
  lines <- .qc_report_lines(result)

  expect_match(lines[[2]], "^  PASS  qc_check_columns$")
  expect_match(lines[[3]], "^  WARN  qc_check_missing_values \\(3 issues\\)$")
  expect_match(lines[[4]], "^  FAIL  qc_check_numeric_ranges \\(12 issues\\)$")
  # Footer counts issues and the checks that produced them, not all checks run.
  expect_equal(
    lines[[length(lines)]],
    "Status: FAIL - 15 issues across 2 checks"
  )
})

test_that(".qc_report_lines singularizes a lone issue", {
  result <- new_qc_issues(
    .qc_issue("qc_check_columns", "fail", "missing_column", column = "cover"),
    table_id = "test_table_v1",
    n_rows = 2L,
    checks_run = "qc_check_columns"
  )
  lines <- .qc_report_lines(result)

  expect_match(lines[[2]], "\\(1 issue\\)$")
  expect_equal(
    lines[[length(lines)]],
    "Status: FAIL - 1 issue across 1 check"
  )
})

test_that(".qc_report_lines distinguishes 'nothing ran' from a clean pass", {
  result <- new_qc_issues(
    NULL,
    table_id = "nonexistent_table",
    n_rows = 2L,
    checks_run = character(0)
  )
  lines <- .qc_report_lines(result)

  expect_equal(lines[[1]], "QC: nonexistent_table (2 rows, 0 checks)")
  expect_equal(lines[[length(lines)]], "Status: NOT CHECKED - no checks ran")
  expect_false(any(grepl("PASS", lines)))
})

test_that(".qc_report_lines lists skipped checks", {
  result <- new_qc_issues(
    NULL,
    table_id = "test_table_v1",
    n_rows = 3L,
    checks_run = "qc_check_data_types",
    checks_skipped = c("qc_check_columns", "qc_check_row_uniqueness")
  )
  lines <- .qc_report_lines(result)

  expect_true(
    "  skipped: qc_check_columns, qc_check_row_uniqueness" %in% lines
  )
})

test_that(".qc_report_lines tolerates the NULL attributes a bare check leaves", {
  bare <- new_qc_issues(
    .qc_issue("qc_check_columns", "fail", "missing_column"),
    checks_run = "qc_check_columns"
  )
  expect_no_error(lines <- .qc_report_lines(bare))
  expect_match(lines[[1]], "^QC: <unknown table> \\(1 rows, 1 checks\\)$")
})

test_that(".qc_report_lines names every table of a multi-table result", {
  result <- new_qc_issues(
    NULL,
    table_id = c("table_a", "table_b"),
    n_rows = 10L,
    checks_run = "qc_check_columns"
  )
  expect_match(
    .qc_report_lines(result)[[1]],
    "^QC: table_a, table_b \\(10 rows"
  )
})

test_that("qc_run reports to the console by default", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  # The result is needed as well, and expect_message() consumes the condition.
  expect_message(
    result <- qc_run(good_df, table_id = "test_table_v1"),
    "Status: PASS"
  )
  expect_qc_issues(result)
})

test_that("qc_run is silent with verbose = FALSE and the result is unchanged", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  quiet <- expect_no_message(
    qc_run(good_df, table_id = "test_table_v1", verbose = FALSE)
  )
  loud <- suppressMessages(qc_run(good_df, table_id = "test_table_v1"))

  # The report must be purely additive.
  expect_equal(quiet, loud)
  expect_identical(attributes(quiet), attributes(loud))
})

test_that("qc_run reports the no-metadata case alongside its warning", {
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )
  expect_message(
    suppressWarnings(qc_run(good_df, table_id = "nonexistent_table")),
    "Status: NOT CHECKED"
  )
})

test_that("qc_run reports table-scoped checks as skipped under partial = TRUE", {
  local_partial_metadata()
  df <- data.frame(cover = c(20.5, 35.0))

  expect_message(
    qc_run(df, table_id = "test_table_v1", partial = TRUE),
    "skipped: .*qc_check_columns"
  )
})

test_that("non-logical verbose stops with an informative error", {
  expect_error(
    qc_run(good_df, table_id = "test_table_v1", verbose = "yes"),
    "`verbose` must be a single `TRUE` or `FALSE`"
  )
  expect_error(
    qc_run(good_df, table_id = "test_table_v1", verbose = NA),
    "`verbose` must be a single `TRUE` or `FALSE`"
  )
})

# ---------------------------------------------------------------------------
# Stale-rule provenance
#
# A rule table that could not be loaded is read as "no rules", so without this
# attribute a run against unreachable metadata is indistinguishable from a
# clean pass. See R/api_mg_get_registries.R.
# ---------------------------------------------------------------------------

test_that("qc_run names the rule tables that fell back to the offline copy", {
  .mg_registry_env$source <- list()
  withr::defer(.mg_registry_env$source <- list())
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL,
    marinegeo.utils.strict_registry = FALSE
  )
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .mg_fetch_json = function(src) stop("HTTP status was '404 Not Found'"),
    .package = "marinegeo.utils"
  )

  result <- suppressWarnings(qc_run(good_df, "test_table_v1"))

  stale <- attr(result, "stale_registries")
  expect_true("database_structure" %in% stale)
  expect_true("categorical_values" %in% stale)

  # And the human-facing report says so, rather than claiming a clean pass.
  expect_match(
    paste(.qc_report_lines(result), collapse = "\n"),
    "offline rules:.*database_structure"
  )
})

test_that("a deliberately offline run reports no stale rule tables", {
  .mg_registry_env$source <- list()
  withr::defer(.mg_registry_env$source <- list())
  withr::local_options(marinegeo.utils.live_registry = FALSE)
  local_mocked_bindings(
    marinegeo_metadata = make_mock_metadata(),
    .package = "marinegeo.utils"
  )

  result <- qc_run(good_df, "test_table_v1")

  expect_identical(attr(result, "stale_registries"), character(0))
  expect_no_match(
    paste(.qc_report_lines(result), collapse = "\n"),
    "offline rules:"
  )
})
