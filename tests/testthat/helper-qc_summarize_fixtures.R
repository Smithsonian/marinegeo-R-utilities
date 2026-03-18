make_pass_result <- function() {
  list(
    table_id = "test_table",
    status   = "pass",
    n_rows   = 5L,
    tests    = list(
      qc_check_columns = list(
        test     = "qc_check_columns",
        status   = "pass",
        message  = "All columns present.",
        summary  = data.frame(n_expected = 3L, n_present = 3L),
        failures = NULL
      ),
      qc_check_sci_name = list(
        test     = "qc_check_sci_name",
        status   = "pass",
        message  = "All scientific names valid.",
        summary  = data.frame(n_checked = 5L, n_invalid = 0L),
        failures = NULL
      )
    )
  )
}

make_fail_result <- function() {
  list(
    table_id = "test_table",
    status   = "fail",
    n_rows   = 10L,
    tests    = list(
      qc_check_columns = list(
        test     = "qc_check_columns",
        status   = "pass",
        message  = "All columns present.",
        summary  = data.frame(n_expected = 3L, n_present = 3L),
        failures = NULL
      ),
      qc_check_data_types = list(
        test     = "qc_check_data_types",
        status   = "fail",
        message  = "2 type mismatches.",
        summary  = data.frame(n_checked = 3L, n_type_mismatches = 2L),
        failures = data.frame(
          column_name = c("value", "date"),
          issue       = c("type_mismatch", "type_mismatch"),
          stringsAsFactors = FALSE
        )
      )
    )
  )
}

make_mixed_result <- function() {
  list(
    table_id = "test_table",
    status   = "fail",
    n_rows   = 10L,
    tests    = list(
      qc_check_columns = list(
        test     = "qc_check_columns",
        status   = "pass",
        message  = "All columns present.",
        summary  = data.frame(n_expected = 3L, n_present = 3L),
        failures = NULL
      ),
      qc_check_sci_name = list(
        test     = "qc_check_sci_name",
        status   = "warn",
        message  = "1 unrecognized scientific name.",
        summary  = data.frame(n_checked = 5L, n_invalid = 1L),
        failures = data.frame(
          scientific_name = "Unknown sp.",
          stringsAsFactors = FALSE
        )
      ),
      qc_check_data_types = list(
        test     = "qc_check_data_types",
        status   = "fail",
        message  = "2 type mismatches.",
        summary  = data.frame(n_checked = 3L, n_type_mismatches = 2L),
        failures = data.frame(
          column_name = c("value", "date"),
          issue       = c("type_mismatch", "type_mismatch"),
          stringsAsFactors = FALSE
        )
      )
    )
  )
}

make_no_detail_result <- function() {
  list(
    table_id = "test_table",
    status   = "fail",
    n_rows   = 10L,
    tests    = list(
      qc_check_data_types = list(
        test     = "qc_check_data_types",
        status   = "fail",
        message  = "2 type mismatches.",
        summary  = data.frame(n_checked = 3L, n_type_mismatches = 2L),
        failures = NULL  # detail = FALSE
      )
    )
  )
}

make_empty_tests_result <- function() {
  list(
    table_id = "unknown_table",
    status   = "pass",
    n_rows   = 0L,
    tests    = list()
  )
}

make_heterogeneous_result <- function() {
  list(
    table_id = "test_table",
    status   = "fail",
    n_rows   = 5L,
    tests    = list(
      qc_test_a = list(
        test     = "qc_test_a",
        status   = "fail",
        message  = "1 failure.",
        summary  = data.frame(n = 1L),
        failures = data.frame(col_a = "x", stringsAsFactors = FALSE)
      ),
      qc_test_b = list(
        test     = "qc_test_b",
        status   = "fail",
        message  = "1 failure.",
        summary  = data.frame(n = 1L),
        failures = data.frame(col_b = "y", stringsAsFactors = FALSE)
      )
    )
  )
}

make_empty_failures_result <- function() {
  list(
    table_id = "test_table",
    status   = "fail",
    n_rows   = 5L,
    tests    = list(
      qc_check_columns = list(
        test     = "qc_check_columns",
        status   = "fail",
        message  = "0 failures recorded.",
        summary  = data.frame(n_expected = 3L, n_present = 3L),
        failures = data.frame(column_name = character(0), stringsAsFactors = FALSE)
      )
    )
  )
}
