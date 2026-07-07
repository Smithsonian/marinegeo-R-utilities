#' Check column presence and order against expected schema
#'
#' @description
#' Validates that a data frame contains all expected columns and that those
#' columns appear in the correct order. Designed for use in MarineGEO QA/QC
#' pipelines and can be called directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#' @param expected_columns Character vector of expected column names, in the
#'   expected order.
#'
#' @return A [qc_issues] tibble with one row per problem (zero rows if the
#'   schema is correct). Missing or unexpected columns are reported as `"fail"`
#'   rows (`issue` of `"missing_column"` / `"unexpected_column"`); columns that
#'   are all present but out of order are reported as `"warn"` rows
#'   (`issue = "wrong_order"`, with the expected position in `col_index`). These
#'   are table/column-level issues, so `row` is `NA`.
#'
#' @details
#' Columns in `data` that are not listed in `expected_columns` are treated as
#' extra and produce a `"fail"` row. Order is evaluated only when all expected
#' columns are present and no extra columns exist.
#'
#' This function is called automatically by [qc_run()] when column metadata
#' exists for the given `table_id`. It can also be called standalone with a
#' custom `expected_columns` vector.
#'
#' @export
#'
#' @examples
#' df <- data.frame(site = "A", date = "2024-01-01", cover = 0.5)
#'
#' # All columns present and in order -> zero issues
#' qc_check_columns(df, c("site", "date", "cover"))
#'
#' # Missing column -> one fail row
#' qc_check_columns(df, c("site", "date", "cover", "species"))
#'
#' # Wrong order -> warn rows
#' qc_check_columns(df, c("date", "site", "cover"))
#'
#' # Extra column -> fail row
#' qc_check_columns(df, c("site", "date"))
qc_check_columns <- function(data, expected_columns) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(expected_columns)) {
    stop("`expected_columns` must be a character vector.")
  }

  actual_cols <- colnames(data)
  missing_cols <- setdiff(expected_columns, actual_cols)
  extra_cols <- setdiff(actual_cols, expected_columns)
  present_expected <- expected_columns[expected_columns %in% actual_cols]
  actual_order <- actual_cols[actual_cols %in% expected_columns]

  chunks <- list()

  if (length(missing_cols) > 0L || length(extra_cols) > 0L) {
    if (length(missing_cols) > 0L) {
      chunks <- c(
        chunks,
        list(.qc_issue(
          check = "qc_check_columns",
          severity = "fail",
          issue = "missing_column",
          column = missing_cols
        ))
      )
    }
    if (length(extra_cols) > 0L) {
      chunks <- c(
        chunks,
        list(.qc_issue(
          check = "qc_check_columns",
          severity = "fail",
          issue = "unexpected_column",
          column = extra_cols
        ))
      )
    }
  } else if (!identical(actual_order, present_expected)) {
    mismatched <- which(actual_order != present_expected)
    chunks <- c(
      chunks,
      list(.qc_issue(
        check = "qc_check_columns",
        severity = "warn",
        issue = "wrong_order",
        column = present_expected[mismatched],
        col_index = mismatched
      ))
    )
  }

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_columns"
  )
}
