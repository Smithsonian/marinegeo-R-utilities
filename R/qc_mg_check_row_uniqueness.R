#' Check that rows are unique across identity columns
#'
#' @description
#' Validates that no two rows share the same combination of identity-column
#' values. Identity columns are the subset of columns whose `uuid_identity`
#' flag is `TRUE` in `marinegeo_metadata$database_structure` — the same
#' columns used by [utl_mg_generate_row_uuid()] to produce stable row UUIDs.
#'
#' Duplicate identity combinations represent data-entry errors or pipeline
#' faults and are always reported as `"fail"`.
#'
#' @param data A data frame to validate.
#' @param id_cols Character vector. Names of the columns that together form
#'   each row's identity (i.e., the columns with `uuid_identity = TRUE` for
#'   the relevant table). If any named columns are absent from `data`, the
#'   check returns zero issues rather than erroring — missing columns are
#'   expected to be flagged by a separate [qc_check_columns()] test.
#'
#' @return A [qc_issues] tibble with one `"fail"` row per duplicated row
#'   (`issue = "duplicate_row"`), or zero rows if all rows are unique (or if
#'   `id_cols` are absent from `data`). `row` is the 1-based position in `data`
#'   and `value` holds the duplicated identity key; `column` is `NA` because the
#'   identity spans multiple columns.
#'
#' @details
#' All rows that are members of a duplicate group are included, not just the
#' second (and later) occurrences. This makes it easy to locate every affected
#' row in the source data.
#'
#' This function is called automatically by [qc_run()] when the
#' `database_structure` metadata contains one or more columns with
#' `uuid_identity = TRUE` for the given `table_id`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   site_code   = c("BIS-001", "BIS-001", "CCN-001"),
#'   transect_id = c(1L, 1L, 1L),        # row 1 and 2 are duplicates
#'   cover       = c(45.2, 30.1, 60.0),
#'   stringsAsFactors = FALSE
#' )
#'
#' qc_check_row_uniqueness(df, id_cols = c("site_code", "transect_id"))
qc_check_row_uniqueness <- function(data, id_cols) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(id_cols) || length(id_cols) == 0L) {
    stop("`id_cols` must be a non-empty character vector.")
  }

  empty <- function() {
    new_qc_issues(
      n_rows = nrow(data),
      checks_run = "qc_check_row_uniqueness"
    )
  }

  # Missing id_cols are caught by qc_check_columns; emit no issues here.
  if (length(setdiff(id_cols, colnames(data))) > 0L || nrow(data) == 0L) {
    return(empty())
  }

  id_df <- data[, id_cols, drop = FALSE]
  is_dup <- duplicated(id_df) | duplicated(id_df, fromLast = TRUE)

  if (!any(is_dup)) {
    return(empty())
  }

  col_strs <- lapply(id_cols, function(cn) {
    paste0(cn, "=", as.character(id_df[[cn]]))
  })
  keys <- do.call(paste, c(col_strs, list(sep = ", ")))[is_dup]

  rows <- .qc_issue(
    check = "qc_check_row_uniqueness",
    severity = "fail",
    issue = "duplicate_row",
    row = which(is_dup),
    value = keys
  )

  new_qc_issues(
    rows,
    n_rows = nrow(data),
    checks_run = "qc_check_row_uniqueness"
  )
}
