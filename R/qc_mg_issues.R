# ---------------------------------------------------------------------------
# qc_issues: the single, uniform output format for all QC checks.
#
# Every qc_check_* function and qc_run() return a `qc_issues` object: a tibble
# with one row per detected problem (zero rows means the check passed) plus a
# fixed set of columns. Run-level metadata (table_id, n_rows, checks_run, and
# the derived status) ride along as attributes. It prints as an ordinary
# tibble. This replaces the old dual `summary` + `failures` per-check lists.
# ---------------------------------------------------------------------------

# Canonical, zero-row skeleton. Single source of truth for column order/types.
.qc_issues_skeleton <- function() {
  tibble::tibble(
    check = character(0),
    severity = character(0),
    row = integer(0),
    column = character(0),
    col_index = integer(0),
    value = character(0),
    issue = character(0)
  )
}

# Derive the worst severity present: fail > warn > pass.
.qc_status <- function(issues) {
  sev <- issues$severity
  if (any(sev == "fail", na.rm = TRUE)) {
    "fail"
  } else if (any(sev == "warn", na.rm = TRUE)) {
    "warn"
  } else {
    "pass"
  }
}

# Vectorized row builder used by each check. Scalar arguments are recycled
# against any vector arguments by tibble(). Always returns the 7 canonical
# columns in canonical order with correct types.
.qc_issue <- function(
  check,
  severity,
  issue,
  row = NA_integer_,
  column = NA_character_,
  col_index = NA_integer_,
  value = NA_character_
) {
  tibble::tibble(
    check = as.character(check),
    severity = as.character(severity),
    row = as.integer(row),
    column = as.character(column),
    col_index = as.integer(col_index),
    value = as.character(value),
    issue = as.character(issue)
  )
}

# Constructor: normalize `rows` to the canonical schema, attach run metadata,
# and stamp the qc_issues class. `rows` may be NULL, a zero-column/zero-row
# frame (e.g. from bind_rows(list())), or a tibble of `.qc_issue()` chunks.
new_qc_issues <- function(
  rows = NULL,
  table_id = NULL,
  n_rows = NULL,
  checks_run = NULL
) {
  skeleton <- .qc_issues_skeleton()

  if (is.null(rows) || nrow(rows) == 0L) {
    out <- skeleton
  } else {
    # Binding against the zero-row skeleton fills any absent canonical column
    # with a correctly-typed NA; selecting by name enforces canonical order and
    # drops anything unexpected.
    out <- dplyr::bind_rows(skeleton, rows)[names(skeleton)]
  }

  attr(out, "table_id") <- table_id
  attr(out, "n_rows") <- n_rows
  attr(out, "checks_run") <- checks_run
  attr(out, "status") <- .qc_status(out)
  class(out) <- c("qc_issues", class(skeleton))
  out
}

#' MarineGEO QC issues table
#'
#' @description
#' The object returned by [qc_run()] and every `qc_check_*()` function. It is a
#' [tibble][tibble::tibble] (class `qc_issues`) with one row per detected
#' problem (zero rows means the data passed the check) and the following
#' columns:
#' \describe{
#'   \item{`check`}{Character. Name of the producing check, e.g.
#'     `"qc_check_numeric_ranges"`.}
#'   \item{`severity`}{Character. `"fail"` or `"warn"`.}
#'   \item{`row`}{Integer. 1-based row number in the validated data, or `NA`
#'     for table- or column-level issues (e.g. a missing column).}
#'   \item{`column`}{Character. Offending column name, or `NA`.}
#'   \item{`col_index`}{Integer. 1-based column position, or `NA`.}
#'   \item{`value`}{Character. The offending value, or `NA`.}
#'   \item{`issue`}{Character. Machine-readable code, e.g. `"out_of_range"`.}
#' }
#'
#' Because it is an ordinary tibble, results are inspected and subset with
#' standard `dplyr` (e.g. `dplyr::filter()`, `dplyr::count()`) and printed like
#' any other tibble.
#'
#' Run-level metadata is attached as attributes, read with [base::attr()]:
#' `table_id`, `n_rows` (rows in the validated data), `checks_run` (checks that
#' executed), and `status` (the worst severity present: `"fail"` > `"warn"` >
#' `"pass"`).
#'
#' @name qc_issues
#' @keywords internal
NULL
