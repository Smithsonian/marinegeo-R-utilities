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
    issue = character(0),
    rule_id = character(0),
    detail = character(0),
    table = character(0)
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
  value = NA_character_,
  rule_id = NA_character_,
  detail = NA_character_,
  table = NA_character_
) {
  tibble::tibble(
    check = as.character(check),
    severity = as.character(severity),
    row = as.integer(row),
    column = as.character(column),
    col_index = as.integer(col_index),
    value = as.character(value),
    issue = as.character(issue),
    rule_id = as.character(rule_id),
    detail = as.character(detail),
    table = as.character(table)
  )
}

# Constructor: normalize `rows` to the canonical schema, attach run metadata,
# and stamp the qc_issues class. `rows` may be NULL, a zero-column/zero-row
# frame (e.g. from bind_rows(list())), or a tibble of `.qc_issue()` chunks.
# `checks_skipped`, `columns` and `rules_skipped` are only populated by the
# dispatcher; the individual checks leave them NULL.
new_qc_issues <- function(
  rows = NULL,
  table_id = NULL,
  n_rows = NULL,
  checks_run = NULL,
  checks_skipped = NULL,
  columns = NULL,
  rules_skipped = NULL
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
  attr(out, "checks_skipped") <- checks_skipped
  attr(out, "columns") <- columns
  attr(out, "rules_skipped") <- rules_skipped
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
#'   \item{`rule_id`}{Character. Identifier of the metadata rule that produced
#'     the issue, or `NA` when the rule is unconditional and carries no id.
#'     Lets a finding be traced back to the CSV row that defined it, and tells
#'     apart a conditional rule from the unconditional rule on the same
#'     column.}
#'   \item{`detail`}{Character. One human-readable sentence of *why*, where a
#'     machine-readable `issue` code is not self-explanatory (e.g. which
#'     condition applied), or `NA`.}
#'   \item{`table`}{Character. Which table the issue is in. `NA` for a
#'     single-table run, where [qc_run()] records the table in the `table_id`
#'     attribute instead; populated by [qc_run_dataset()], whose result spans
#'     several tables.}
#' }
#'
#' Because it is an ordinary tibble, results are inspected and subset with
#' standard `dplyr` (e.g. `dplyr::filter()`, `dplyr::count()`) and printed like
#' any other tibble.
#'
#' Run-level metadata is attached as attributes, read with [base::attr()]:
#' \describe{
#'   \item{`table_id`}{The table identifier the data was validated against.}
#'   \item{`n_rows`}{Rows in the validated data.}
#'   \item{`checks_run`}{Checks that executed.}
#'   \item{`checks_skipped`}{Checks that were not applicable to the input.
#'     Populated by [qc_run()] with `partial = TRUE` and by
#'     [qc_check_variable()], where the three table-scoped checks
#'     (`qc_check_columns`, `qc_check_row_uniqueness`,
#'     `qc_check_aggregate`) cannot run. Lets a caller distinguish
#'     "not applicable" from "passed".}
#'   \item{`columns`}{The column names that were validated — the only record of
#'     what was checked when the result has zero rows.}
#'   \item{`rules_skipped`}{`condition_id`s of conditional rules that could not
#'     be evaluated because the column their condition tests was not among the
#'     columns supplied. Populated by [qc_run()] with `partial = TRUE` and by
#'     [qc_check_variable()], which only ever sees one column. Such a rule is
#'     dropped rather than applied as if unconditional, so this attribute is
#'     the only record that a bound exists but went unchecked.}
#'   \item{`status`}{The worst severity present: `"fail"` > `"warn"` >
#'     `"pass"`.}
#' }
#'
#' @name qc_issues
#' @keywords internal
NULL
