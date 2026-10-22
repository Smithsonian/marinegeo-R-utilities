# ---------------------------------------------------------------------------
# Console reporting for QC runs.
#
# A `qc_issues` result carries one row per problem, so a clean run returns zero
# rows and tells an interactive user nothing. These helpers turn a finished
# result into a human-readable block: one line per check that ran, plus a
# summary footer. Everything is derived from the returned object — a check
# listed in `checks_run` with no rows in the `check` column passed — so there is
# a single call site per entry point and the dispatcher stays untouched.
#
# Informational only: the report never changes the result or raises a condition
# other than the message itself.
# ---------------------------------------------------------------------------

# Width of the PASS/WARN/FAIL label column, so check names line up.
.qc_report_label_width <- 4L

# Collapse a character vector for display, tolerating NULL and zero length.
.qc_report_join <- function(x) {
  paste(as.character(x), collapse = ", ")
}

# Build the report as a character vector of lines. Pure: no side effects, so
# tests can assert on exact strings without capturing conditions. Reads only
# attributes and the `check`/`severity` columns, and tolerates the NULL
# attributes left by an object built by a bare qc_check_* function.
.qc_report_lines <- function(x) {
  table_id <- attr(x, "table_id")
  n_rows <- attr(x, "n_rows")
  checks_run <- attr(x, "checks_run")
  checks_skipped <- attr(x, "checks_skipped")
  rules_skipped <- attr(x, "rules_skipped")

  # `table_id` is a vector for a qc_run_dataset() result.
  label <- if (length(table_id) > 0) {
    .qc_report_join(table_id)
  } else {
    "<unknown table>"
  }
  if (is.null(n_rows)) {
    n_rows <- nrow(x)
  }

  lines <- sprintf(
    "QC: %s (%d rows, %d checks)",
    label,
    as.integer(n_rows),
    length(checks_run)
  )

  # One line per check, in dispatch order (the attribute preserves it). A check
  # with zero rows is a pass: .qc_status() already returns "pass" for that.
  for (nm in checks_run) {
    rows <- x[!is.na(x$check) & x$check == nm, , drop = FALSE]
    n_issues <- nrow(rows)
    status <- toupper(.qc_status(rows))
    line <- sprintf(
      "  %-*s  %s",
      .qc_report_label_width,
      status,
      nm
    )
    if (n_issues > 0) {
      line <- sprintf(
        "%s (%d issue%s)",
        line,
        n_issues,
        if (n_issues == 1) "" else "s"
      )
    }
    lines <- c(lines, line)
  }

  if (length(checks_skipped) > 0) {
    lines <- c(lines, paste0("  skipped: ", .qc_report_join(checks_skipped)))
  }
  # A conditional rule that could not be evaluated: a bound exists but went
  # unchecked, which is exactly what a human needs to be told.
  if (length(rules_skipped) > 0) {
    lines <- c(
      lines,
      paste0("  rules_skipped: ", .qc_report_join(rules_skipped))
    )
  }

  # Footer. Nothing having run is not a pass — it is the case this whole report
  # exists to make visible.
  if (length(checks_run) == 0) {
    lines <- c(lines, "Status: NOT CHECKED - no checks ran")
    return(lines)
  }

  n_issues <- nrow(x)
  if (n_issues == 0) {
    lines <- c(lines, "Status: PASS - no issues")
  } else {
    n_checks <- length(unique(x$check[!is.na(x$check)]))
    lines <- c(
      lines,
      sprintf(
        "Status: %s - %d issue%s across %d check%s",
        toupper(.qc_status(x)),
        n_issues,
        if (n_issues == 1) "" else "s",
        n_checks,
        if (n_checks == 1) "" else "s"
      )
    )
  }
  lines
}

# Emit the report. One message() for the whole block keeps it a single
# condition object — tidier in logs, simpler to assert on. Returns `x`
# invisibly so it can be dropped into a pipeline without altering the result.
.qc_report <- function(x, verbose = TRUE) {
  if (isTRUE(verbose)) {
    message(paste(.qc_report_lines(x), collapse = "\n"))
  }
  invisible(x)
}
