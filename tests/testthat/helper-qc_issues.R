# Shared helpers for QC issues-table tests.

qc_issue_cols <- c(
  "check",
  "severity",
  "row",
  "column",
  "col_index",
  "value",
  "issue",
  "message"
)

# Assert an object is a well-formed qc_issues table.
expect_qc_issues <- function(x) {
  testthat::expect_s3_class(x, "qc_issues")
  testthat::expect_identical(names(x), qc_issue_cols)
  invisible(x)
}

# Convenience accessor for the derived run status attribute.
qc_status <- function(x) attr(x, "status")
