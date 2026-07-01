# Tests for the qc_issues constructor and status derivation.

test_that(".qc_issue returns the canonical columns with correct types", {
  rows <- .qc_issue(
    check = "qc_check_columns",
    severity = "fail",
    issue = "missing_column",
    column = c("a", "b")
  )
  expect_identical(names(rows), qc_issue_cols)
  expect_type(rows$row, "integer")
  expect_type(rows$col_index, "integer")
  expect_type(rows$value, "character")
  expect_equal(nrow(rows), 2L) # scalar args recycled against the length-2 column
})

test_that("new_qc_issues() with no rows is a zero-row pass table", {
  x <- new_qc_issues()
  expect_qc_issues(x)
  expect_equal(nrow(x), 0L)
  expect_equal(attr(x, "status"), "pass")
})

test_that("new_qc_issues() prints as an ordinary tibble", {
  x <- new_qc_issues(
    table_id = "demo",
    n_rows = 1L,
    checks_run = "qc_check_columns"
  )
  out <- capture.output(expect_invisible(print(x)))
  expect_match(paste(out, collapse = "\n"), "tibble")
})

test_that("new_qc_issues() stores run metadata as attributes", {
  x <- new_qc_issues(
    table_id = "demo",
    n_rows = 42L,
    checks_run = c("qc_check_columns")
  )
  expect_equal(attr(x, "table_id"), "demo")
  expect_equal(attr(x, "n_rows"), 42L)
  expect_equal(attr(x, "checks_run"), "qc_check_columns")
})

test_that(".qc_status derives fail > warn > pass", {
  expect_equal(.qc_status(.qc_issue("c", "warn", "i")), "warn")
  expect_equal(.qc_status(.qc_issue("c", "fail", "i")), "fail")
  both <- dplyr::bind_rows(
    .qc_issue("c", "warn", "i"),
    .qc_issue("c", "fail", "i")
  )
  expect_equal(.qc_status(both), "fail")
  expect_equal(.qc_status(.qc_issues_skeleton()), "pass")
})

test_that("status attribute reflects the bound rows", {
  rows <- dplyr::bind_rows(
    .qc_issue("c", "warn", "i", row = 1L),
    .qc_issue("c", "fail", "i", row = 2L)
  )
  x <- new_qc_issues(rows, table_id = "demo", n_rows = 5L)
  expect_equal(attr(x, "status"), "fail")
})
