test_that("utl_qc_summarize: all tests pass", {
  result <- make_pass_result()

  out <- utl_qc_summarize(result)

  expect_named(out, c("summary", "failures"))

  # Summary table
  expect_s3_class(out$summary, "data.frame")
  expect_equal(nrow(out$summary), 2L)
  expect_equal(out$summary$status, c("pass", "pass"))
  expect_equal(out$summary$n_failures, c(0L, 0L))

  # Failures table is zero-row
  expect_s3_class(out$failures, "data.frame")
  expect_equal(nrow(out$failures), 0L)
})

test_that("utl_qc_summarize: one failing test with failures rows", {
  result <- make_fail_result()

  out <- utl_qc_summarize(result)

  # Summary: one pass, one fail
  expect_equal(out$summary$status, c("pass", "fail"))
  expect_equal(out$summary$n_failures, c(0L, 2L))

  # Failures table has the right rows and prepended test column
  expect_equal(nrow(out$failures), 2L)
  expect_true("test" %in% names(out$failures))
  expect_equal(out$failures$test[1], "qc_check_data_types")
})

test_that("utl_qc_summarize: mixed pass/warn/fail", {
  result <- make_mixed_result()

  out <- utl_qc_summarize(result)

  expect_equal(sort(out$summary$status), sort(c("pass", "warn", "fail")))
  # pass → 0, warn → 1 failure row, fail → 2 failure rows
  summary_sorted <- out$summary[order(out$summary$test), ]
  expect_equal(
    out$summary$n_failures[out$summary$status == "pass"], 0L
  )
  expect_equal(
    out$summary$n_failures[out$summary$status == "warn"], 1L
  )
  expect_equal(
    out$summary$n_failures[out$summary$status == "fail"], 2L
  )
  expect_equal(nrow(out$failures), 3L)
})

test_that("utl_qc_summarize: detail = FALSE gives NA_integer_ for n_failures", {
  result <- make_no_detail_result()

  out <- utl_qc_summarize(result)

  expect_true(is.na(out$summary$n_failures[out$summary$status == "fail"]))
  expect_equal(nrow(out$failures), 0L)
})

test_that("utl_qc_summarize: empty tests returns list() with message", {
  result <- make_empty_tests_result()

  expect_message(out <- utl_qc_summarize(result), "No tests found")
  expect_equal(out, list())
})

# --- type argument -----------------------------------------------------------

test_that("utl_qc_summarize: type = 'summary' returns only summary", {
  result <- make_fail_result()
  out <- utl_qc_summarize(result, type = "summary")

  expect_named(out, "summary")
  expect_false("failures" %in% names(out))
})

test_that("utl_qc_summarize: type = 'failures' returns only failures", {
  result <- make_fail_result()
  out <- utl_qc_summarize(result, type = "failures")

  expect_named(out, "failures")
  expect_false("summary" %in% names(out))
})

test_that("utl_qc_summarize: type = 'both' returns both", {
  result <- make_pass_result()
  out <- utl_qc_summarize(result, type = "both")

  expect_named(out, c("summary", "failures"))
})

# --- Summary table structure -------------------------------------------------

test_that("utl_qc_summarize: summary has required columns", {
  result <- make_pass_result()
  out <- utl_qc_summarize(result, type = "summary")

  expect_named(out$summary, c("test", "status", "message", "n_failures"))
})

test_that("utl_qc_summarize: summary has one row per test", {
  result <- make_mixed_result()
  out <- utl_qc_summarize(result, type = "summary")

  expect_equal(nrow(out$summary), length(result$tests))
})

# --- Failures table structure ------------------------------------------------

test_that("utl_qc_summarize: failures table has 'test' column first", {
  result <- make_fail_result()
  out <- utl_qc_summarize(result, type = "failures")

  expect_equal(names(out$failures)[1], "test")
})

test_that("utl_qc_summarize: heterogeneous failure columns filled with NA", {
  result <- make_heterogeneous_result()
  out <- utl_qc_summarize(result, type = "failures")

  # Both test columns should be present; rows missing one get NA
  expect_true("col_a" %in% names(out$failures))
  expect_true("col_b" %in% names(out$failures))
  expect_equal(nrow(out$failures), 2L)
})

test_that("utl_qc_summarize: zero-row failures when $failures is non-NULL but empty", {
  result <- make_empty_failures_result()
  out <- utl_qc_summarize(result)

  expect_equal(nrow(out$failures), 0L)
  expect_equal(out$summary$n_failures[out$summary$status == "fail"], 0L)
})

# --- Input validation --------------------------------------------------------

test_that("utl_qc_summarize: stops on non-list input", {
  expect_error(utl_qc_summarize("not a list"), "`qc_result` must be a list")
})

test_that("utl_qc_summarize: stops when $tests is missing", {
  expect_error(utl_qc_summarize(list(status = "fail")), "`qc_result` must be a list")
})

test_that("utl_qc_summarize: stops on invalid type", {
  result <- make_pass_result()
  expect_error(utl_qc_summarize(result, type = "bad"), "`type` must be")
  expect_error(utl_qc_summarize(result, type = c("summary", "failures")), "`type` must be")
  expect_error(utl_qc_summarize(result, type = 1L), "`type` must be")
})

