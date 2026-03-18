#' Summarize QC run results into tidy data frames
#'
#' @description
#' Flattens the nested list returned by [qc_run()] into one or two tidy data
#' frames that are easier to read and display. Useful for reporting, logging,
#' and interactive review of QC results.
#'
#' @param qc_result A named list as returned by [qc_run()]. Must contain a
#'   `$tests` element (itself a named list of per-test result lists).
#' @param type Character scalar. Controls which summary objects are returned.
#'   One of `"summary"`, `"failures"`, or `"both"` (default).
#'
#' @return A named list. Contents depend on `type`:
#'
#'   - `"summary"` → `list(summary = <data frame>)` with one row per test and
#'     columns `test`, `status`, `message`, `n_failures`.
#'   - `"failures"` → `list(failures = <data frame>)` with all per-test failure
#'     rows bound together and a leading `test` column. Zero-row data frame when
#'     there are no failures.
#'   - `"both"` → both of the above.
#'   - Empty `$tests` → `list()` (with a message).
#'
#' @details
#' **`n_failures` values in the summary table:**
#' - `0L` when the test status is `"pass"`.
#' - Row count of `$failures` when the test produced a non-NULL, non-empty
#'   failures data frame.
#' - `NA_integer_` for non-passing tests where `$failures` is `NULL`, which
#'   occurs when `qc_run()` was called with `detail = FALSE`.
#'
#' **Failures table columns:**
#' Individual tests may record different columns in their `$failures` data
#' frames. Before binding, all columns except `row_index` and `col_index` are
#' coerced to character to avoid type conflicts (e.g. when `value` is numeric
#' in one test and character in another). Columns not present in a given test
#' are filled with `NA`.
#'
#' @examples
#' result <- list(
#'   table_id = "demo",
#'   status = "fail",
#'   n_rows = 10L,
#'   tests = list(
#'     qc_check_columns = list(
#'       test = "qc_check_columns", status = "pass",
#'       message = "All columns present.",
#'       summary = data.frame(n_expected = 3L, n_present = 3L),
#'       failures = NULL
#'     ),
#'     qc_check_data_types = list(
#'       test = "qc_check_data_types", status = "fail",
#'       message = "2 type mismatches.",
#'       summary = data.frame(n_checked = 3L, n_type_mismatches = 2L),
#'       failures = data.frame(
#'         column_name = c("value", "date"),
#'         issue = c("type_mismatch", "type_mismatch")
#'       )
#'     )
#'   )
#' )
#'
#' utl_qc_summarize(result)
#' utl_qc_summarize(result, type = "summary")
#' utl_qc_summarize(result, type = "failures")
#'
#' @export
utl_qc_summarize <- function(qc_result, type = "both") {
  # --- Input validation ------------------------------------------------------
  if (!is.list(qc_result) || is.null(qc_result$tests)) {
    stop(
      "`qc_result` must be a list with a `$tests` element, ",
      "as returned by `qc_run()`.",
      call. = FALSE
    )
  }

  if (!is.character(type) || length(type) != 1L ||
      !type %in% c("summary", "failures", "both")) {
    stop(
      '`type` must be a single character value: "summary", "failures", or "both".',
      call. = FALSE
    )
  }

  tests <- qc_result$tests

  # --- Empty tests -----------------------------------------------------------
  if (length(tests) == 0L) {
    message("No tests found in `qc_result$tests`. Returning empty list.")
    return(list())
  }

  # --- Build summary table ---------------------------------------------------
  if (type %in% c("summary", "both")) {
    summary_rows <- lapply(tests, function(tr) {
      n_fail <- if (identical(tr$status, "pass")) {
        0L
      } else if (!is.null(tr$failures) && nrow(tr$failures) > 0L) {
        nrow(tr$failures)
      } else if (!is.null(tr$failures) && nrow(tr$failures) == 0L) {
        0L
      } else {
        NA_integer_
      }

      data.frame(
        test       = tr$test,
        status     = tr$status,
        message    = tr$message,
        n_failures = n_fail,
        stringsAsFactors = FALSE
      )
    })

    summary_df <- dplyr::bind_rows(summary_rows)
  }

  # --- Build failures table --------------------------------------------------
  if (type %in% c("failures", "both")) {
    failure_frames <- lapply(tests, function(tr) {
      if (!is.null(tr$failures) && nrow(tr$failures) > 0L) {
        dplyr::mutate(tr$failures, test = tr$test, .before = 1)
      } else {
        NULL
      }
    })

    # Remove NULLs before binding
    failure_frames <- Filter(Negate(is.null), failure_frames)

    if (length(failure_frames) == 0L) {
      failures_df <- dplyr::tibble()
    } else {
      # Coerce columns to character before binding to handle cases where shared
      # columns (e.g. `value`) have different types across tests. `row_index`
      # and `col_index` are always numeric and are excluded from coercion.
      failure_frames <- lapply(failure_frames, function(df) {
        dplyr::mutate(
          df,
          dplyr::across(
            -dplyr::any_of(c("row_index", "col_index")),
            as.character
          )
        )
      })
      failures_df <- dplyr::bind_rows(failure_frames)
    }
  }

  # --- Assemble return value -------------------------------------------------
  switch(
    type,
    summary  = list(summary  = summary_df),
    failures = list(failures = failures_df),
    both     = list(summary  = summary_df, failures = failures_df)
  )
}
