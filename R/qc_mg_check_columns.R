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
#' @param detail Logical. If `TRUE` (default), the `failures` element of the
#'   returned list contains a data frame describing each problem. If `FALSE`,
#'   `failures` is `NULL`, which reduces memory use in automated pipelines.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_columns"`.}
#'     \item{`status`}{Character. One of `"pass"`, `"warn"`, or `"fail"`.
#'       `"fail"` if any expected columns are missing or any extra columns are
#'       present; `"warn"` if all expected columns are present (with no extras)
#'       but in the wrong order; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with counts: `n_expected`, `n_present`,
#'       `n_missing`, `n_extra`, and a logical `order_correct` flag.}
#'     \item{`failures`}{Data frame with columns `column_name` and `issue`
#'       (values `"missing"` or `"extra"`), or `NULL` if `status == "pass"` or
#'       `detail == FALSE`.}
#'   }
#'
#' @details
#' Columns in `data` that are not listed in `expected_columns` are treated as
#' extra and produce a `"fail"` result. Order is evaluated only when all
#' expected columns are present and no extra columns exist.
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
#' # All columns present and in order -> pass
#' qc_check_columns(df, c("site", "date", "cover"))
#'
#' # Missing column -> fail
#' qc_check_columns(df, c("site", "date", "cover", "species"))
#'
#' # Wrong order -> warn
#' qc_check_columns(df, c("date", "site", "cover"))
#'
#' # Extra column -> fail
#' qc_check_columns(df, c("site", "date"))
qc_check_columns <- function(data, expected_columns, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(expected_columns)) {
    stop("`expected_columns` must be a character vector.")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  actual_cols      <- colnames(data)
  missing_cols     <- setdiff(expected_columns, actual_cols)
  extra_cols       <- setdiff(actual_cols, expected_columns)
  present_expected <- expected_columns[expected_columns %in% actual_cols]

  n_expected <- length(expected_columns)
  n_missing  <- length(missing_cols)
  n_extra    <- length(extra_cols)
  n_present  <- n_expected - n_missing

  # Order of present expected columns as they appear in data
  actual_order <- actual_cols[actual_cols %in% expected_columns]
  order_correct <- identical(actual_order, present_expected)

  # --- Determine status and build failures ------------------------------------
  if (n_missing > 0 || n_extra > 0) {
    status <- "fail"
    parts <- c(
      if (n_missing > 0) paste0(n_missing, " expected column(s) missing: ",  paste(missing_cols, collapse = ", ")),
      if (n_extra  > 0) paste0(n_extra,  " unexpected column(s) present: ", paste(extra_cols,   collapse = ", "))
    )
    msg <- paste(parts, collapse = "; ")
    failures <- if (detail) {
      rbind(
        if (n_missing > 0) data.frame(column_name = missing_cols, issue = "missing", stringsAsFactors = FALSE),
        if (n_extra  > 0) data.frame(column_name = extra_cols,   issue = "extra",   stringsAsFactors = FALSE)
      )
    } else {
      NULL
    }

  } else if (!order_correct) {
    status <- "warn"
    mismatched <- which(actual_order != present_expected)
    msg <- paste0(
      "All expected columns present but ",
      length(mismatched), " column(s) are out of order."
    )
    failures <- if (detail) {
      data.frame(
        position         = mismatched,
        expected_column  = present_expected[mismatched],
        actual_column    = actual_order[mismatched],
        issue            = "wrong_order",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

  } else {
    status  <- "pass"
    msg     <- paste0(
      "All ", n_expected, " expected column(s) present and in correct order."
    )
    failures <- NULL
  }

  list(
    test     = "qc_check_columns",
    status   = status,
    message  = msg,
    summary  = data.frame(
      n_expected    = n_expected,
      n_present     = n_present,
      n_missing     = n_missing,
      n_extra       = n_extra,
      order_correct = order_correct,
      stringsAsFactors = FALSE
    ),
    failures = failures
  )
}
