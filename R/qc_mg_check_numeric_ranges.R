#' Check numeric column values against expected ranges
#'
#' @description
#' Validates that numeric columns in a data frame fall within expected bounds
#' defined in `numeric_ranges` metadata. Supports both inclusive and exclusive
#' range types, separate fail and warn thresholds, and per-column NA-safe
#' evaluation. Designed for use in MarineGEO QA/QC pipelines and can be called
#' directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#' @param rules A data frame with columns `column_name`, `max_fail`,
#'   `min_fail`, `max_warn`, `min_warn`, and `range_type`. Each row specifies
#'   the range rule for one column:
#'   \itemize{
#'     \item `range_type == "inclusive"`: fail if `value > max_fail` or
#'       `value < min_fail`; warn if outside warn bounds but within fail bounds.
#'     \item `range_type == "exclusive"`: fail if `value >= max_fail` or
#'       `value <= min_fail`; warn if outside warn bounds but within fail bounds.
#'     \item `range_type == NA`: row is silently skipped.
#'   }
#'   Bound columns (`max_fail`, `min_fail`, `max_warn`, `min_warn`) may be `NA`
#'   individually — only non-`NA` bounds are applied.
#' @param detail Logical. If `TRUE` (default), the `failures` element contains
#'   a data frame with the row and column indices of each out-of-range value.
#'   If `FALSE`, `failures` is `NULL`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_numeric_ranges"`.}
#'     \item{`status`}{Character. One of `"pass"`, `"warn"`, or `"fail"`.
#'       `"fail"` if any fail threshold is breached; `"warn"` if only warn
#'       thresholds are breached; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with one row per validated column, showing
#'       `column_name`, `n_fail`, and `n_warn`.}
#'     \item{`failures`}{Data frame with columns `row_index`, `col_index`,
#'       `column_name`, `value`, and `severity` (`"fail"` or `"warn"`), or
#'       `NULL` if `status == "pass"` or `detail == FALSE`. Indices are
#'       1-based (R convention).}
#'   }
#'
#' @details
#' Only columns named in `rules$column_name` that also exist in `data` are
#' validated; other columns are silently skipped. Rows in `rules` with
#' `range_type == NA` are also silently skipped.
#'
#' `NA` values in the data column being evaluated are silently ignored and not
#' counted as violations.
#'
#' Fail thresholds are evaluated first. A row that breaches a fail threshold is
#' recorded as `"fail"` and is not additionally evaluated for warn. A row that
#' does not breach any fail threshold but breaches a warn threshold is recorded
#' as `"warn"`.
#'
#' `col_index` reflects the column's position in the original `data` argument,
#' not its rank among only the validated columns.
#'
#' This function is called automatically by [qc_run()] when `numeric_ranges`
#' metadata exists for the given `table_id` and has non-`NA` `range_type`
#' values.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   percent_cover = c(10, 50, 110, -5),
#'   depth_m       = c(1, 5, 20, 3)
#' )
#'
#' rules <- data.frame(
#'   column_name = c("percent_cover", "depth_m"),
#'   min_fail    = c(0,  NA),
#'   max_fail    = c(100, 50),
#'   min_warn    = c(NA, NA),
#'   max_warn    = c(80,  30),
#'   range_type  = c("inclusive", "inclusive"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # percent_cover 110 > 100 (fail), -5 < 0 (fail), 50 > 80 (warn within fail)
#' qc_check_numeric_ranges(df, rules)
qc_check_numeric_ranges <- function(data, rules, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop(
      "`rules` must be a data frame with columns `column_name`, `max_fail`, ",
      "`min_fail`, `max_warn`, `min_warn`, and `range_type`."
    )
  }
  required_cols <- c("column_name", "max_fail", "min_fail", "max_warn",
                      "min_warn", "range_type")
  if (!all(required_cols %in% colnames(rules))) {
    stop(
      "`rules` must have columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # Keep only rows with an actionable range_type
  active_rules <- rules[!is.na(rules$range_type), ]

  # Intersect with columns present in data
  cols_to_check <- intersect(active_rules$column_name, colnames(data))

  if (length(cols_to_check) == 0) {
    return(list(
      test     = "qc_check_numeric_ranges",
      status   = "pass",
      message  = "No columns to validate for numeric ranges.",
      summary  = data.frame(
        column_name = character(0),
        n_fail      = integer(0),
        n_warn      = integer(0),
        stringsAsFactors = FALSE
      ),
      failures = NULL
    ))
  }

  summary_list  <- vector("list", length(cols_to_check))
  failures_list <- vector("list", length(cols_to_check))

  for (i in seq_along(cols_to_check)) {
    col       <- cols_to_check[[i]]
    col_pos   <- which(colnames(data) == col)
    rule      <- active_rules[active_rules$column_name == col, ][1, ]
    rtype     <- rule$range_type
    values    <- data[[col]]

    # Only evaluate non-NA values
    valid_idx <- which(!is.na(values))
    v         <- values[valid_idx]

    # Determine fail violations
    fail_mask <- rep(FALSE, length(v))
    if (!is.na(rule$max_fail)) {
      if (rtype == "inclusive") {
        fail_mask <- fail_mask | (v > rule$max_fail)
      } else {
        fail_mask <- fail_mask | (v >= rule$max_fail)
      }
    }
    if (!is.na(rule$min_fail)) {
      if (rtype == "inclusive") {
        fail_mask <- fail_mask | (v < rule$min_fail)
      } else {
        fail_mask <- fail_mask | (v <= rule$min_fail)
      }
    }

    # Determine warn violations (only for rows not already failing)
    warn_mask <- rep(FALSE, length(v))
    if (!is.na(rule$max_warn)) {
      if (rtype == "inclusive") {
        warn_mask <- warn_mask | (v > rule$max_warn)
      } else {
        warn_mask <- warn_mask | (v >= rule$max_warn)
      }
    }
    if (!is.na(rule$min_warn)) {
      if (rtype == "inclusive") {
        warn_mask <- warn_mask | (v < rule$min_warn)
      } else {
        warn_mask <- warn_mask | (v <= rule$min_warn)
      }
    }
    # Warn only if not already a fail
    warn_mask <- warn_mask & !fail_mask

    n_fail <- sum(fail_mask)
    n_warn <- sum(warn_mask)

    summary_list[[i]] <- data.frame(
      column_name = col,
      n_fail      = n_fail,
      n_warn      = n_warn,
      stringsAsFactors = FALSE
    )

    if ((n_fail > 0 || n_warn > 0) && detail) {
      fail_rows <- valid_idx[fail_mask]
      warn_rows <- valid_idx[warn_mask]

      fail_df <- if (n_fail > 0) {
        data.frame(
          row_index   = fail_rows,
          col_index   = col_pos,
          column_name = col,
          value       = values[fail_rows],
          severity    = "fail",
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }

      warn_df <- if (n_warn > 0) {
        data.frame(
          row_index   = warn_rows,
          col_index   = col_pos,
          column_name = col,
          value       = values[warn_rows],
          severity    = "warn",
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }

      failures_list[[i]] <- rbind(fail_df, warn_df)
    }
  }

  summary_df  <- do.call(rbind, summary_list)
  failures_df <- do.call(rbind, Filter(Negate(is.null), failures_list))

  total_fail <- sum(summary_df$n_fail)
  total_warn <- sum(summary_df$n_warn)

  if (total_fail > 0) {
    status   <- "fail"
    bad_cols <- summary_df$column_name[summary_df$n_fail > 0]
    msg <- paste0(
      total_fail, " value(s) exceed fail thresholds across ",
      length(bad_cols), " column(s): ",
      paste(bad_cols, collapse = ", ")
    )
  } else if (total_warn > 0) {
    status   <- "warn"
    bad_cols <- summary_df$column_name[summary_df$n_warn > 0]
    msg <- paste0(
      total_warn, " value(s) outside warn thresholds across ",
      length(bad_cols), " column(s): ",
      paste(bad_cols, collapse = ", ")
    )
  } else {
    status      <- "pass"
    msg         <- paste0(
      "All values in ", length(cols_to_check),
      " numeric column(s) are within expected ranges."
    )
    failures_df <- NULL
  }

  list(
    test     = "qc_check_numeric_ranges",
    status   = status,
    message  = msg,
    summary  = summary_df,
    failures = if (detail) failures_df else NULL
  )
}
