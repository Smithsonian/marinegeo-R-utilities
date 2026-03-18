#' Check for missing values against column-level rules
#'
#' @description
#' Validates that columns in a data frame do not contain `NA` values where
#' missing data is prohibited or flagged. Designed for use in MarineGEO QA/QC
#' pipelines and can be called directly or via [qc_run()].
#'
#' @param data A data frame to validate.
#' @param rules A data frame with columns `column_name` and `missing_values`.
#'   Each row specifies the missing-value rule for one column:
#'   \itemize{
#'     \item `"enforce"` — `NA` values are a **fail**.
#'     \item `"warn"` — `NA` values produce a **warn**.
#'     \item `"allow"` or `NA` — column is skipped (caller should pre-filter,
#'       but these rows are silently ignored if present).
#'   }
#' @param detail Logical. If `TRUE` (default), the `failures` element contains
#'   a data frame with the row and column indices of each `NA` cell. If
#'   `FALSE`, `failures` is `NULL`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_missing_values"`.}
#'     \item{`status`}{Character. One of `"pass"`, `"warn"`, or `"fail"`.
#'       `"fail"` if any `"enforce"` column contains `NA`; `"warn"` if only
#'       `"warn"` columns contain `NA`; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with one row per validated column, showing
#'       `column_name`, `missing_rule`, and `n_missing`.}
#'     \item{`failures`}{Data frame with columns `row_index`, `col_index`,
#'       `column_name`, and `severity` (`"fail"` for `"enforce"` columns,
#'       `"warn"` for `"warn"` columns), or `NULL` if `status == "pass"` or
#'       `detail == FALSE`. Indices are 1-based (R convention).}
#'   }
#'
#' @details
#' Only columns named in `rules$column_name` that also exist in `data` are
#' validated; other columns are silently skipped. Rows with `missing_values`
#' equal to `"allow"` or `NA` are also silently skipped.
#'
#' `col_index` reflects the column's position in the original `data` argument,
#' not its rank among only the validated columns.
#'
#' This function is called automatically by [qc_run()] when the
#' `database_structure` metadata contains `"enforce"` or `"warn"` rules for
#' the given `table_id`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   site_name   = c("A", NA, "C"),
#'   survey_date = c("2024-01-01", "2024-01-02", NA),
#'   cover       = c(10, 20, 30)
#' )
#'
#' rules <- data.frame(
#'   column_name    = c("site_name", "survey_date", "cover"),
#'   missing_values = c("enforce", "warn", "allow"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # site_name has an NA and is "enforce" -> fail
#' qc_check_missing_values(df, rules)
qc_check_missing_values <- function(data, rules, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop("`rules` must be a data frame with columns `column_name` and `missing_values`.")
  }
  if (!all(c("column_name", "missing_values") %in% colnames(rules))) {
    stop("`rules` must have columns `column_name` and `missing_values`.")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # Keep only actionable rules
  active_rules <- rules[rules$missing_values %in% c("enforce", "warn"), ]

  # Intersect with columns present in data
  cols_to_check <- intersect(active_rules$column_name, colnames(data))

  if (length(cols_to_check) == 0) {
    return(list(
      test     = "qc_check_missing_values",
      status   = "pass",
      message  = "No columns to validate for missing values.",
      summary  = data.frame(
        column_name  = character(0),
        missing_rule = character(0),
        n_missing    = integer(0),
        stringsAsFactors = FALSE
      ),
      failures = NULL
    ))
  }

  summary_list  <- vector("list", length(cols_to_check))
  failures_list <- vector("list", length(cols_to_check))

  for (i in seq_along(cols_to_check)) {
    col      <- cols_to_check[[i]]
    col_pos  <- which(colnames(data) == col)
    rule     <- active_rules$missing_values[active_rules$column_name == col][1]
    na_idx   <- which(is.na(data[[col]]))

    summary_list[[i]] <- data.frame(
      column_name  = col,
      missing_rule = rule,
      n_missing    = length(na_idx),
      stringsAsFactors = FALSE
    )

    if (length(na_idx) > 0 && detail) {
      failures_list[[i]] <- data.frame(
        row_index   = na_idx,
        col_index   = col_pos,
        column_name = col,
        severity    = if (rule == "enforce") "fail" else "warn",
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df  <- do.call(rbind, summary_list)
  failures_df <- do.call(rbind, Filter(Negate(is.null), failures_list))

  # Determine status: fail > warn > pass
  has_fail <- any(
    summary_df$n_missing > 0 & summary_df$missing_rule == "enforce"
  )
  has_warn <- any(
    summary_df$n_missing > 0 & summary_df$missing_rule == "warn"
  )

  if (has_fail) {
    status   <- "fail"
    bad_cols <- summary_df$column_name[
      summary_df$n_missing > 0 & summary_df$missing_rule == "enforce"
    ]
    msg <- paste0(
      "Missing values found in ", length(bad_cols),
      " required column(s): ", paste(bad_cols, collapse = ", ")
    )
  } else if (has_warn) {
    status   <- "warn"
    bad_cols <- summary_df$column_name[
      summary_df$n_missing > 0 & summary_df$missing_rule == "warn"
    ]
    msg <- paste0(
      "Missing values found in ", length(bad_cols),
      " column(s) where missing data is discouraged: ",
      paste(bad_cols, collapse = ", ")
    )
  } else {
    status      <- "pass"
    msg         <- paste0(
      "No missing values found in ", length(cols_to_check),
      " validated column(s)."
    )
    failures_df <- NULL
  }

  list(
    test     = "qc_check_missing_values",
    status   = status,
    message  = msg,
    summary  = summary_df,
    failures = if (detail) failures_df else NULL
  )
}
