#' Check categorical column values against controlled vocabularies
#'
#' @description
#' Validates that columns in a data frame contain only values listed in the
#' MarineGEO controlled vocabulary (`categorical_values` metadata). Designed
#' for use in MarineGEO QA/QC pipelines and can be called directly or via
#' [qc_run()].
#'
#' @param data A data frame to validate.
#' @param rules A data frame with columns `column_name` and `value`. Each row
#'   specifies one allowed value for one column. Multiple rows with the same
#'   `column_name` define the complete allowed set for that column.
#' @param detail Logical. If `TRUE` (default), the `failures` element contains
#'   a data frame with the row indices and values that violate the controlled
#'   vocabulary. If `FALSE`, `failures` is `NULL`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_categorical_values"`.}
#'     \item{`status`}{Character. One of `"pass"` or `"fail"`.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with one row per validated column, showing
#'       `column_name`, `n_allowed_values`, and `n_violations`.}
#'     \item{`failures`}{Data frame with columns `row_index`, `col_index`,
#'       `column_name`, and `value`, or `NULL` if `status == "pass"` or
#'       `detail == FALSE`. Both indices are 1-based (R convention); DT/DataTables
#'       users should subtract 1 from each for JavaScript cell callbacks.}
#'   }
#'
#' @details
#' Only columns named in `rules$column_name` that also exist in `data` are
#' validated; other columns are silently skipped. `NA` values in a categorical
#' column are silently ignored and not counted as violations.
#'
#' `col_index` reflects the column's position in the original `data` argument
#' (`which(colnames(data) == column_name)`), not its rank among only the
#' validated columns, so it remains correct when `data` contains unvalidated columns.
#'
#' This function is called automatically by [qc_run()] when categorical value
#' metadata exists for the given `table_id`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   habitat = c("seagrass", "coral", "unknown"),
#'   stringsAsFactors = FALSE
#' )
#'
#' rules <- data.frame(
#'   column_name = c("habitat", "habitat"),
#'   value       = c("seagrass", "coral"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # "unknown" is not in the allowed set -> fail
#' qc_check_categorical_values(df, rules)
qc_check_categorical_values <- function(data, rules, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop("`rules` must be a data frame with columns `column_name` and `value`.")
  }
  if (!all(c("column_name", "value") %in% colnames(rules))) {
    stop("`rules` must have columns `column_name` and `value`.")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # Columns to validate: those in rules that also exist in data
  rule_cols     <- unique(rules$column_name)
  cols_to_check <- intersect(rule_cols, colnames(data))

  if (length(cols_to_check) == 0) {
    return(list(
      test     = "qc_check_categorical_values",
      status   = "pass",
      message  = "No categorical columns to validate (none found in data).",
      summary  = data.frame(
        column_name      = character(0),
        n_allowed_values = integer(0),
        n_violations     = integer(0),
        stringsAsFactors = FALSE
      ),
      failures = NULL
    ))
  }

  summary_list  <- vector("list", length(cols_to_check))
  failures_list <- vector("list", length(cols_to_check))

  for (i in seq_along(cols_to_check)) {
    col     <- cols_to_check[[i]]
    col_pos <- which(colnames(data) == col)
    allowed <- rules$value[rules$column_name == col]
    actual  <- data[[col]]

    bad_idx <- which(!(actual %in% allowed) & !is.na(actual))

    summary_list[[i]] <- data.frame(
      column_name      = col,
      n_allowed_values = length(allowed),
      n_violations     = length(bad_idx),
      stringsAsFactors = FALSE
    )

    if (length(bad_idx) > 0 && detail) {
      failures_list[[i]] <- data.frame(
        row_index        = bad_idx,
        col_index        = col_pos,
        column_name      = col,
        value            = as.character(actual[bad_idx]),
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df       <- do.call(rbind, summary_list)
  failures_df      <- do.call(rbind, Filter(Negate(is.null), failures_list))
  total_violations <- sum(summary_df$n_violations)

  if (total_violations > 0) {
    status   <- "fail"
    bad_cols <- summary_df$column_name[summary_df$n_violations > 0]
    msg <- paste0(
      total_violations, " invalid value(s) found across ",
      length(bad_cols), " column(s): ",
      paste(bad_cols, collapse = ", ")
    )
  } else {
    status      <- "pass"
    msg         <- paste0(
      "All values in ", length(cols_to_check),
      " categorical column(s) match the controlled vocabulary."
    )
    failures_df <- NULL
  }

  list(
    test     = "qc_check_categorical_values",
    status   = status,
    message  = msg,
    summary  = summary_df,
    failures = if (detail) failures_df else NULL
  )
}
