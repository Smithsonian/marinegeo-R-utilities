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
#'
#' @return A [qc_issues] tibble with one row per offending `NA` cell
#'   (`issue = "missing_value"`), or zero rows if no required values are
#'   missing. Cells in `"enforce"` columns are `"fail"` rows; cells in `"warn"`
#'   columns are `"warn"` rows. `row` and `col_index` are 1-based; `value` is
#'   `NA` (the cell is empty by definition).
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
#' # site_name has an NA and is "enforce" -> one fail row
#' qc_check_missing_values(df, rules)
qc_check_missing_values <- function(data, rules) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.data.frame(rules)) {
    stop(
      "`rules` must be a data frame with columns `column_name` and `missing_values`."
    )
  }
  if (!all(c("column_name", "missing_values") %in% colnames(rules))) {
    stop("`rules` must have columns `column_name` and `missing_values`.")
  }

  active_rules <- rules[rules$missing_values %in% c("enforce", "warn"), ]
  cols_to_check <- intersect(active_rules$column_name, colnames(data))

  chunks <- lapply(cols_to_check, function(col) {
    col_pos <- which(colnames(data) == col)
    rule <- active_rules$missing_values[active_rules$column_name == col][1]
    na_idx <- which(is.na(data[[col]]))

    if (length(na_idx) == 0L) {
      return(NULL)
    }

    severity <- if (rule == "enforce") "fail" else "warn"
    message <- if (rule == "enforce") {
      paste0("Required value missing in column '", col, "'.")
    } else {
      paste0("Missing value in column '", col, "' where data is discouraged.")
    }

    .qc_issue(
      check = "qc_check_missing_values",
      severity = severity,
      issue = "missing_value",
      message = message,
      row = na_idx,
      column = col,
      col_index = col_pos
    )
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_missing_values"
  )
}
