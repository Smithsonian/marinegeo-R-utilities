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
#'
#' @return A [qc_issues] tibble with one `"fail"` row per offending cell
#'   (`issue = "invalid_category"`), or zero rows if every value matches the
#'   controlled vocabulary. `row` and `col_index` are 1-based (R convention);
#'   DT/DataTables users should subtract 1 for JavaScript cell callbacks.
#'
#' @details
#' Only columns named in `rules$column_name` that also exist in `data` are
#' validated; other columns are silently skipped. `NA` values in a categorical
#' column are silently ignored and not counted as violations.
#'
#' `col_index` reflects the column's position in the original `data` argument
#' (`which(colnames(data) == column_name)`), not its rank among only the
#' validated columns, so it remains correct when `data` contains unvalidated
#' columns.
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
#' # "unknown" is not in the allowed set -> one fail row
#' qc_check_categorical_values(df, rules)
qc_check_categorical_values <- function(data, rules) {
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

  rule_cols <- unique(rules$column_name)
  cols_to_check <- intersect(rule_cols, colnames(data))

  chunks <- lapply(cols_to_check, function(col) {
    col_pos <- which(colnames(data) == col)
    allowed <- rules$value[rules$column_name == col]
    actual <- data[[col]]
    bad_idx <- which(!(actual %in% allowed) & !is.na(actual))

    if (length(bad_idx) == 0L) {
      return(NULL)
    }

    bad_vals <- as.character(actual[bad_idx])
    .qc_issue(
      check = "qc_check_categorical_values",
      severity = "fail",
      issue = "invalid_category",
      message = paste0(
        "Value '",
        bad_vals,
        "' in column '",
        col,
        "' is not an allowed category."
      ),
      row = bad_idx,
      column = col,
      col_index = col_pos,
      value = bad_vals
    )
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_categorical_values"
  )
}
