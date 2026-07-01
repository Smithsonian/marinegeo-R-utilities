#' Check column data types against expected schema
#'
#' @description
#' Validates that columns in a data frame match the expected SQL-style data
#' types defined in the MarineGEO `database_structure` metadata. Designed for
#' use in MarineGEO QA/QC pipelines and can be called directly or via
#' [qc_run()].
#'
#' @param data A data frame to validate.
#' @param type_map Named character vector. Names are column names; values are
#'   SQL-style type strings: `"STRING"`, `"INT"`, `"TINYINT"`, `"DOUBLE"`,
#'   `"DATE"`, or `"BOOL"`. Columns in `data` not present in `type_map` are
#'   silently skipped.
#'
#' @return A [qc_issues] tibble with one row per column whose type does not
#'   match (zero rows if all types are correct). True mismatches are `"fail"`
#'   rows (`issue = "type_mismatch"`); a column that is entirely `NA` and stored
#'   as `logical` (a read artifact) is a `"warn"` row
#'   (`issue = "all_na_type"`). These are column-level issues, so `row` is `NA`
#'   and `value` carries the observed R type.
#'
#' @details
#' SQL-style types map to R types as follows:
#' \itemize{
#'   \item `STRING` — character
#'   \item `INT` / `TINYINT` — integer or numeric (double)
#'   \item `DOUBLE` — numeric (double)
#'   \item `DATE` — Date, POSIXct, or POSIXlt
#'   \item `BOOL` — logical
#' }
#' Columns in `type_map` that are absent from `data` are skipped silently
#' (use [qc_check_columns()] to catch missing columns). Unknown SQL type
#' strings are also skipped with no error.
#'
#' A column that is entirely `NA` and stored as `logical` (a common artifact
#' of `read_csv()` or `read_excel()` when no non-missing values are present)
#' is treated as a warning rather than a failure for non-`BOOL` expected types.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   site  = "A",
#'   depth = 5L,
#'   cover = 0.75,
#'   stringsAsFactors = FALSE
#' )
#'
#' # All types match -> zero issues
#' qc_check_data_types(df, c(site = "STRING", depth = "INT", cover = "DOUBLE"))
#'
#' # cover stored as character -> one fail row
#' df2 <- df
#' df2$cover <- "0.75"
#' qc_check_data_types(df2, c(site = "STRING", cover = "DOUBLE"))
qc_check_data_types <- function(data, type_map) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(type_map) || is.null(names(type_map))) {
    stop(
      "`type_map` must be a named character vector (column names -> SQL types)."
    )
  }

  cols_to_check <- intersect(names(type_map), colnames(data))

  chunks <- lapply(cols_to_check, function(col) {
    sql_type <- toupper(type_map[[col]])
    actual_col <- data[[col]]
    col_pos <- which(colnames(data) == col)

    # All-NA logical column — inferred-type artifact from read_csv/read_excel.
    if (
      is.logical(actual_col) && all(is.na(actual_col)) && sql_type != "BOOL"
    ) {
      return(.qc_issue(
        check = "qc_check_data_types",
        severity = "warn",
        issue = "all_na_type",
        message = paste0(
          "Column '",
          col,
          "' is entirely NA (type inferred as logical); expected ",
          sql_type,
          "."
        ),
        column = col,
        col_index = col_pos,
        value = "logical (all NA)"
      ))
    }

    if (!isTRUE(.type_check(actual_col, sql_type))) {
      actual_label <- .r_type_label(actual_col)
      return(.qc_issue(
        check = "qc_check_data_types",
        severity = "fail",
        issue = "type_mismatch",
        message = paste0(
          "Column '",
          col,
          "' expected ",
          sql_type,
          ", found ",
          actual_label,
          "."
        ),
        column = col,
        col_index = col_pos,
        value = actual_label
      ))
    }

    NULL
  })

  new_qc_issues(
    dplyr::bind_rows(chunks),
    n_rows = nrow(data),
    checks_run = "qc_check_data_types"
  )
}

# Internal: returns TRUE if col matches sql_type, FALSE otherwise.
.type_check <- function(col, sql_type) {
  switch(
    sql_type,
    "STRING" = is.character(col),
    "INT" = ,
    "TINYINT" = is.numeric(col) || is.integer(col),
    "DOUBLE" = is.numeric(col),
    "DATE" = inherits(col, c("Date", "POSIXct", "POSIXlt")),
    "BOOL" = is.logical(col),
    TRUE # unknown SQL type — skip (return pass)
  )
}

# Internal: return a readable R type label for a column vector.
.r_type_label <- function(col) {
  if (inherits(col, "Date")) {
    "Date"
  } else if (inherits(col, "POSIXct")) {
    "POSIXct"
  } else if (inherits(col, "POSIXlt")) {
    "POSIXlt"
  } else if (is.logical(col)) {
    "logical"
  } else if (is.integer(col)) {
    "integer"
  } else if (is.numeric(col)) {
    "numeric"
  } else if (is.character(col)) {
    "character"
  } else {
    paste(class(col), collapse = "/")
  }
}
