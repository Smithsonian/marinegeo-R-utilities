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
#' @param detail Logical. If `TRUE` (default), the `failures` element of the
#'   returned list contains a data frame describing each type mismatch. If
#'   `FALSE`, `failures` is `NULL`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`test`}{Character. Always `"qc_check_data_types"`.}
#'     \item{`status`}{Character. One of `"pass"`, `"warn"`, or `"fail"`.
#'       `"fail"` if any checked column has a true type mismatch; `"warn"` if
#'       any column is entirely `NA` and stored as `logical` (a read artifact)
#'       but no true mismatches exist; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with counts: `n_checked`,
#'       `n_type_mismatches`, and `n_type_warnings`.}
#'     \item{`failures`}{Data frame with columns `column_name`,
#'       `expected_type`, `actual_type`, `issue`, and `severity`
#'       (`"fail"` for true mismatches, `"warn"` for all-NA inferred-type
#'       columns), or `NULL` if `status == "pass"` or `detail == FALSE`.}
#'   }
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
#' The column appears in `$failures` with `severity = "warn"` and
#' `issue = "all_na_inferred_type"`.
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
#' # All types match -> pass
#' qc_check_data_types(df, c(site = "STRING", depth = "INT", cover = "DOUBLE"))
#'
#' # cover stored as character -> fail
#' df2 <- df
#' df2$cover <- "0.75"
#' qc_check_data_types(df2, c(site = "STRING", cover = "DOUBLE"))
qc_check_data_types <- function(data, type_map, detail = TRUE) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(type_map) || is.null(names(type_map))) {
    stop("`type_map` must be a named character vector (column names -> SQL types).")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # Only check columns present in data
  cols_to_check <- intersect(names(type_map), colnames(data))

  failures_list <- lapply(cols_to_check, function(col) {
    sql_type   <- toupper(type_map[[col]])
    actual_col <- data[[col]]

    # All-NA logical column — inferred type artifact from read_csv/read_excel
    if (is.logical(actual_col) && all(is.na(actual_col)) && sql_type != "BOOL") {
      return(data.frame(
        column_name      = col,
        expected_type    = sql_type,
        actual_type      = "logical (all NA)",
        issue            = "all_na_inferred_type",
        severity         = "warn",
        stringsAsFactors = FALSE
      ))
    }

    ok <- .type_check(actual_col, sql_type)

    if (!isTRUE(ok)) {
      data.frame(
        column_name      = col,
        expected_type    = sql_type,
        actual_type      = .r_type_label(actual_col),
        issue            = "type_mismatch",
        severity         = "fail",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  failures_df  <- do.call(rbind, Filter(Negate(is.null), failures_list))
  n_checked    <- length(cols_to_check)
  n_warn       <- if (is.null(failures_df)) 0L else sum(failures_df$severity == "warn")
  n_mismatches <- if (is.null(failures_df)) 0L else sum(failures_df$severity == "fail")

  if (n_mismatches > 0) {
    status <- "fail"
    fail_cols <- failures_df$column_name[failures_df$severity == "fail"]
    msg <- paste0(
      n_mismatches, " column(s) have unexpected data types: ",
      paste(fail_cols, collapse = ", ")
    )
  } else if (n_warn > 0) {
    status <- "warn"
    warn_cols <- failures_df$column_name[failures_df$severity == "warn"]
    msg <- paste0(
      n_warn, " column(s) are entirely NA (type inferred as logical): ",
      paste(warn_cols, collapse = ", ")
    )
  } else {
    status      <- "pass"
    msg         <- paste0("All ", n_checked, " checked column(s) have correct data types.")
    failures_df <- NULL
  }

  list(
    test     = "qc_check_data_types",
    status   = status,
    message  = msg,
    summary  = data.frame(
      n_checked         = n_checked,
      n_type_mismatches = n_mismatches,
      n_type_warnings   = n_warn,
      stringsAsFactors  = FALSE
    ),
    failures = if (detail) failures_df else NULL
  )
}

# Internal: returns TRUE if col matches sql_type, FALSE otherwise.
.type_check <- function(col, sql_type) {
  switch(sql_type,
    "STRING"  = is.character(col),
    "INT"     = ,
    "TINYINT" = is.numeric(col) || is.integer(col),
    "DOUBLE"  = is.numeric(col),
    "DATE"    = inherits(col, c("Date", "POSIXct", "POSIXlt")),
    "BOOL"    = is.logical(col),
    TRUE  # unknown SQL type — skip (return pass)
  )
}

# Internal: return a readable R type label for a column vector.
.r_type_label <- function(col) {
  if (inherits(col, "Date"))          "Date"
  else if (inherits(col, "POSIXct")) "POSIXct"
  else if (inherits(col, "POSIXlt")) "POSIXlt"
  else if (is.logical(col))          "logical"
  else if (is.integer(col))          "integer"
  else if (is.numeric(col))          "numeric"
  else if (is.character(col))        "character"
  else                                paste(class(col), collapse = "/")
}
