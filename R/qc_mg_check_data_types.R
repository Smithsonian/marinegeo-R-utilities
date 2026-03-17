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
#'     \item{`status`}{Character. One of `"pass"` or `"fail"`. `"fail"` if any
#'       checked column has a type mismatch; `"pass"` otherwise.}
#'     \item{`message`}{Character. Human-readable summary.}
#'     \item{`summary`}{Data frame with counts: `n_checked` and
#'       `n_type_mismatches`.}
#'     \item{`failures`}{Data frame with columns `column_name`,
#'       `expected_type`, `actual_type`, and `issue`, or `NULL` if
#'       `status == "pass"` or `detail == FALSE`.}
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
    ok <- .type_check(actual_col, sql_type)

    if (!isTRUE(ok)) {
      data.frame(
        column_name      = col,
        expected_type    = sql_type,
        actual_type      = .r_type_label(actual_col),
        issue            = "type_mismatch",
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  failures_df  <- do.call(rbind, Filter(Negate(is.null), failures_list))
  n_checked    <- length(cols_to_check)
  n_mismatches <- if (is.null(failures_df)) 0L else nrow(failures_df)

  if (n_mismatches > 0) {
    status <- "fail"
    msg <- paste0(
      n_mismatches, " column(s) have unexpected data types: ",
      paste(failures_df$column_name, collapse = ", ")
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
