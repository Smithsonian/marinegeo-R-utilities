#' Validate column data types for Arrow-safe writing
#'
#' @description
#' A pipe-friendly guardrail that validates column types in a data frame against
#' the MarineGEO schema for a given table, then throws a hard error if any
#' column violates what Apache Arrow expects. Place this function in a pipe
#' chain before a write operation (e.g., [readr::write_csv()]) to block
#' incorrect data from being written.
#'
#' Unlike [qc_check_data_types()], which returns a structured result list for
#' reporting, this function's purpose is to **stop execution** when types are
#' wrong. It looks up the expected schema automatically from internal metadata
#' using `table_id`, so no manual type map is needed.
#'
#' @param df A data frame or tibble to validate.
#' @param table_id Character scalar. A MarineGEO versioned table identifier
#'   (e.g., `"seagrass-cover-monitoring-v1"`). Must match a `table_id` present
#'   in `marinegeo_metadata$database_structure`.
#'
#' @return Returns `df` invisibly if all checked columns pass type validation,
#'   allowing the function to be used in a pipe chain without interrupting flow.
#'
#' @details
#' Only columns present in **both** `df` and the metadata for `table_id` are
#' checked. Columns in `df` not in the metadata, and metadata columns absent
#' from `df`, are silently skipped.
#'
#' SQL-style types from the metadata map to acceptable R types as follows:
#' \itemize{
#'   \item `STRING`  — any R type is accepted (Arrow can coerce anything to
#'     string)
#'   \item `DOUBLE`  — numeric, integer, or logical
#'   \item `INT` / `TINYINT` — integer or logical; numeric (double) only if all
#'     non-NA values are whole numbers (i.e., `x == round(x, 0)`)
#'   \item `DATE`    — Date, POSIXct, or POSIXlt
#'   \item `BOOL` / `BOOLEAN` — logical
#' }
#'
#' All failures are collected before throwing a single error, so all type
#' violations are reported at once rather than stopping on the first.
#'
#' **All-NA logical columns**: A column stored as `logical` because all values
#' are `NA` (a common artifact of `read_csv()` or `read_excel()`) is treated as
#' a `message()` warning rather than a hard error, unless the expected type is
#' `BOOL` (in which case it passes normally).
#'
#' **Unknown SQL types**: Types not listed above (e.g., `TIMESTAMP`, `INT64`)
#' are silently skipped.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_data |>
#'   dplyr::filter(site_code == "BIS-001") |>
#'   dplyr::mutate(cover = as.numeric(cover)) |>
#'   utl_mg_test_data_types(table_id = "seagrass-cover-monitoring-v1") |>
#'   readr::write_csv("output.csv")
#' }
utl_mg_test_data_types <- function(df, table_id) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  if (!is.character(table_id) || length(table_id) != 1L || is.na(table_id)) {
    stop("`table_id` must be a single non-NA character string.")
  }

  # --- Metadata lookup --------------------------------------------------------
  schema <- marinegeo_metadata$database_structure |>
    dplyr::filter(table_id == !!table_id) |>
    dplyr::select(column_name, data_type)

  if (nrow(schema) == 0L) {
    stop(
      "`table_id` '", table_id, "' was not found in ",
      "`marinegeo_metadata$database_structure`."
    )
  }

  # --- Type checking ----------------------------------------------------------
  cols_to_check <- intersect(names(df), schema$column_name)
  known_types   <- c("STRING", "DOUBLE", "INT", "TINYINT", "DATE", "BOOL", "BOOLEAN")
  failure_messages <- list()

  for (col in cols_to_check) {
    sql_type   <- toupper(schema$data_type[schema$column_name == col][1])
    actual_col <- df[[col]]

    # All-NA logical column: emit message and skip (not a hard error)
    if (is.logical(actual_col) && all(is.na(actual_col)) && sql_type != "BOOL") {
      message(
        "Column '", col, "' is entirely NA (stored as logical). ",
        "This is usually a read_csv/read_excel artifact. Skipping type check."
      )
      next
    }

    # Unknown SQL types: skip silently
    if (!sql_type %in% known_types) next

    # STRING: Arrow coerces anything to string
    if (sql_type == "STRING") next

    # INT / TINYINT: stricter than .type_check() — numeric must be whole numbers
    if (sql_type %in% c("INT", "TINYINT")) {
      if (is.integer(actual_col) || is.logical(actual_col)) {
        next
      } else if (is.numeric(actual_col)) {
        non_na_vals <- actual_col[!is.na(actual_col)]
        if (length(non_na_vals) == 0L || all(non_na_vals == round(non_na_vals, 0))) {
          next
        } else {
          failure_messages[[col]] <- paste0(
            "'", col, "' (", sql_type, "): ",
            "column is numeric but contains non-integer values"
          )
        }
      } else {
        failure_messages[[col]] <- paste0(
          "'", col, "' (", sql_type, "): ",
          "expected integer, numeric (whole numbers only), or logical ",
          "but got ", .r_type_label(actual_col)
        )
      }
      next
    }

    # DOUBLE, DATE, BOOL/BOOLEAN: delegate to .type_check()
    # Map BOOLEAN -> BOOL so .type_check() handles it
    check_type <- if (sql_type == "BOOLEAN") "BOOL" else sql_type
    ok <- .type_check(actual_col, check_type)

    if (!isTRUE(ok)) {
      expected_label <- switch(sql_type,
        "DOUBLE"  = "numeric, integer, or logical",
        "DATE"    = "Date, POSIXct, or POSIXlt",
        "BOOL"    = ,
        "BOOLEAN" = "logical",
        sql_type
      )
      failure_messages[[col]] <- paste0(
        "'", col, "' (", sql_type, "): ",
        "expected ", expected_label, " but got ", .r_type_label(actual_col)
      )
    }
  }

  # --- Single stop() after all columns are checked ---------------------------
  if (length(failure_messages) > 0L) {
    stop(
      "Data type validation failed for table '", table_id, "'.\n",
      "The following column(s) have incorrect types:\n",
      paste0("  - ", unlist(failure_messages), collapse = "\n")
    )
  }

  invisible(df)
}
