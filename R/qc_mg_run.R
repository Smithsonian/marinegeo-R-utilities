#' Run all applicable QC tests on a MarineGEO data table
#'
#' @description
#' Single entry point for the MarineGEO QA/QC test suite. Given a data object
#' and a `table_id`, `qc_run()` queries `marinegeo_metadata` to determine
#' which tests apply, assembles test parameters from metadata, and returns a
#' structured result list.
#'
#' Accepts a data frame, a file path (CSV, XLSX, Parquet), or an Arrow
#' Dataset/Table.
#'
#' @param x A data frame, tibble, file path (character string), Arrow Dataset,
#'   or Arrow Table.
#' @param table_id Character scalar. The MarineGEO table identifier (from
#'   `marinegeo_metadata$data_index$table_id`). Used to look up expected
#'   columns, data types, and controlled vocabularies.
#' @param detail Logical. If `TRUE` (default), failing rows are included in
#'   each test result's `failures` element. If `FALSE`, `failures` is `NULL`
#'   for all tests, reducing memory use in automated pipelines.
#' @param sheet Integer or character. Only used when `x` is an `.xlsx` or
#'   `.xls` file path. Passed to [readxl::read_excel()]. Defaults to `1`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`table_id`}{The `table_id` argument.}
#'     \item{`status`}{Character. The worst status across all tests:
#'       `"fail"` > `"warn"` > `"pass"`.}
#'     \item{`n_rows`}{Integer. Number of rows in the validated data.}
#'     \item{`tests`}{Named list of individual test result lists, one per test
#'       run. Each element has `test`, `status`, `message`, `summary`, and
#'       `failures` (see [qc_check_columns()], [qc_check_data_types()],
#'       [qc_check_categorical_values()]).}
#'   }
#'
#' @details
#' **Metadata-driven dispatch:** Tests are not hard-coded to table types.
#' Instead, the dispatcher queries `marinegeo_metadata` for the given
#' `table_id` and infers which tests to run:
#' \itemize{
#'   \item Column presence/order test: runs if `database_structure` has rows
#'     for `table_id`.
#'   \item Data type test: runs if `database_structure` has rows with non-`NA`
#'     `data_type` values for `table_id`.
#'   \item Categorical values test: runs if `categorical_values` has rows for
#'     `table_id`.
#' }
#' Adding a new table type requires only updating the metadata CSVs and
#' rebuilding sysdata — no R code changes required.
#'
#' **Arrow support:** Arrow Datasets and Tables are collected into memory via
#' [dplyr::collect()] before testing. This requires the `arrow` package.
#'
#' **File path support:** CSV files are read with [readr::read_csv()]; XLSX/XLS
#' files with [readxl::read_excel()]; Parquet files with
#' [arrow::read_parquet()] (requires `arrow`).
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   site_name       = "Example Site",
#'   survey_date     = as.Date("2024-06-01"),
#'   scientific_name = "Zostera marina",
#'   percent_cover   = 45.2
#' )
#'
#' # Supply a valid table_id from marinegeo_metadata$data_index
#' # result <- qc_run(df, table_id = "sav_cover_v1")
qc_run <- function(x, table_id, detail = TRUE, sheet = 1L) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(table_id) || length(table_id) != 1) {
    stop("`table_id` must be a single character string.")
  }
  if (!is.logical(detail) || length(detail) != 1) {
    stop("`detail` must be a single logical value (TRUE or FALSE).")
  }

  # --- Ingest: resolve x to a data frame -------------------------------------
  data <- .qc_ingest(x, sheet = sheet)

  # --- Dispatch ---------------------------------------------------------------
  .qc_dispatch(data, table_id = table_id, detail = detail)
}

# ---------------------------------------------------------------------------
# Internal: resolve x to a plain data frame
# ---------------------------------------------------------------------------
.qc_ingest <- function(x, sheet = 1L) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("File not found: '", x, "'")
    }
    ext <- tolower(tools::file_ext(x))

    if (ext == "csv") {
      return(readr::read_csv(x, show_col_types = FALSE))
    } else if (ext %in% c("xlsx", "xls")) {
      return(readxl::read_excel(x, sheet = sheet))
    } else if (ext == "parquet") {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop(
          "The 'arrow' package is required to read Parquet files. ",
          "Install it with: install.packages('arrow')"
        )
      }
      return(arrow::read_parquet(x))
    } else {
      stop(
        "Unsupported file extension '.", ext, "'. ",
        "Supported: csv, xlsx, xls, parquet."
      )
    }
  }

  # Arrow Dataset or Table
  if (inherits(x, c("ArrowObject", "Dataset", "arrow_dplyr_query",
                     "RecordBatch", "Table", "ArrowTabular"))) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop(
        "The 'arrow' package is required to process Arrow objects. ",
        "Install it with: install.packages('arrow')"
      )
    }
    return(dplyr::collect(x))
  }

  stop(
    "`x` must be a data frame, a file path (character), or an Arrow object. ",
    "Got: ", paste(class(x), collapse = "/")
  )
}

# ---------------------------------------------------------------------------
# Internal: metadata-driven dispatch
# ---------------------------------------------------------------------------
.qc_dispatch <- function(data, table_id, detail) {
  db_struct  <- marinegeo_metadata$database_structure
  cat_vals   <- marinegeo_metadata$categorical_values

  tbl_struct <- db_struct[db_struct$table_id == table_id, ]
  tbl_cats   <- cat_vals[cat_vals$table_id == table_id, ]

  if (nrow(tbl_struct) == 0 && nrow(tbl_cats) == 0) {
    warning(
      "No metadata found for table_id '", table_id, "'. ",
      "No tests will be run. ",
      "Check `marinegeo_metadata$data_index` for valid table_id values."
    )
    return(list(
      table_id = table_id,
      status   = "pass",
      n_rows   = nrow(data),
      tests    = list()
    ))
  }

  results <- list()

  # --- Test 1: column presence and order -------------------------------------
  if (nrow(tbl_struct) > 0) {
    results$qc_check_columns <- qc_check_columns(
      data             = data,
      expected_columns = tbl_struct$column_name,
      detail           = detail
    )
  }

  # --- Test 2: data types ----------------------------------------------------
  if (nrow(tbl_struct) > 0) {
    type_rows <- tbl_struct[!is.na(tbl_struct$data_type), ]
    if (nrow(type_rows) > 0) {
      type_map <- stats::setNames(type_rows$data_type, type_rows$column_name)
      results$qc_check_data_types <- qc_check_data_types(
        data     = data,
        type_map = type_map,
        detail   = detail
      )
    }
  }

  # --- Test 3: categorical values --------------------------------------------
  if (nrow(tbl_cats) > 0) {
    results$qc_check_categorical_values <- qc_check_categorical_values(
      data   = data,
      rules  = tbl_cats[, c("column_name", "value")],
      detail = detail
    )
  }

  # --- Aggregate status: fail > warn > pass ----------------------------------
  all_statuses <- vapply(results, `[[`, character(1), "status")
  if ("fail" %in% all_statuses) {
    top_status <- "fail"
  } else if ("warn" %in% all_statuses) {
    top_status <- "warn"
  } else {
    top_status <- "pass"
  }

  list(
    table_id = table_id,
    status   = top_status,
    n_rows   = nrow(data),
    tests    = results
  )
}
