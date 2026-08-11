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
#' @param sheet Integer or character. Only used when `x` is an `.xlsx` or
#'   `.xls` file path. Passed to [readxl::read_excel()]. Defaults to `1`.
#'
#' @return A [qc_issues] tibble: one row per detected problem across all
#'   applicable checks (zero rows if the data is clean), with the producing
#'   check in the `check` column. Run-level metadata rides along as attributes
#'   — `table_id`, `n_rows` (rows validated), `checks_run` (which checks
#'   executed), and `status` (the worst severity present: `"fail"` > `"warn"` >
#'   `"pass"`). See [qc_issues] for the full column description.
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
#'   \item Missing values test: runs if `database_structure` has rows with
#'     `missing_values` equal to `"enforce"` or `"warn"` for `table_id`.
#'   \item Numeric ranges test: runs if `numeric_ranges` has rows with non-`NA`
#'     `range_type` for `table_id`.
#'   \item Lookup values test: runs if `data` contains any of the columns
#'     `partner_code`, `site_name`, `site_code`, or `scientific_name`, which are
#'     validated against the corresponding global entity registries in
#'     `marinegeo_metadata` (`partner_codes`, `site_codes`,
#'     `observation_lookup`).
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
qc_run <- function(x, table_id, sheet = 1L) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(table_id) || length(table_id) != 1) {
    stop("`table_id` must be a single character string.")
  }

  # --- Ingest: resolve x to a data frame -------------------------------------
  data <- .qc_ingest(x, sheet = sheet)

  # --- Dispatch ---------------------------------------------------------------
  .qc_dispatch(data, table_id = table_id)
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
        "Unsupported file extension '.",
        ext,
        "'. ",
        "Supported: csv, xlsx, xls, parquet."
      )
    }
  }

  # Arrow Dataset or Table
  if (
    inherits(
      x,
      c(
        "ArrowObject",
        "Dataset",
        "arrow_dplyr_query",
        "RecordBatch",
        "Table",
        "ArrowTabular"
      )
    )
  ) {
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
    "Got: ",
    paste(class(x), collapse = "/")
  )
}

# ---------------------------------------------------------------------------
# Internal: metadata-driven dispatch
# ---------------------------------------------------------------------------
.qc_dispatch <- function(data, table_id) {
  db_struct <- .mg_get_registry_table("database_structure")
  cat_vals <- .mg_get_registry_table("categorical_values")

  tbl_struct <- db_struct[db_struct$table_id == table_id, ]
  tbl_cats <- cat_vals[cat_vals$table_id == table_id, ]

  if (nrow(tbl_struct) == 0 && nrow(tbl_cats) == 0) {
    warning(
      "No metadata found for table_id '",
      table_id,
      "'. ",
      "No tests will be run. ",
      "Check `marinegeo_metadata$data_index` for valid table_id values."
    )
    return(new_qc_issues(
      table_id = table_id,
      n_rows = nrow(data),
      checks_run = character(0)
    ))
  }

  results <- list()

  # --- Test 1: column presence and order -------------------------------------
  if (nrow(tbl_struct) > 0) {
    results$qc_check_columns <- qc_check_columns(
      data = data,
      expected_columns = tbl_struct$column_name
    )
  }

  # --- Test 2: data types ----------------------------------------------------
  if (nrow(tbl_struct) > 0) {
    type_rows <- tbl_struct[!is.na(tbl_struct$data_type), ]
    if (nrow(type_rows) > 0) {
      type_map <- stats::setNames(type_rows$data_type, type_rows$column_name)
      results$qc_check_data_types <- qc_check_data_types(
        data = data,
        type_map = type_map
      )
    }
  }

  # --- Test 3: categorical values --------------------------------------------
  if (nrow(tbl_cats) > 0) {
    results$qc_check_categorical_values <- qc_check_categorical_values(
      data = data,
      rules = tbl_cats[, c("column_name", "value")]
    )
  }

  # --- Test 4: missing values ------------------------------------------------
  miss_rows <- tbl_struct[tbl_struct$missing_values %in% c("enforce", "warn"), ]
  if (nrow(miss_rows) > 0) {
    results$qc_check_missing_values <- qc_check_missing_values(
      data = data,
      rules = miss_rows[, c("column_name", "missing_values")]
    )
  }

  # --- Test 5: numeric ranges ------------------------------------------------
  num_ranges <- .mg_get_registry_table("numeric_ranges")
  range_cols <- c(
    "column_name",
    "max_fail",
    "min_fail",
    "max_warn",
    "min_warn",
    "range_type"
  )
  if (
    nrow(num_ranges) > 0 &&
      "table_id" %in% colnames(num_ranges) &&
      all(range_cols %in% colnames(num_ranges))
  ) {
    tbl_ranges <- num_ranges[num_ranges$table_id == table_id, ]
    tbl_ranges <- tbl_ranges[!is.na(tbl_ranges$range_type), ]
  } else {
    tbl_ranges <- data.frame()
  }
  if (nrow(tbl_ranges) > 0) {
    results$qc_check_numeric_ranges <- qc_check_numeric_ranges(
      data = data,
      rules = tbl_ranges[, c(
        "column_name",
        "max_fail",
        "min_fail",
        "max_warn",
        "min_warn",
        "range_type"
      )]
    )
  }

  # --- Test 6: lookup values ---------------------------------------------------
  lookup_map <- Filter(
    Negate(is.null),
    list(
      partner_code = .mg_get_registry_table("partner_codes")$partner_code,
      site_name = .mg_get_registry_table("site_codes")$site_name,
      site_code = .mg_get_registry_table("site_codes")$site_code,
      scientific_name = .mg_get_registry_table(
        "observation_lookup"
      )$scientific_name
    )
  )
  present_lookup_cols <- intersect(names(lookup_map), colnames(data))
  if (length(present_lookup_cols) > 0) {
    results$qc_check_lookup_values <- qc_check_lookup_values(
      data = data,
      lookups = lookup_map[present_lookup_cols]
    )
  }

  # --- Test 7: row uniqueness ------------------------------------------------
  uuid_cols <- if ("uuid_identity" %in% colnames(tbl_struct)) {
    tbl_struct$column_name[which(tbl_struct$uuid_identity)]
  } else {
    character(0)
  }
  if (length(uuid_cols) > 0) {
    results$qc_check_row_uniqueness <- qc_check_row_uniqueness(
      data = data,
      id_cols = uuid_cols
    )
  }
  
  # --- Test 8: point count validity ----------------------------------------------
  if("points_in_quadrat" %in% colnames(tbl_struct & "point_count" %in% colnames(tbl_struct))) {
    results$qc_check_point_counts <- qc_check_point_counts(
      data = data,
    )
  }

  # --- Combine into a single issues table ------------------------------------
  # Each check returns a qc_issues tibble; bind them and re-wrap with run-level
  # metadata. `status` is derived from the combined rows (fail > warn > pass).
  new_qc_issues(
    dplyr::bind_rows(results),
    table_id = table_id,
    n_rows = nrow(data),
    checks_run = names(results)
  )
}
