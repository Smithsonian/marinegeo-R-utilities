#' Connect to a MarineGEO Arrow dataset
#'
#' @description
#' Opens a controlled MarineGEO L2 data directory using Arrow. The target
#' `table_id` must be defined in the internal MarineGEO data index. Use
#' `return_type = "dataframe"` to load the full dataset into R immediately.
#' The default `return_type = "arrow_dataset"` returns a lazy Arrow dataset,
#' allowing you to filter before collecting into memory.
#'
#' @param table_id Character. The versioned table identifier (e.g.,
#'   `"reef-life-survey-data-marinegeo-v1"`). Must match a `table_id` in the
#'   internal `marinegeo_metadata$data_index`.
#' @param return_type Character. Either `"arrow_dataset"` (default) to return
#'   a lazy Arrow dataset, or `"dataframe"` to collect the full dataset into a
#'   data frame immediately.
#'
#' @return An Arrow Dataset object when `return_type = "arrow_dataset"`, or a
#'   data frame when `return_type = "dataframe"`. Returns `NULL` with a message
#'   if `table_id` is not found in the data structure metadata.
#'
#' @details
#' Column types are derived from the internal `marinegeo_metadata$database_structure`
#' table using SQL-style type names (`STRING`, `INT`, `INT64`, `DOUBLE`, `DATE`,
#' `BOOL`, `TINYINT`, `TIMESTAMP`). Two columns receive special handling
#' regardless of the metadata: `timestamp_utc` is always cast to
#' `arrow::timestamp(timezone = "UTC")` and `timestamp_local` is always cast to
#' `arrow::string()`.
#'
#' Dataset path is resolved from `marinegeo_metadata$data_index`. Currently
#' only `location = "github"` is supported; the path is constructed from the
#' `repository_filepath` environment variable and the `directory` field.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' table_id <- "reef-life-survey-data-marinegeo-v1"
#' ds <- db_arrow_marinegeo(table_id)
#' ds |> dplyr::filter(site_code == "BC4") |> dplyr::collect()
#'
#' # Load full dataset into memory
#' df <- db_arrow_marinegeo(table_id, return_type = "dataframe")
#' }
db_arrow_marinegeo <- function(
  table_id,
  return_type = "arrow_dataset"
) {
  # Validate return_type
  return_type <- match.arg(
    return_type,
    choices = c("arrow_dataset", "dataframe")
  )

  # Load data structure and use column and data types to create schema
  table_data_structure <- marinegeo_metadata$database_structure |>
    dplyr::filter(table_id == !!table_id)

  if (nrow(table_data_structure) == 0) {
    message("TABLE NOT DEFINED IN DATA STRUCTURE")
    return(NULL)
  }

  # Create Arrow schema from column metadata
  schema_list <- lapply(seq_len(nrow(table_data_structure)), function(row) {
    column_name <- table_data_structure$column_name[row]
    data_type <- table_data_structure$data_type[row]

    # Override timestamp handling regardless of metadata type
    if (column_name == "timestamp_utc") {
      data_type <- "TIMESTAMP UTC"
    } else if (column_name == "timestamp_local") {
      data_type <- "STRING"
    }

    arrow_type <- switch(
      data_type,
      "STRING" = arrow::string(),
      "INT" = arrow::int32(),
      "INT64" = arrow::int64(),
      "TIMESTAMP" = arrow::timestamp(),
      "TIMESTAMP UTC" = arrow::timestamp(timezone = "UTC"),
      "DOUBLE" = arrow::float64(),
      "DATE" = arrow::date32(),
      "TINYINT" = arrow::uint8(),
      "BOOL" = arrow::bool()
    )

    arrow::field(column_name, arrow_type)
  })

  arrow_schema <- do.call(arrow::schema, schema_list)

  # Get dataset path from data index
  data_index_row <- marinegeo_metadata$data_index |>
    dplyr::filter(table_id == !!table_id)

  db_location <- dplyr::pull(data_index_row, location)
  db_directory <- dplyr::pull(data_index_row, directory)

  if (db_location == "github") {
    dataset_path <- paste0(Sys.getenv("repository_filepath"), db_directory)
  } else {
    message("Unsupported data location: ", db_location)
    return(NULL)
  }

  # The first row must be skipped when providing a schema (skips the header row)
  if (return_type == "arrow_dataset") {
    ds <- arrow::open_csv_dataset(dataset_path, schema = arrow_schema, skip = 1)
    return(ds)
  } else {
    df <- arrow::open_csv_dataset(
      dataset_path,
      schema = arrow_schema,
      skip = 1
    ) |>
      dplyr::collect()
    return(df)
  }
}
