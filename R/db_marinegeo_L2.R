#' Connect to a controlled data directory with Arrow. Target should be defined in MarineGEO data structure. Use "dataframe" as return type to load entire dataset into R. Default, "arrow", allows you to filter dataset before collecting into R.
#'
#' @param table_id
#' @param return_type "arrow" to return an active binding to directory or "dataframe" to collect all data at connection
#'
#' @returns
#' @export
#'
#' @examples
#' table_id <- "reef-life-survey-data-marinegeo-v1"
#' ds <- db_marinegeo_L2(table_id, "arrow")
#' ds %>% filter(site_code == "BC4") %>% collect()
db_marinegeo_L2 <- function(table_id, return_type = "arrow"){

  # Create Arrow schema

  # Load data structure and use column and data types to create schema
  table_data_structure <- utl_mg_data_structure() |>
    dplyr::filter(table_id == !!table_id)

  schema_list <- lapply(1:nrow(table_data_structure), function(row){

    column_name <- table_data_structure$column_name[row]
    data_type <- table_data_structure$data_type[row]

    if(column_name == "timestamp_utc"){
      data_type <- "TIMESTAMP UTC"
    } else if(column_name == "timestamp_local"){
      data_type <- "STRING"
    }

    arrow_type <- switch(data_type,
                         "STRING" = arrow::string(),
                         "INT" = int32(),
                         "INT64" = int64(),
                         "TIMESTAMP" = timestamp(),
                         "TIMESTAMP UTC" = timestamp(timezone = "UTC"),
                         "DOUBLE" = double(),
                         "DATE" = date32(),
                         "TINYINT" = uint8()
    )

    arrow::field(column_name, arrow_type)

  })

  arrow_schema <- arrow::schema(unlist(schema_list))

  # Get dataset path
  db_location <- utl_mg_data_index() |>
    dplyr::filter(table_id == !!table_id) |>
    dplyr::pull(location)

  db_directory <- utl_mg_data_index() |>
    dplyr::filter(table_id == !!table_id) |>
    dplyr::pull(directory)

  if(db_location == "github"){
    dataset_path <- paste0(Sys.getenv("repository_filepath"), db_directory)
  } else {
    dataset_path <- NULL
  }

  if(return_type == "arrow"){

    # The first row should be skipped when providing a schema (skips header row)
    ds <- arrow::open_csv_dataset(dataset_path, schema = arrow_schema, skip = 1)

    return(ds)

  } else if(return_type == "dataframe"){

    # The first row should be skipped when providing a schema (skips header row)
    df <- arrow::open_csv_dataset(dataset_path, schema = arrow_schema, skip = 1) %>%
      collect()

    return(df)

  }

}
