# df <- readr::read_csv("C:/Users/marine/Documents/repositories/marinegeo-reef-life-survey/L2-data/reef-life-survey-data-marinegeo-v1/Gretta_RLS USVI Data Entry.csv")
# table_id <- "reef-life-survey-data-marinegeo-v1"

#' Verify that data types in table match arrow data types
#'
#' If there is a mismatch, an error is created with information on the offending column name and data type.
#' The function is designed to fit into the select all columns and write_csv pipe,
#' so it returns the original dataframe if it passes the tests. The error will prevent mismatch
#' types from getting written to a directory used for Apache Arrow.
#'
#' @param df
#' @param table_id
#'
#' @returns df
#' @export
#'
#' @examples
qc_mg_column_data_types <- function(df, table_id){

  column_arrow_types <- marinegeo_resources$database_structure |>
    dplyr::filter(table_id == !!table_id) |>
    dplyr::select(column_name, data_type) |>
    dplyr::rename(arrow_data_type = data_type)

  input_column_types <- tibble::tibble(column_name = names(df), r_data_type = unname(purrr::map_chr(df, ~class(.x)[1])))

  df_types <- dplyr::left_join(column_arrow_types, input_column_types)

  results <- lapply(1:nrow(df_types), function(i){

    arrow_type <- df_types[i,]$arrow_data_type
    r_type <- df_types[i,]$r_data_type
    column_name <- df_types[i,]$column_name

    error_message <- NA

    # STRING columns can be any data type
    if(arrow_type == "STRING"){
      result <- TRUE


    # DOUBLE columns can be numeric, integer, OR logical
    } else if(arrow_type == "DOUBLE"){

      if(r_type %in% c("integer", "numeric", "logical")){
        result <- TRUE

      } else {
        result <- FALSE
        error_message <- "Column type is not numeric, integer, or logical"

      }

    # INT and TINYINT columns can be numeric, integer, or logical
    # If numeric, additional test for no decimals, i.e., x == round(x, 0)
    } else if(arrow_type %in% c("INT", "TINYINT")){

      if(r_type %in% c("integer", "logical")){
        result <- TRUE


      } else if(r_type == "numeric"){

        col_dat <- df |>
          dplyr::filter(!is.na(.data[[column_name]])) |>
          dplyr::pull(.data[[column_name]])

        if(all(col_dat == round(col_dat, 0))){
          result <- TRUE

        } else {
          result <- FALSE
          error_message <- "Column type is numeric with non-integers present in data"

        }

      } else {
        result <- FALSE
        error_message <- "Column type is not numeric, integer, or logical"

      }

    # DATE columns must be a date type
    } else if(arrow_type == "DATE"){

      if(r_type == "Date"){
        result <- TRUE

      } else {
        result <- FALSE
        error_message <- "Column type is not Date"

      }

    # Boolean columns should be logical
    } else if (arrow_type %in% c("BOOL", "BOOLEAN")){

      if(r_type == "logical"){
        result <- TRUE

      } else {
        result <- FALSE
        error_message <- "Column type is logical"

      }
    }

    if(!result){
      stop(paste0(column_name, " : ", error_message))

    }

  })

  return(df)

}
