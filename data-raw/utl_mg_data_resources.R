#' Return the MarineGEO data index table
#'
#' @returns df: a dataframe containing the MarineGEO data index
#' @export
#'
#' @examples
#' utl_mg_data_index()
utl_mg_data_index <- function(){
  df <- marinegeo_resources$data_index
  return(df)
}

#' Return the MarineGEO table relationships
#'
#' @returns df: a dataframe containing the MarineGEO table relationships
#' @export
#'
#' @examples
#' utl_mg_table_relationships()
utl_mg_table_relationships <- function(){
  df <- marinegeo_resources$table_relationships
  return(df)
}

#' Return the MarineGEO data table column and data type schema
#'
#' @returns df: a dataframe containing the MarineGEO data structures
#' @export
#'
#' @examples
#' utl_mg_data_structure()
utl_mg_data_structure <- function(){
  df <- marinegeo_resources$database_structure
  return(df)
}

#' Return the column order for a MarineGEO table
#'
#' @param table_id
#'
#' @returns a character vector representing column names
#' @export
#'
#' @examples
utl_mg_column_order <- function(table_id){

  column_names <- marinegeo_resources$database_structure |>
    dplyr::filter(table_id == !!table_id) |>
    dplyr::pull(column_name)

  return(column_names)
}
