#' Check for registered partner codes
#'
#' @param df
#'
#' @returns vector of row numbers for partner codes that are not in a controlled vocabulary
#' @export
#'
#' @examples
qc_mg_partner_codes <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  if(!"partner_code" %in% colnames(df)){
    message("Partner Code is not in the input data frame")
    return(NULL)
  } else {

    tryCatch({

      partner_codes <- marinegeo_resources$partner_codes |>
        dplyr::pull(partner_code)

      row_numbers <- df |>
        tibble::rowid_to_column() |>
        dplyr::filter(!partner_code %in% partner_codes)|>
        dplyr::pull(rowid)

      if(length(row_numbers) == 0){
        return(NULL)
      }else{
        return(row_numbers)
      }

    }, error = function(e) {
      message(paste("Error checking that partner code(s) are accepted:", e$message))
    })

  }
}

#' Check for registered site names
#'
#' @param df
#'
#' @returns vector of row numbers for site names that are not in a controlled vocabulary
#' @export
#'
#' @examples
qc_mg_site_names <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  if(!"site_name" %in% colnames(df)){
    message("Site name is not in the input data frame")
    return(NULL)
  } else {

    tryCatch({

      site_names <- marinegeo_resources$site_names |>
        dplyr::pull(site_name)

      row_numbers <- df |>
        tibble::rowid_to_column() |>
        dplyr::filter(!site_name %in% site_names)|>
        dplyr::pull(rowid)

      if(length(row_numbers) == 0){
        return(NULL)
      }else{
        return(row_numbers)
      }

    }, error = function(e) {
      message(paste("Error checking that site name(s) are accepted:", e$message))
    })

  }
}

#' Evaluate categorical values
#' Evaluate based on lookup table stored with R package
#'
#' @param df
#' @param table_id
#'
#' @returns vector of row numbers for values that are not in a controlled vocabulary
#' @export
#'
#' @examples
qc_mg_categorical_values <- function(df, table_id){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  if(!table_id %in% unique(marinegeo_resources$categorical_values$table_id)){
    message("This table does not have controlled categorical variables")
    return(NULL)
  }

  tryCatch({

  df_allowed_values <- marinegeo_resources$categorical_values |>
    dplyr::filter(table_id == !!table_id)

  cols_to_test <- df_allowed_values |>
    dplyr::count(column_name) |>
    dplyr::pull(column_name)

  df_evaluate <- df |>
    tibble::rowid_to_column("rowid") |>
    dplyr::select("rowid", dplyr::any_of(cols_to_test))

  row_numbers <- purrr::compact(
    lapply(cols_to_test, function(column_name){

      allowed_values <- df_allowed_values |>
        dplyr::filter(column_name == !!column_name) |>
        dplyr::pull(value)

      if(column_name %in% colnames(df_evaluate)){

        result <- df_evaluate %>%
          filter(!is.na(.data[[column_name]])) %>%
          filter(!.data[[column_name]] %in% allowed_values)

        if(nrow(result) > 1){
          result$rowid
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
  )

  row_numbers <- unique(unlist(unname(row_numbers)))

  if(length(row_numbers) == 0){
    return(NULL)
  } else{
    return(row_numbers)
  }

  }, error = function(e) {
    message(paste("Error checking categorical values:", e$message))
  })

}

#' Evaluate categorical values for column details
#' Evaluate based on lookup table stored with R package
#'
#' @param df
#' @param table_id
#'
#' @returns table with details about which columns are being flagged on each row for the invalid categorical values.
#' @export
#'
#' @examples
qc_mg_categorical_values_details <- function(df, table_id){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  if(!table_id %in% unique(marinegeo_resources$categorical_values$table_id)){
    message("This table does not have controlled categorical variables")
    return(NULL)
  }

  tryCatch({

    df_allowed_values <- marinegeo_resources$categorical_values |>
      dplyr::filter(table_id == !!table_id)

    cols_to_test <- df_allowed_values |>
      dplyr::count(column_name) |>
      dplyr::pull(column_name)

    df_evaluate <- df |>
      tibble::rowid_to_column("rowid") |>
      dplyr::select("rowid", dplyr::any_of(cols_to_test))

    #Get the detailed output for the flag details module
    column_flag_table <- purrr::map_dfr(cols_to_test, function(col){
      allowed_values <- df_allowed_values %>%
        dplyr::filter(column_name == col) %>%
        dplyr::pull(value)

      invalid_rows_col <- df_evaluate %>%
        dplyr::filter(!.data[[col]] %in% allowed_values) %>%
        dplyr::pull(rowid)

      if(length(invalid_rows_col) > 0){
        tibble::tibble(
          column = col,
          rows   = paste(invalid_rows_col, collapse = ", "),  # collapse into one string
          allowed_values = paste(allowed_values, collapse = ", ")
        )
      } else {
        tibble::tibble(
          column = col,
          rows   = "",   # empty string if no violations
          allowed_values = paste(allowed_values, collapse = ", ")
        )
      }
    })%>%
      dplyr::filter(rows != "")

    if(nrow(column_flag_table) == 1){
      column_flag_table <- column_flag_table%>%
        mutate(rows = "All Flagged Rows")
    }

    if(nrow(column_flag_table) == 0){
      return(NULL)
    } else{
      return(column_flag_table)
    }

  }, error = function(e) {
    message(paste("Error checking categorical values:", e$message))
  })

}
