#' QC Survey Summary Table
#'
#'
#' Looks for NA and Blank values in the dataframe summary table
#'
#'
#' @param df
#'
#' @returns a list of rows in the summary data table to highlight because they have a metadata issue that need to be corrected.
#'
#' @export
#'
#' @examples
#' filepath <- "inst/extdata/test_rls_data_EPA_one_header_row.xlsx"
#' filepath <- "C:/Users/westbrooke/OneDrive - Smithsonian Institution/MarineGEO-Data/reef-life-survey/rls_excel_data/UK/RLS Florida_AV_UK_September2024.xlsx"
#'
#' df <- utl_rls_load_excel(filepath)
#' summary_df <- utl_rls_sample_event_summary(df)
#' dives_missing_metadata <- qc_rls_empty_metadata(df)

qc_rls_empty_metadata <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  id_cols <- c("Diver","Buddy","Site No.","Site Name", "Latitude", "Longitude", "Date", "Depth", "vis","Direction","Time", "P-Qs")
  missing_columns <- dplyr::setdiff(id_cols, colnames(df))

  if(length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

    #Filter to the data that is missing information
    row_numbers <- df |>
      tibble::rowid_to_column() |>
      dplyr::mutate(across(everything(), as.character)) |>
      dplyr::select(rowid, Diver, Buddy, `Site No.`, `Site Name`, Latitude, Longitude, Date, vis, Direction, Time, `P-Qs`, Depth) |>
      dplyr::filter(if_any(everything(), ~ is.na(.x) | .x == "BLANK")) |>
      dplyr::mutate(rowid = as.numeric(rowid)) |>
      dplyr::pull(rowid)

  }, error = function(e) {
    message(paste("Error Flagging Summary Table", e$message))
  })

  return(row_numbers)

}
