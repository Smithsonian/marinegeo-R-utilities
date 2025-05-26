#' Load Reef Life Survey Excel into R
#'
#' Load the DATA sheet from a RLS (Reef Life Survey) Excel file into R.
#' Drops first and potentially second row based on contents of `Method` column.
#' Drops trailing empty rows based on `Total` column
#'
#' @param filepath
#'
#' @returns A dataframe without any header rows or with empty trailing rows.
#' @export
#'
#' @examples
#' filepath <- "inst/extdata/test_rls_data_EPA_two_header_rows.xlsx"
#' filepath <- "inst/extdata/test_rls_data_EPA_one_header_row.xlsx"
#' df <- utl_rls_load_excel(filepath)
utl_rls_load_excel <- function(filepath){

  tryCatch({

    df_raw <- readxl::read_excel(filepath, sheet = "DATA")

    # Check for necessary columns to process dataframe
    missing_columns <- dplyr::setdiff(c("Total", "Method", "Site No.", "P-Qs"), colnames(df_raw))

    if (length(missing_columns) > 0) {
      message(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
      return(NULL)
    }

    # Filter out header rows, files have either 1 or 2 rows
    # Total, Inverts, and size class columns should remain as numeric data types
    df <- df_raw |>
      dplyr::filter(Method != "0, 1, 2")

    if(nrow(df) == 0){
      warning("Dataframe has 0 rows after dropping header rows")
    }

    # Calculate the number of rows that were dropped by filter()
    dropped_rows <- nrow(df_raw) - nrow(df)

    if(dropped_rows == 1){
      print("1 header row dropped")
    } else if(dropped_rows == 2){
      print("2 header rows dropped")
    } else {
      print("Unexpected number of rows dropped")
    }

    # This section removes the many blank rows from the bottom of the datasheet
    # rle() identifies runs of consecutive values within a vector
    # In rle() output:
    # The final item in the values vector should be TRUE (evaluated as Total == 0)
    # and the final item in the lengths vector represents the number of rows from the bottom that should be dropped
    runs <- rle(df$Total == 0)

    if(dplyr::last(runs$values) & length(runs$lengths) == 1){

      warning("Dataframe only has rows with a Total equal to 0")

    } else if(dplyr::last(runs$values)){

      # last row to keep
      last_row <- nrow(df) - dplyr::last(runs$lengths)

      df <- df[1:last_row,]

      print(paste0("dropped ", as.character(dplyr::last(runs$lengths)), " rows containing 0 total count from the end of the datasheet"))

    } else {
      print("No rows were dropped from the end of the datasheet")

    }

    # Rename some columns to match MarineGEO column standards
    df <- df %>%
      rename(
        site_code = `Site No.`,
        photoquadrats = `P-Qs`
      ) %>%
      # Input filename is metadata used to track file curation
      mutate(input_filename = basename(filepath))

    # Convert to lower case and remove spaces
    colnames(df) <- tolower(gsub(" ", "_", colnames(df)))

    return(df)

  }, error = function(e) {
    message(paste("Error reading Excel file:", e$message))
    return(NULL)
  })
}
