#' Check for observations missing a taxonomic ID
#'
#' Check that no observations are missing a value in the "taxonomic_id" column.
#' Typically these values are Aphia IDs. Negative numbers are MarineGEO-specific IDs.
#'
#' @param df
#'
#' @returns a list of row numbers that have an NA or non-numeric value in the "taxonomic_id" column
#' @export
#'
#' @examples
#' qc_mg_missing_taxonomic_id(df)
qc_mg_missing_taxonomic_id <- function(df) {

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(c("taxonomic_id"), colnames(df))

  # expect_error()
  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

    df_taxonomic_id <- df |>
      tibble::rowid_to_column() |>
      dplyr::filter(is.na(taxonomic_id) | !is.numeric(taxonomic_id))

    row_numbers <- df_taxonomic_id$rowid

    if (length(row_numbers) == 0){
      message("There are no missing taxonomic IDs")
    }else{
      return(row_numbers)
    }
  }, error = function(e) {
    message(paste("Error checking for missing taxonomic IDs:", e$message))
  })
}
