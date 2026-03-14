#' Check for invertebrate counts over 20
#'
#' Check that the rls dataset is a dataframe and contains columns `Inverts` and `phylum`
#' finds rows where the phylum is not "Chordata" and the column `Inverts` is a numerical value over 20.
#'
#' @param df
#'
#' @returns a list of row numbers that are invert species with an invert count over 20
#' @export
#'
#' @examples
#' rows_invert_counts_over_20 <- qc_rls_inverts_over_20(df)
qc_rls_inverts_over_20 <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(c("inverts", "phylum"), colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

    #use "phylum" column in conjunction with invert count
    df_inverts <- df |>
      tibble::rowid_to_column() |>
      dplyr::filter(phylum != "Chordata")

    if (nrow(df_inverts) == 0){
      message("Data Frame Contains only Chordata Species")
    }else{
      #use tidyverse syntax whenever possible
      #example for this function: dplyr::filter(), tibble::rowid_to_column(), and dplyr::pull()
      row_numbers <- df_inverts |>
        dplyr::filter(inverts > 20)|>
        dplyr::pull(rowid)

      if (length(row_numbers) == 0){
        message("There are no Inverts with counts over 20.")
        return(NULL)
      }else{
        return(row_numbers)
      }
    }
  }, error = function(e) {
    message(paste("Error identifying invert counts over 20:", e$message))
  })
}
