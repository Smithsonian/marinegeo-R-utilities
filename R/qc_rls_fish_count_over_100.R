#' Check for fish count over 100
#'
#' Flag any rows where phylum is "Chordata" and "Total" is > 100.
#' These rows should be reviewed, but a flag does not mean it's an error.
#'
#' @param df
#'
#' @returns a list of row numbers that are fish species with a count over 100
#' @export
#'
#' @examples
#' qc_rls_fish_count_over_100(df)
qc_rls_fish_count_over_100 <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(c("Total", "phylum"), colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

    #use "phylum" column in conjunction with Total
    df_fish <- df |>
      tibble::rowid_to_column() |>
      dplyr::filter(phylum == "Chordata")

    if (nrow(df_fish) == 0){
      message("Data Frame Contains only Inverts or no observations")

    }else{
      row_numbers <- df_fish |>
        dplyr::filter(Total > 100)|>
        dplyr::pull(rowid)

      if (length(row_numbers) == 0){
        message("There are no fish with counts over 100.")
        return(NULL)
      }else{
        return(row_numbers)
      }
    }
  }, error = function(e) {
    message(paste("Error identifying fish counts over 100:", e$message))
  })
}
