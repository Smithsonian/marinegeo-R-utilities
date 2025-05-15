#' Check for total count of 0
#'
#' Flag any row where the Total count is 0
#'
#' @param df
#'
#' @returns a list of row numbers where Total equals 0
#' @export
#'
#' @examples
#' qc_rls_total_count_0(df)
qc_rls_total_count_0 <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(c("Total"), colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

    #use "phylum" column in conjunction with Total
    df_total <- df |>
      tibble::rowid_to_column()

    if (nrow(df_total) == 0){
      message("Data Frame has no observations (rows)")

    }else{
      row_numbers <- df_total |>
        dplyr::filter(Total == 0)|>
        dplyr::pull(rowid)

      if (length(row_numbers) == 0){
        message("There are no Total counts of 0")
        return(NULL)
      }else{
        return(row_numbers)
      }
    }
  }, error = function(e) {
    message(paste("Error identifying counts of 0:", e$message))
  })
}
