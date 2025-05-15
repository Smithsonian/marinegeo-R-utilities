#' Check for vertebrates that have been given an invert count 
#'
#' Check that the rls dataset is a dataframe and contains columns `Inverts` and `phylum`
#' finds rows where the phylum is "Chordata" and the invert count column has a numerical value over 0. 
#'
#' @param df
#'
#' @returns a list of row numbers that are vertebrae species with an invert count assigned to them. 
#' @export
#'
#' @examples
#' rows_verts_with_invert_count <- qc_rls_verts_with_invert_count(df)
qc_rls_verts_with_invert_count <- function(df) {
  
  stopifnot("`df` is not a data frame" = is.data.frame(df))
  
  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(c("Inverts", "phylum"), colnames(df))

  # expect_error() 
  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }
  
  tryCatch({
    
    df_verts_with_invert_count <-df |>
      tibble::rowid_to_column() |>
      dplyr::filter(Inverts > 0 & phylum == "Chordata")
    
    row_numbers <- df_verts_with_invert_count$rowid
    
    if (length(row_numbers) == 0){
      message("There are no Vertebrates with an Invert Count.")
    }else{
      return(row_numbers)
    }
  }, error = function(e) {
    message(paste("Error identifying verts with invert count:", e$message))
  })
}
