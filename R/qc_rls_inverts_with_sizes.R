#' Check for invertebrates that have been assigned size counts
#'
#' Check that the rls dataset is a dataframe and contains columns `Total`, `Inverts`, `phylum`, and all the size count columns
#' finds rows where the phylum is not "Chordata" and any of the size coulmns have a numerical value over 0.
#'
#' @param df
#'
#' @returns a list of row numbers that are invert species with a size assigned to them.
#' @export
#'
#' @examples
#' rows_inverts_with_sizes <- qc_rls_inverts_with_sizes(df)
qc_rls_inverts_with_sizes <- function(df){

  stopifnot("`df` is not a data frame" = is.data.frame(df))

  # Check for necessary columns to process dataframe
  size_count_columns <- c(
    "2.5","5","7.5","10","12.5","15","20","25","30","35","40","50","62.5","75",
    "87.5","100","112.5","137.5","150","162.5","175","187.5","200","250","300","350","400"
  )

  missing_columns <- dplyr::setdiff(c("Total","Inverts", "phylum", size_count_columns), colnames(df))
  # expect_error()
  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  tryCatch({

    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(0))) |>
      dplyr::mutate(sum_size_row = rowSums(dplyr::across(dplyr::all_of(size_count_columns)), na.rm = TRUE))

    #Filter to the species observed and given sizes
    inverts_with_sizes <- df |>
      tibble::rowid_to_column() |>
      dplyr::filter(sum_size_row > 0 & phylum != "Chordata")

    row_numbers <- inverts_with_sizes$rowid

    if (length(row_numbers)==0){
      message("There are no Inverts with recorded sizes")
    }else{
    return(row_numbers)
    }
  }, error = function(e) {
    message(paste("Error identifying inverts with sizes:", e$message))
  })
}

