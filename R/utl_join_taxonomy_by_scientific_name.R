
#' Join taxonomic classification to input dataframe
#'
#' The function calls `utl_get_taxonomy_by_scientific_name()` to query the MarineGEO taxonomic
#' database by scientific name. If the sample is identified by a column not named "scientific_name",
#' it should be defined in the `identification_column_name` argument.
#'
#' @param df A data frame to join taxonomic levels to
#' @param identification_column_name Defaults to "scientific_name"
#' @param taxonomic_levels Vector of classification levels. By default, returns all available levels
#'
#' @returns A dataframe with taxonomic levels as columns
#' @export
#'
#' @examples
#' df <- readxl::read_excel("inst/extdata/test_rls_data_EPA.xlsx", sheet = "DATA") |>
#'  dplyr::filter(Method == "1" | Method == "2")
#' utl_join_taxonomy_by_scientific_name(df, identification_column_name = "Species")
#' utl_join_taxonomy_by_scientific_name(df, identification_column_name = "Species", "phylum")
utl_join_taxonomy_by_scientific_name <- function(df,
                                                 identification_column_name = "scientific_name",
                                                 taxonomic_levels = NULL){

  # Test inputs
  stopifnot("`df` must be a dataframe" = is.data.frame(df))
  stopifnot("`identification_column_name` must be a character" = is.character(identification_column_name))

  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(identification_column_name, colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
  }

  tryCatch({

    # If necessary, rename column to allow join with taxonomic database
    if(identification_column_name != "scientific_name"){

      df <- df |>
        dplyr::rename(scientific_name := dplyr::all_of(identification_column_name))

    }

    scientific_names <- df |>
      dplyr::select(scientific_name) |>
      dplyr::distinct() |>
      dplyr::pull(scientific_name)

    taxonomic_lookup_df <- db_get_taxonomy_by_scientific_name(scientific_names, taxonomic_levels)

    df_with_taxonomy <- df |>
      dplyr::left_join(taxonomic_lookup_df, by = "scientific_name")

    return(df_with_taxonomy)

  }, error = function(e){

    cat("Error in utl_join_taxonomy_by_scientific_name():", e$message, "\n")
    return(NULL)

  })

}
