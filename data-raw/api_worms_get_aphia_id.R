#' Query WORMS API for Aphia ID from species identification name
#'
#' Wrapper around functions from worrms package with special functionality
#' to deal with issues commonly seen with MarineGEO IDs (sp., spp., A, B, C, etc.)
#' and to format the output
#'
#' @param scientific_names
#'
#' @returns a dataframe with 3 columns: taxonomic ID (Aphia ID), scientific_name, and name_to_evaluate (the string used to query WORMS API)
#' @export
#'
#' @examples
#' phylum <- "Mollusca" # 51
#' api_worms_get_aphia_id(phylum)
api_worms_get_aphia_id <- function(scientific_names){

  # Make sure scientific_names is a character vector
  stopifnot("`scientific_names` should be a character object or vector" = is.character(scientific_names))

  tryCatch({

    input_df <- tibble::tibble(
      scientific_name = scientific_names
    ) |>
      dplyr::mutate(name_to_evaluate = dplyr::case_when(
        stringr::str_ends(scientific_name, " sp.") ~ gsub(" sp.", "", scientific_name),
        stringr::str_ends(scientific_name, " spp.") ~ gsub(" spp.", "", scientific_name),
        T ~ scientific_name
      ))

    exact_match_list <- worrms::wm_name2id_(input_df$name_to_evaluate)

    validated_exact_match <- tibble::tibble(
      name_to_evaluate = names(exact_match_list),
      taxonomic_id = unname(unlist(exact_match_list))
    ) |>
      dplyr::filter(taxonomic_id != "Not found") |>
      dplyr::mutate(taxonomic_id = as.numeric(taxonomic_id)) |>
      dplyr::filter(taxonomic_id != -999) |>
      dplyr::right_join(input_df, by = "name_to_evaluate") |>
      dplyr::arrange(taxonomic_id)

    missing <- scientific_names[!scientific_names %in% validated_exact_match$name_to_evaluate]

  }, error = function(e){

    cat("Error in api_worms_get_aphia_id():", e$message, "\n")
    validated_exact_match <- NULL
    missing <- NULL

  })

  if(length(missing) > 0){
    warning(length(missing), " scientific names could not be matched.")
  }

  return(validated_exact_match)
}
