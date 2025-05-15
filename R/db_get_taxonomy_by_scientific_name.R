#' Get taxonomic classifications by scientific name
#'
#' @param scientific_names Vector of scientific names to get taxonomic classifications for
#' @param taxonomic_levels Vector of classification levels. By default, returns all available levels
#'
#' @returns dataframe of taxonomic classifications and taxonomic ID (usually Aphia ID)
#' @export
#'
#' @examples
#' utl_get_taxonomy_by_scientific_name(c("Gastropoda", "Dardanus venosus"))
#' utl_get_taxonomy_by_scientific_name("Dardanus venosus", taxonomic_levels = "phylum")
#' utl_get_taxonomy_by_scientific_name("Dardanus venosus", taxonomic_levels = "pilum") # misspelling

db_get_taxonomy_by_scientific_name <- function(scientific_names,
                                                taxonomic_levels = NULL){

  # Test inputs
  stopifnot("`scientific_names` must be a character vector" = is.character(scientific_names))

  tryCatch({

    taxonomy_subset <- marinegeo_resources$taxonomic_ids |>
      dplyr::filter(scientific_name %in% scientific_names) |>
      dplyr::left_join(marinegeo_resources$taxonomic_classifications, by = "taxonomic_id")

    no_matches <- scientific_names[!scientific_names %in% taxonomy_subset$scientific_name]

    if(nrow(taxonomy_subset) == 0){

      warning("There are no matches in the taxonomic database")

    } else if(length(no_matches) > 1){

      warning(
        length(no_matches), " scientific names could not be matched in taxonomic database"
      )

    }

    if(!is.null(taxonomic_levels)){

      # Check for necessary taxonomic columns
      missing_columns <- dplyr::setdiff(taxonomic_levels, colnames(taxonomy_subset))

      if(length(missing_columns) > 0) {

        allowed_columns <- tibble::tibble(column_names = colnames(taxonomy_subset)) |>
          dplyr::filter(!column_names %in% c("taxonomic_id", "scientific_name", "level")) |>
          dplyr::distinct() |>
          dplyr::pull(column_names)

        warning(paste("The following taxonomic level(s) is not defined in the database table:", paste(missing_columns, collapse = ", ")))

        message("The following taxonomic levels are accepted: ", paste(allowed_columns, collapse = ", "))

      }

      taxonomy_level_subset <- taxonomy_subset |>
        dplyr::select(
          dplyr::any_of(c("scientific_name", "taxonomic_id", taxonomic_levels))
        )

      return(taxonomy_level_subset)

    } else return(taxonomy_subset)

  }, error = function(e){

    cat("Error in db_get_taxonomy_by_scientific_name():", e$message, "\n")
    return(NULL)

  })
}




