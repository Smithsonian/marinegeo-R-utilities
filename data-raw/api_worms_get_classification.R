
#' Query WORMS API for taxonomic levels with Aphia ID
#'
#' Returns all associated taxon ranks for a Aphia ID (e.g., kingdom, phylum, genus, species).
#'
#' @param taxonomic_ids
#'
#' @return A dataframe with a column for `scientific_name` and `taxonomic_id` with accompanying columns for each taxon rank.
#' @export
#'
#' @examples
#' phylum <- 51 # Mollusca
#' api_worms_get_classification(phylum)
api_worms_get_classification <- function(taxonomic_ids){

  # Make sure taxonomic_ids is a integer vector
  stopifnot("`taxonomic_ids` should be an integer object or vector" = is.numeric(taxonomic_ids))

  tryCatch({

    classifications_raw <- worrms::wm_classification_(taxonomic_ids)

    classifications_long <- classifications_raw |>
      dplyr::rename(taxonomic_id = id) |>
      dplyr::select(-AphiaID) |>
      dplyr::mutate(rank = dplyr::case_when(
        rank == "Phylum (Division)" ~ "phylum_division",
        rank == "Subphylum (Subdivision)" ~ "subphylum_subdivision",
        T ~ tolower(rank)
      ))

    classifications_finest <- classifications_long |>
      dplyr::group_by(taxonomic_id) |>
      dplyr::summarize(level = dplyr::last(rank))

    eval <- classifications_long |>
      dplyr::count(taxonomic_id, rank) |>
      dplyr::filter(n > 1)

    if(nrow(eval) > 1){

      # Remove dups before proceeding
      duplicate_ids <- eval |>
        dplyr::pull(taxonomic_id)

      message(length(duplicate_ids), " IDs have duplicate taxonomic ranks returned from WORMS. These Aphia IDs are removed from the output.")

      classifications_long <- classifications_long |>
        dplyr::filter(!taxonomic_id %in% duplicate_ids)

      classifications_finest <- classifications_finest |>
        dplyr::filter(!taxonomic_id %in% duplicate_ids)

    }

    classifications_wide <- classifications_long |>
      tidyr::pivot_wider(id_cols = taxonomic_id,
                         names_from = rank,
                         values_from = scientificname) |>
      dplyr::left_join(classifications_finest, by = "taxonomic_id") |>
      dplyr::select(taxonomic_id, level, everything()) |>
      dplyr::mutate(taxonomic_id = as.numeric(taxonomic_id))

    return(classifications_wide)

  }, error = function(e){

    cat("Error in api_worms_get_classification():", e$message, "\n")
    return(NULL)

  })

}
