#' Get wide-form taxonomic classifications for a set of scientific IDs
#'
#' @description
#' Returns precomputed taxonomic classifications for the supplied
#' `scientific_ids` by filtering the wide-form
#' `marinegeo_metadata$taxonomic_classifications` table. Each row in the
#' result corresponds to one matched `scientific_id`, with one column per
#' standard taxonomic rank.
#'
#' IDs that are not present in the classifications table produce zero rows,
#' not an error. Only `"APHIA:X"` identifiers are present in the table;
#' `"FUNCTIONAL:X"` IDs will never match and are silently excluded.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:495077", "APHIA:374534")`). `NA` values are removed with
#'   a message.
#'
#' @return A data frame with one row per matched `scientific_id`. Columns:
#'   \describe{
#'     \item{scientific_id}{The supplied identifier.}
#'     \item{rank}{Taxonomic rank of the input ID itself (e.g. `"Species"`).}
#'     \item{Kingdom, Phylum, Class, Order, Family, Genus, Species}{Taxon name
#'       at each standard rank. A column is present only when at least one
#'       matched row has a value for that rank; missing ranks for a given row
#'       are `NA`.}
#'   }
#'   Returns a zero-row data frame with columns `scientific_id` and `rank` (no
#'   rank columns) if no IDs are matched or if `scientific_ids` is empty after
#'   NA removal.
#'
#' @details
#' Classifications are precomputed at package build time by
#' `.get_taxonomic_classifications()` and stored in
#' `marinegeo_metadata$taxonomic_classifications`. This function performs a
#' single filter — no tree traversal occurs at runtime.
#'
#' @export
#'
#' @examples
#' # Single species lookup
#' utl_mg_get_taxonomic_classifications("APHIA:495077")
#'
#' # Batch lookup
#' utl_mg_get_taxonomic_classifications(c("APHIA:495077", "APHIA:374534"))
#'
#' # Unknown or FUNCTIONAL: IDs produce zero rows (no error)
#' utl_mg_get_taxonomic_classifications("APHIA:99999999")
utl_mg_get_taxonomic_classifications <- function(scientific_ids) {
  if (!is.character(scientific_ids)) {
    stop("`scientific_ids` must be a character vector.")
  }

  na_count <- sum(is.na(scientific_ids))
  if (na_count > 0) {
    message(na_count, " NA value(s) removed from `scientific_ids`.")
    scientific_ids <- scientific_ids[!is.na(scientific_ids)]
  }

  if (length(scientific_ids) == 0) {
    return(data.frame(
      scientific_id = character(0),
      rank = character(0),
      stringsAsFactors = FALSE
    ))
  }

  marinegeo_metadata$taxonomic_classifications |>
    dplyr::filter(.data$scientific_id %in% scientific_ids)
}
