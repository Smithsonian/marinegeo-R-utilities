#' Get wide-form taxonomic classifications for a set of scientific IDs
#'
#' @description
#' Reconstructs taxonomic classifications for the supplied `scientific_ids` by
#' walking the MarineGEO `taxonomic_lookup` adjacency table at call time. Each
#' row in the result corresponds to one matched `scientific_id`, with one column
#' per standard taxonomic rank.
#'
#' `taxonomic_lookup` is resolved through the live registry (with a bundled
#' fallback), so the classifications always reflect the current lookup table
#' rather than a precomputed snapshot.
#'
#' IDs that are not present in `taxonomic_lookup` produce zero rows, not an
#' error. Only `"APHIA:X"` identifiers are present in the table;
#' `"FUNCTIONAL:X"` IDs will never match and are silently excluded.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:495077", "APHIA:374534")`). `NA` values are removed with
#'   a message.
#' @param ranks Optional character vector of taxonomic ranks to keep in the
#'   output. Must be a subset of `"Kingdom"`, `"Phylum"`, `"Class"`, `"Order"`,
#'   `"Family"`, `"Genus"`, `"Species"`. When supplied, the returned rank columns
#'   are restricted to these (and ordered as given); `scientific_id` and `rank`
#'   are always retained. Defaults to `NULL` (all standard ranks).
#'
#' @return A data frame with one row per matched `scientific_id`. Columns:
#'   \describe{
#'     \item{scientific_id}{The supplied identifier.}
#'     \item{rank}{Taxonomic rank of the input ID itself (e.g. `"Species"`).}
#'     \item{Kingdom, Phylum, Class, Order, Family, Genus, Species}{Taxon name
#'       at each standard rank. A column is present only when at least one
#'       matched row has a value for that rank (or, when `ranks` is supplied,
#'       only the requested ranks); missing ranks for a given row are `NA`.}
#'   }
#'   Returns a zero-row data frame if no IDs are matched or if `scientific_ids`
#'   is empty after NA removal.
#'
#' @details
#' Classifications are reconstructed at call time by
#' `.get_taxonomic_classifications()`, which walks the `taxonomic_lookup`
#' adjacency table from each matched node up to the root. `Phylum (Division)` —
#' the WoRMS rank name used for some plant and algae lineages — is mapped to
#' `Phylum`, and intermediate ranks (Tribe, Forma, and super-/sub-/infra- etc.
#' prefixed ranks) are dropped.
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
#' # Restrict the output to a custom set of taxonomic levels
#' utl_mg_get_taxonomic_classifications(
#'   "APHIA:495077",
#'   ranks = c("Family", "Genus", "Species")
#' )
#'
#' # Unknown or FUNCTIONAL: IDs produce zero rows (no error)
#' utl_mg_get_taxonomic_classifications("APHIA:99999999")
utl_mg_get_taxonomic_classifications <- function(scientific_ids, ranks = NULL) {
  if (!is.character(scientific_ids)) {
    stop("`scientific_ids` must be a character vector.")
  }

  standard_ranks <- c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species"
  )

  if (!is.null(ranks)) {
    if (!is.character(ranks)) {
      stop("`ranks` must be a character vector or NULL.")
    }
    unknown <- setdiff(ranks, standard_ranks)
    if (length(unknown) > 0) {
      stop(
        "Unknown taxonomic rank(s): ",
        paste(paste0('"', unknown, '"'), collapse = ", "),
        ". Valid ranks are: ",
        paste(paste0('"', standard_ranks, '"'), collapse = ", "),
        "."
      )
    }
  }

  result <- .get_taxonomic_classifications(
    scientific_ids,
    taxonomic_lookup = .mg_get_registry_table("taxonomic_lookup")
  )

  if (!is.null(ranks)) {
    result <- result |>
      dplyr::select(dplyr::any_of(c("scientific_id", "rank", ranks)))
  }

  result
}
