#' Get functional group memberships for a set of scientific IDs
#'
#' @description
#' Returns all functional group enrollments for the supplied `scientific_ids`
#' by filtering the precomputed `functional_group_enrollment` table stored in
#' `marinegeo_metadata`. Each row in the result represents one
#' (species, functional group) membership.
#'
#' IDs that do not appear in the enrollment table are silently excluded from
#' output (they have no functional group assignment). Unknown IDs produce zero
#' rows, not an error.
#'
#' Both `"APHIA:X"` and `"FUNCTIONAL:X"` identifiers are accepted.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:495077", "FUNCTIONAL:9")`). `NA` values are removed with
#'   a message.
#'
#' @return A data frame (long format) with one row per (scientific_id,
#'   functional group) pair. Columns:
#'   \describe{
#'     \item{scientific_id}{The supplied identifier.}
#'     \item{functional_group_id}{`"FUNCTIONAL:X"` of the functional group
#'       this ID belongs to (the deepest FUNCTIONAL: node in the enrollment
#'       lineage).}
#'     \item{functional_group_name}{Display name of `functional_group_id`
#'       (e.g. `"Seagrass"`).}
#'     \item{lineage}{Arrow-delimited path from the root functional group
#'       down to the anchor node that caused enrollment, e.g.
#'       `"Biota > Macrophytes > Submerged Aquatic Vegetation > Seagrass >
#'       Zosteraceae"`. The species name itself is not appended.}
#'     \item{enrolled_via}{`"direct"` if the ID is explicitly listed in the
#'       functional group lookup, or `"enroll_all_lower_ranks"` if enrollment
#'       was inherited from a taxonomic ancestor node that carries the
#'       `enroll_all_lower_ranks` flag.}
#'     \item{anchor_id}{The `scientific_id` of the node in
#'       `functional_group_lookup` that caused this enrollment (may differ
#'       from `scientific_id` when `enrolled_via = "enroll_all_lower_ranks"`).}
#'   }
#'   Returns a zero-row data frame with the above columns if no IDs are matched
#'   or if `scientific_ids` is empty after NA removal.
#'
#' @details
#' Functional group memberships are precomputed at package build time and
#' stored in `marinegeo_metadata$functional_group_enrollment`. This function
#' performs a single `dplyr::filter()` — no tree traversal occurs at runtime.
#'
#' A species may belong to more than one functional group (e.g. a taxon
#' enrolled at the family level AND at a higher class level). In that case,
#' multiple rows are returned for the same `scientific_id`.
#'
#' @export
#'
#' @examples
#' # Single species lookup
#' utl_mg_get_functional_groups("APHIA:495077")
#'
#' # Batch lookup
#' utl_mg_get_functional_groups(c("APHIA:495077", "FUNCTIONAL:9"))
#'
#' # Unknown IDs produce zero rows (no error)
#' utl_mg_get_functional_groups("APHIA:99999999")
utl_mg_get_functional_groups <- function(scientific_ids) {
  if (!is.character(scientific_ids)) {
    stop("`scientific_ids` must be a character vector.")
  }

  na_count <- sum(is.na(scientific_ids))
  if (na_count > 0) {
    message(na_count, " NA value(s) removed from `scientific_ids`.")
    scientific_ids <- scientific_ids[!is.na(scientific_ids)]
  }

  if (length(scientific_ids) == 0) {
    return(
      data.frame(
        scientific_id       = character(0),
        functional_group_id = character(0),
        functional_group_name = character(0),
        lineage             = character(0),
        enrolled_via        = character(0),
        anchor_id           = character(0)
      )
    )
  }

  marinegeo_metadata$functional_group_enrollment |>
    dplyr::filter(.data$scientific_id %in% scientific_ids)
}
