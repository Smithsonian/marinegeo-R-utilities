# Internal helpers for traversing the MarineGEO taxonomic adjacency table.
#
# All functions in this file are internal (not exported). They are called at
# runtime by utl_mg_get_taxonomic_classifications() to reconstruct wide-form
# classifications from the `taxonomic_lookup` table.

# ---------------------------------------------------------------------------
# Taxonomic adjacency table traversal
# ---------------------------------------------------------------------------

#' Walk the taxonomic adjacency table upward from a given node
#'
#' @description
#' Collects the taxon name at each rank by walking from `node_id` up to the
#' root of the `taxonomic_lookup` adjacency table. Used by
#' `.get_taxonomic_classifications()`.
#'
#' @param node_id The `id` value of the starting node in `df`. May be numeric
#'   or character depending on how the adjacency table was loaded.
#' @param df Data frame. The adjacency table (e.g. `taxonomic_lookup`) with
#'   columns `id`, `scientific_id`, `parent_id`, `rank`, and `name`.
#'
#' @return A named list where each name is a rank label and each value is the
#'   taxon name at that rank.
#'
#' @details
#' The initial row is looked up by `id` (the raw adjacency-table key). All
#' subsequent steps navigate via `parent_id` → `scientific_id`, because
#' `parent_id` stores the `scientific_id` of the parent node (e.g.
#' `"APHIA:51"`), not the raw `id` value. This handles the case where `id` is
#' numeric and `parent_id` is character.
#'
#' @keywords internal
.get_parent_rank <- function(node_id, df) {
  parent_ranks <- list()

  # First lookup: by raw id column (may be numeric or character)
  row <- dplyr::filter(df, .data$id == node_id)

  while (nrow(row) > 0) {
    parent_ranks[[row$rank]] <- row$name
    parent_val <- row$parent_id
    if (is.na(parent_val)) {
      break
    }
    # parent_id holds the scientific_id of the parent — navigate by scientific_id
    row <- dplyr::filter(df, .data$scientific_id == parent_val)
  }

  parent_ranks
}


#' Get wide-form taxonomic classifications for a set of scientific IDs
#'
#' @description
#' For each Aphia-based `scientific_id` supplied, walks the MarineGEO
#' `taxonomic_lookup` adjacency table to reconstruct the full taxonomic
#' hierarchy and returns the result as a wide data frame with one column per
#' standard rank. Called at runtime by [utl_mg_get_taxonomic_classifications()].
#'
#' Only IDs that begin with `"APHIA:"` are present in `taxonomic_lookup`.
#' Functional group IDs (`"FUNCTIONAL:"`) will not be matched and are silently
#' excluded from the output.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:374534", "APHIA:145792")`). `NA` values are removed with
#'   a message.
#' @param taxonomic_lookup Data frame. The `taxonomic_lookup` adjacency table.
#'   Must have columns `scientific_id`, `id`, `parent_id`, `rank`, and `name`.
#'
#' @return A data frame with one row per matched `scientific_id` and columns:
#'   \describe{
#'     \item{scientific_id}{The input identifier.}
#'     \item{rank}{The taxonomic rank of the input ID itself (e.g. `"Species"`).}
#'     \item{Kingdom, Phylum, Class, Order, Family, Genus, Species}{Taxon name
#'       at each standard rank. Columns are only present when at least one row
#'       has a value; missing ranks for a given row are `NA`.}
#'   }
#'   Returns a zero-row data frame with the expected columns if no IDs are
#'   matched.
#'
#' @details
#' `Phylum (Division)` — the WoRMS rank name used for some plant and algae
#' lineages — is mapped to `Phylum` before the standard columns are selected.
#'
#' Intermediate ranks (Tribe, Forma, super-, sub-, infra-, mega-, giga-, parv-,
#' subter- prefixed ranks) are dropped from the output.
#'
#' @keywords internal
.get_taxonomic_classifications <- function(scientific_ids, taxonomic_lookup) {
  # --- Input validation -------------------------------------------------------
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
        scientific_id = character(0),
        rank = character(0),
        Kingdom = character(0),
        Phylum = character(0),
        Class = character(0),
        Order = character(0),
        Family = character(0),
        Genus = character(0),
        Species = character(0)
      )
    )
  }

  # --- Lookup -----------------------------------------------------------------
  taxonomic_lookup <- taxonomic_lookup |>
    dplyr::mutate(
      rank = dplyr::case_when(
        rank == "Phylum (Division)" ~ "Phylum",
        T ~ rank
      )
    )

  classifications_df <- taxonomic_lookup |>
    dplyr::filter(.data$scientific_id %in% scientific_ids) |>
    dplyr::mutate(
      parent_ranks = purrr::map(.data$id, \(id) {
        .get_parent_rank(id, taxonomic_lookup)
      }),
      classifications_df = purrr::map(.data$parent_ranks, \(a) {
        tibble::as_tibble(as.list(a))
      })
    ) |>
    dplyr::select("scientific_id", "classifications_df") |>
    tidyr::unnest("classifications_df") |>
    dplyr::select(
      "scientific_id",
      dplyr::any_of(c(
        "Kingdom",
        "Phylum",
        "Class",
        "Order",
        "Family",
        "Genus",
        "Species"
      ))
    )

  ranks_df <- taxonomic_lookup |>
    dplyr::filter(.data$scientific_id %in% scientific_ids) |>
    dplyr::select("scientific_id", "rank")

  classifications_df |>
    dplyr::left_join(ranks_df, by = "scientific_id") |>
    dplyr::select("scientific_id", "rank", dplyr::everything())
}
