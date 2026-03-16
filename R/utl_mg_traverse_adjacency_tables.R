# Internal helpers for traversing MarineGEO taxonomic and functional group
# adjacency tables at sysdata build time.
#
# All functions in this file are internal (not exported). They are called from
# data-raw/assemble_marinegeo_metadata_sysdata.R via devtools::load_all().


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
    if (is.na(parent_val)) break
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
#' standard rank. Called at sysdata build time to precompute classifications.
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


# ---------------------------------------------------------------------------
# Functional group adjacency table traversal
# ---------------------------------------------------------------------------

#' Build functional group enrollment nested tree
#'
#' @description
#' Precomputes a nested tree structure representing functional group
#' memberships. Called at sysdata build time (from
#' `data-raw/assemble_marinegeo_metadata_sysdata.R`) — never at package
#' runtime.
#'
#' Walks the taxonomic adjacency table downward (BFS from each APHIA: anchor
#' node with `enroll_all_lower_ranks = TRUE`), filters enrolled species to
#' those present in either `functional_group_lookup` or `observation_lookup`,
#' and assembles a nested list where each FUNCTIONAL: and APHIA: node contains
#' its display name, the set of all enrolled APHIA: IDs at or below it, and
#' its child nodes.
#'
#' @param tl Data frame. `taxonomic_lookup` as read from CSV. Must have columns
#'   `scientific_id` (character `"APHIA:X"`), `parent_id` (character ID of
#'   parent), `name`, and `rank`.
#' @param fg Data frame. `functional_group_lookup` as read from CSV. Must have
#'   columns `scientific_id` (character `"APHIA:X"` or `"FUNCTIONAL:X"`),
#'   `parent_id` (character `"APHIA:X"`, `"FUNCTIONAL:X"`, or `NA`),
#'   `functional_group_name`, and `enroll_all_lower_ranks` (logical).
#' @param ol Data frame. `observation_lookup` as read from CSV. Must have
#'   column `scientific_id` (character `"APHIA:X"`). Used to restrict
#'   BFS-enrolled descendants to species known to MarineGEO.
#'
#' @return A named nested list representing the functional group hierarchy.
#'   Each element is named by `scientific_id` (e.g. `"FUNCTIONAL:1"`,
#'   `"APHIA:143770"`) and contains:
#'   \describe{
#'     \item{name}{Display name of the node (e.g. `"Biota"`, `"Zosteraceae"`).}
#'     \item{members}{Character vector of APHIA: IDs enrolled directly at this
#'       node. FUNCTIONAL: nodes always have `character(0)`. Each APHIA: ID
#'       appears at exactly one node — the deepest enrollment point in the
#'       tree. Filtered to IDs present in `fg$scientific_id` or
#'       `ol$scientific_id`.}
#'     \item{children}{Named list of child nodes, each with the same structure.
#'       Empty list for leaf nodes.}
#'   }
#'   Returns an empty list if `fg` has no rows.
#'
#' @details
#' Each APHIA: ID is stored only at the deepest node where it is enrolled —
#' members are not propagated upward. To determine which FUNCTIONAL: groups
#' contain a species, callers must search the full subtree (as
#' `.find_functional_groups()` does).
#'
#' Filtering: only APHIA: IDs present in `fg$scientific_id` or
#' `ol$scientific_id` are included in `members`. BFS descendants outside this
#' set are excluded.
#'
#' @keywords internal
.build_functional_group_enrollment <- function(tl, fg, ol) {
  if (nrow(fg) == 0) return(list())

  # --- Build allowed_aphia set -----------------------------------------------
  # Only species present in fg or ol are valid enrollment targets.
  aphia_from_fg <- fg$scientific_id[grepl("^APHIA:", fg$scientific_id)]
  aphia_from_ol <- ol$scientific_id[grepl("^APHIA:", ol$scientific_id)]
  allowed_aphia <- union(aphia_from_fg, aphia_from_ol)

  # --- Taxonomic children index (for BFS) ------------------------------------
  tl_valid <- tl[!is.na(tl$parent_id) & !is.na(tl$scientific_id), ]
  children_index <- split(tl_valid$scientific_id, tl_valid$parent_id)

  # Returns all taxonomic descendants of root_id (including root_id itself).
  .get_all_descendants <- function(root_id) {
    visited <- character(0)
    queue <- root_id
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      if (current %in% visited) next
      visited <- c(visited, current)
      kids <- children_index[[current]]
      if (!is.null(kids)) queue <- c(queue, kids)
    }
    visited
  }

  # --- Build flat node map from fg -------------------------------------------
  fg_unique <- fg[!duplicated(fg$scientific_id), ]
  node_map <- list()

  for (i in seq_len(nrow(fg_unique))) {
    row <- fg_unique[i, ]
    node_id <- row$scientific_id
    is_aphia <- grepl("^APHIA:", node_id)
    enroll_all <- isTRUE(row$enroll_all_lower_ranks)

    # Assign leaf members for APHIA: nodes; FUNCTIONAL: nodes start empty.
    members <- character(0)
    if (is_aphia) {
      if (enroll_all) {
        desc_ids <- .get_all_descendants(node_id)
        members <- intersect(desc_ids, allowed_aphia)
      } else if (node_id %in% allowed_aphia) {
        members <- node_id
      }
    }

    node_map[[node_id]] <- list(
      name      = row$functional_group_name,
      parent_id = row$parent_id,
      members   = members
    )
  }

  # --- Build parent -> children index for fg hierarchy ----------------------
  valid_ids <- names(node_map)
  children_by_parent <- list()
  for (node_id in valid_ids) {
    parent <- node_map[[node_id]]$parent_id
    if (!is.na(parent) && parent %in% valid_ids) {
      children_by_parent[[parent]] <- c(children_by_parent[[parent]], node_id)
    }
  }

  # --- Recursively assemble tree; members stay at the node they are assigned -
  .assemble_tree_node <- function(id) {
    node <- node_map[[id]]
    child_ids <- children_by_parent[[id]]
    child_nodes <- list()

    if (!is.null(child_ids)) {
      for (child_id in child_ids) {
        child_nodes[[child_id]] <- .assemble_tree_node(child_id)
      }
    }

    list(
      name     = node$name,
      members  = node$members,
      children = child_nodes
    )
  }

  # --- Find root nodes and assemble the full tree ----------------------------
  root_ids <- valid_ids[vapply(valid_ids, function(id) {
    parent <- node_map[[id]]$parent_id
    is.na(parent) || !(parent %in% valid_ids)
  }, logical(1))]

  if (length(root_ids) == 0) return(list())

  tree <- list()
  for (root_id in root_ids) {
    tree[[root_id]] <- .assemble_tree_node(root_id)
  }

  tree
}
