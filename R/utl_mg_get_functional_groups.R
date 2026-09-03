#' Get functional group memberships for a set of scientific IDs
#'
#' @description
#' Returns all functional group memberships for the supplied `scientific_ids`
#' by traversing the `functional_group_lookup` edge-list stored in
#' `marinegeo_metadata` using the `data.tree` package. Each row in the result
#' represents one group-level ancestor node that the queried ID belongs to.
#'
#' Both `"APHIA:X"` species IDs and `"FUNCTIONAL:X"` group IDs are accepted as
#' input. IDs that do not appear in the tree produce zero rows, not an error.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:495077", "FUNCTIONAL:SAV")`). `NA` values are removed with
#'   a message.
#' @param functional_group_tree Character scalar. Name of the functional group
#'   tree to query (e.g. `"vegetation"`). Used to filter rows in
#'   `marinegeo_metadata$functional_group_lookup` by the `tree_name` column.
#'
#' @return A data frame with one row per (`scientific_id`, group-level ancestor)
#'   pair. Columns:
#'   \describe{
#'     \item{scientific_id}{The supplied identifier.}
#'     \item{group_id}{`scientific_id` of the ancestor group-level node
#'       (e.g. `"FUNCTIONAL:SAV"`, or an Aphia ID when the group is labeled
#'       with one instead — see Details).}
#'     \item{group_name}{Display name of the ancestor node (e.g. `"Seagrass"`).}
#'   }
#'   Returns a zero-row data frame with the above columns if no IDs are matched
#'   or if `scientific_ids` is empty after NA removal.
#'
#' @details
#' The `functional_group_lookup` table in `marinegeo_metadata` is an edge-list
#' data frame with columns `from` (child node display name), `to` (parent node
#' display name), `scientific_id` (the child node's scientific identifier),
#' `type`, and `tree_name`. At query time the relevant tree is built from this
#' table, the node corresponding to the supplied `scientific_id` is located
#' via `data.tree::FindNode()`, and all group-level ancestor nodes on the path
#' from root to that node are returned.
#'
#' A node counts as "group-level" if its `scientific_id` is `FUNCTIONAL:`-
#' prefixed *or* its `type` column is `"primary"`. Most trees label every
#' group with a synthetic `"FUNCTIONAL:X"` id, but some (e.g. `oyster_density`)
#' label a group with a real Aphia ID instead (e.g. `"Gastropods"`) — `type ==
#' "primary"` flags those group nodes so they are still returned as ancestors
#' even though their `scientific_id` isn't `FUNCTIONAL:`-prefixed.
#'
#' When a `"FUNCTIONAL:X"` ID is supplied, that node itself is included in
#' the results alongside its ancestors. `"APHIA:X"` IDs (species or taxonomic
#' anchor nodes) return only their group-level ancestors, not themselves —
#' unless the queried ID itself is a group-level (`type == "primary"`) node.
#'
#' @export
#'
#' @examples
#' # Species lookup — returns all FUNCTIONAL: ancestor nodes
#' utl_mg_get_functional_groups("APHIA:144474", functional_group_tree = "vegetation")
#'
#' # Functional group lookup — returns the group itself and its ancestors
#' utl_mg_get_functional_groups("FUNCTIONAL:SEAGRASS", functional_group_tree = "vegetation")
#'
#' # Batch lookup
#' utl_mg_get_functional_groups(
#'   c("APHIA:144474", "APHIA:208925"),
#'   functional_group_tree = "vegetation"
#' )
#'
#' # Unknown IDs produce zero rows (no error)
#' utl_mg_get_functional_groups("APHIA:99999999", functional_group_tree = "vegetation")
utl_mg_get_functional_groups <- function(
  scientific_ids,
  functional_group_tree
) {
  if (!is.character(scientific_ids)) {
    stop("`scientific_ids` must be a character vector.")
  }

  na_count <- sum(is.na(scientific_ids))
  if (na_count > 0) {
    message(na_count, " NA value(s) removed from `scientific_ids`.")
    scientific_ids <- scientific_ids[!is.na(scientific_ids)]
  }

  empty_result <- data.frame(
    scientific_id = character(0),
    group_id = character(0),
    group_name = character(0),
    stringsAsFactors = FALSE
  )

  if (length(scientific_ids) == 0) {
    return(empty_result)
  }

  fg <- .mg_get_registry_table("functional_group_lookup") |>
    dplyr::filter(tree_name == functional_group_tree)

  # Some trees label a group-level node with a real Aphia ID instead of a
  # synthetic "FUNCTIONAL:" id (e.g. "Gastropods" in the oyster_density
  # tree), so `type == "primary"` is used alongside the "FUNCTIONAL:" prefix
  # to identify group-level ancestors below. Default to NA if `type` is
  # absent from the table entirely.
  if (!"type" %in% names(fg)) {
    fg$type <- NA_character_
  }

  # CSV uses from=child, to=parent; swap to match data.tree's from=parent, to=child convention
  # dplyr::select (not rename) is required to also reorder columns — data.tree reads by position
  fg_tree <- fg |>
    dplyr::select(from = to, to = from, dplyr::everything()) |>
    data.tree::FromDataFrameNetwork(
      check = c("check", "no-warn", "no-check")
    )

  rows <- dplyr::bind_rows(lapply(scientific_ids, function(id) {
    node_display_name <- fg$from[fg$scientific_id == id]
    if (length(node_display_name) == 0) {
      return(NULL)
    }

    found <- data.tree::FindNode(fg_tree, node_display_name)
    fg |>
      dplyr::filter(from %in% found$path) |>
      dplyr::filter(
        stringr::str_starts(scientific_id, "FUNCTIONAL:") |
          (!is.na(type) & type == "primary")
      ) |>
      dplyr::mutate(queried_id = id)
  }))

  if (nrow(rows) == 0) {
    return(empty_result)
  }

  rows |>
    dplyr::select(queried_id, group_id = scientific_id, group_name = from) |>
    dplyr::rename(scientific_id = queried_id)
}
