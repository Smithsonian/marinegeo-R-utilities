#' Get functional group memberships for a set of scientific IDs
#'
#' @description
#' Returns all functional group memberships for the supplied `scientific_ids`
#' by traversing the `functional_group_lookup` edge-list stored in
#' `marinegeo_metadata` using the `data.tree` package. Each row in the result
#' represents one `FUNCTIONAL:` ancestor node that the queried ID belongs to.
#'
#' Both `"APHIA:X"` species IDs and `"FUNCTIONAL:X"` group IDs are accepted as
#' input. IDs that do not appear in the tree produce zero rows, not an error.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:495077", "FUNCTIONAL:9")`). `NA` values are removed with
#'   a message.
#' @param functional_group_tree Character scalar. Name of the functional group
#'   tree to query (e.g. `"vegetation"`). Used to filter rows in
#'   `marinegeo_metadata$functional_group_lookup` by the `tree_name` column.
#'
#' @return A data frame with one row per (`scientific_id`, `FUNCTIONAL:` ancestor)
#'   pair. Columns:
#'   \describe{
#'     \item{scientific_id}{The supplied identifier.}
#'     \item{group_id}{`scientific_id` of the ancestor `FUNCTIONAL:` node
#'       (e.g. `"FUNCTIONAL:2"`).}
#'     \item{group_name}{Display name of the ancestor node (e.g. `"Macrophytes"`).}
#'   }
#'   Returns a zero-row data frame with the above columns if no IDs are matched
#'   or if `scientific_ids` is empty after NA removal.
#'
#' @details
#' The `functional_group_lookup` table in `marinegeo_metadata` is an edge-list
#' data frame (columns `from`, `to`, `node_name`, `tree_name`) compatible with
#' `data.tree::FromDataFrameNetwork()`. At query time the relevant tree is built
#' from this table, the supplied ID is located via `data.tree::FindNode()`, and
#' all `FUNCTIONAL:`-prefixed ancestor nodes on the path from root to that ID
#' are returned.
#'
#' When a `"FUNCTIONAL:X"` ID is supplied, that node itself is included in
#' the results alongside its ancestors. `"APHIA:X"` IDs (species or taxonomic
#' anchor nodes) return only their `FUNCTIONAL:` ancestors, not themselves.
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

  fg <- marinegeo_metadata$functional_group_lookup |>
    dplyr::filter(tree_name == functional_group_tree)

  fg_tree <- data.tree::FromDataFrameNetwork(
    fg,
    check = c("check", "no-warn", "no-check")
  )

  dplyr::bind_rows(
    lapply(scientific_ids, function(id) {
      found <- data.tree::FindNode(fg_tree, id)
      parents <- found$path
      fg |>
        dplyr::filter(from %in% parents) |>
        dplyr::mutate(scientific_id = id)
    })
  ) |>
    dplyr::filter(stringr::str_starts(from, "FUNCTIONAL:")) |>
    dplyr::select(scientific_id, from, node_name) |>
    dplyr::rename(group_id = from, group_name = node_name)
}
