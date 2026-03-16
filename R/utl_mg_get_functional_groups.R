#' Get functional group memberships for a set of scientific IDs
#'
#' @description
#' Returns all functional group memberships for the supplied `scientific_ids`
#' by walking the precomputed `functional_group_enrollment` nested tree stored
#' in `marinegeo_metadata`. Each row in the result represents one ancestor
#' node from the functional group lookup that the queried ID belongs to.
#'
#' Both `"APHIA:X"` species IDs and `"FUNCTIONAL:X"` group IDs are accepted as
#' input. IDs that do not appear in the tree produce zero rows, not an error.
#'
#' @param scientific_ids Character vector of `scientific_id` values to look up
#'   (e.g. `c("APHIA:495077", "FUNCTIONAL:9")`). `NA` values are removed with
#'   a message.
#'
#' @return A data frame with one row per (`scientific_id`, ancestor node) pair.
#'   Columns:
#'   \describe{
#'     \item{scientific_id}{The supplied identifier.}
#'     \item{parent_scientific_id}{`scientific_id` of the ancestor node from the
#'       functional group lookup. May be `"FUNCTIONAL:X"` (a named functional
#'       group) or `"APHIA:X"` (a taxonomic anchor node, e.g. a family used as
#'       an enrollment boundary).}
#'     \item{parent_name}{Display name of the ancestor node (e.g. `"Seagrass"`,
#'       `"Zosteraceae"`).}
#'     \item{depth}{Integer. Position of the ancestor node in the functional
#'       group hierarchy, counting all fg-lookup nodes from the root. The root
#'       node is depth 1; each child level increments by 1. Rows are ordered
#'       root-first (ascending depth) within each `scientific_id`.}
#'   }
#'   Returns a zero-row data frame with the above columns if no IDs are matched
#'   or if `scientific_ids` is empty after NA removal.
#'
#' @details
#' Functional group memberships are precomputed at package build time and
#' stored in `marinegeo_metadata$functional_group_enrollment` as a nested tree.
#' This function walks that tree — no on-the-fly enrollment computation occurs
#' at runtime.
#'
#' A species enrolled under a leaf node (e.g. an APHIA: family anchor) will
#' also appear as a member of all ancestor nodes up to the root. One row is
#' returned per ancestor, with `depth` indicating each node's position in the
#' hierarchy. The queried ID itself is never included as a result row —
#' only its ancestors are returned.
#'
#' When a `"FUNCTIONAL:X"` ID is supplied, the function returns all functional
#' group nodes that group is a child of (i.e. its ancestors in the tree).
#'
#' @export
#'
#' @examples
#' # Species lookup — returns all ancestor nodes from the functional group tree
#' utl_mg_get_functional_groups("APHIA:495077")
#'
#' # Functional group lookup — returns the groups it belongs to
#' utl_mg_get_functional_groups("FUNCTIONAL:9")
#'
#' # Batch lookup
#' utl_mg_get_functional_groups(c("APHIA:495077", "APHIA:111111"))
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

  empty_result <- data.frame(
    scientific_id = character(0),
    parent_scientific_id = character(0),
    parent_name = character(0),
    depth = integer(0),
    stringsAsFactors = FALSE
  )

  if (length(scientific_ids) == 0) {
    return(empty_result)
  }

  tree <- marinegeo_metadata$functional_group_enrollment

  rows <- lapply(scientific_ids, function(id) {
    matches <- .find_functional_groups(id, tree)
    if (length(matches) == 0) {
      return(empty_result)
    }
    data.frame(
      scientific_id = id,
      parent_scientific_id = vapply(matches, `[[`, character(1), "id"),
      parent_name = vapply(matches, `[[`, character(1), "name"),
      depth = vapply(matches, `[[`, integer(1), "depth"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


#' Walk the functional group tree to find all ancestor nodes for an ID
#'
#' @description
#' Recursively walks the `functional_group_enrollment` nested tree, collecting
#' every node that is an ancestor of `scientific_id`. The queried ID may be an
#' APHIA: species (found in a node's `members`), or any node ID present in the
#' tree itself (FUNCTIONAL: or APHIA: anchor node).
#'
#' The queried node is never included in its own results — only ancestors are
#' returned.
#'
#' @param scientific_id Character scalar. The identifier to search for.
#' @param tree Named list. The `functional_group_enrollment` nested tree as
#'   produced by `.build_functional_group_enrollment()`.
#'
#' @return An unnamed list of named lists, each with elements `id` (character),
#'   `name` (character), and `depth` (integer, 1 = root). Results are ordered
#'   root-first (ascending depth). Returns an empty list if `scientific_id` is
#'   not found anywhere in the tree.
#'
#' @keywords internal
.find_functional_groups <- function(scientific_id, tree) {
  results <- list()

  # Returns TRUE if scientific_id is in node$members OR matches any descendant
  # node_id anywhere in the subtree. Does not match the node itself.
  .subtree_contains <- function(node) {
    if (scientific_id %in% node$members) {
      return(TRUE)
    }
    for (child_id in names(node$children)) {
      if (scientific_id == child_id) {
        return(TRUE)
      }
      if (.subtree_contains(node$children[[child_id]])) return(TRUE)
    }
    FALSE
  }

  # depth counts all fg-lookup nodes traversed from the root.
  .walk <- function(node_id, node, depth) {
    current_depth <- depth + 1L
    # Collect this node if scientific_id is in its subtree, but never collect
    # the queried node itself.
    if (node_id != scientific_id && .subtree_contains(node)) {
      results[[length(results) + 1]] <<- list(
        id = node_id,
        name = node$name,
        depth = current_depth
      )
    }
    for (child_id in names(node$children)) {
      .walk(child_id, node$children[[child_id]], current_depth)
    }
  }

  for (root_id in names(tree)) {
    .walk(root_id, tree[[root_id]], 0L)
  }

  results
}
