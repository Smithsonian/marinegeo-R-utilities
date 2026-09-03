#' Assign the nearest matching ancestor label to each scientific name
#'
#' @description
#' For use inside [dplyr::mutate()]. Walks up a functional group tree from each
#' scientific name and returns the label of the *nearest* ancestor whose tree
#' attributes match the criteria supplied in `...`.
#'
#' Where [utl_mg_assign_functional_groups()] requires you to enumerate the
#' candidate group labels up front, this function selects the ancestor by
#' matching against any column of the tree — so `type = "primary"` returns
#' whichever primary group the name falls under, and `rank = "Family"` returns
#' its family, without either label being known in advance.
#'
#' @param fg_tree Character scalar. Name of the functional group tree to query
#'   (e.g. `"vegetation"`), matched against the `tree_name` column of
#'   `functional_group_lookup`.
#' @param scientific_names Character vector of scientific names to classify,
#'   typically a column from a data frame passed through [dplyr::mutate()].
#'   `NA` values are preserved as `NA` in the output without a message.
#' @param ... One or more named `column = value` criteria identifying the
#'   ancestor to return, e.g. `type = "primary"` or `rank = "Family"`. Each
#'   name must be a column of `functional_group_lookup` other than `from`,
#'   `to`, or `tree_name`; each value must be a length-1, non-`NA` atomic
#'   value. When several criteria are given they are combined with **and** —
#'   a single ancestor must satisfy all of them.
#'
#' @return A character vector of the same length as `scientific_names`. Each
#'   element is the label of the nearest matching ancestor, or `NA_character_`
#'   if:
#'   \itemize{
#'     \item The name is `NA`.
#'     \item The name is not found in `observation_lookup` (a `warning()` is
#'       raised by [utl_mg_get_scientific_id()]).
#'     \item The name's identifier does not appear in `fg_tree`.
#'     \item No node on the path from the name to the root satisfies `...`.
#'   }
#'   The last two cases are reported together in a single `message()` listing
#'   the affected names.
#'
#' @details
#' Names are resolved to identifiers by [utl_mg_get_scientific_id()], which
#' strips trailing rank abbreviations (e.g. `"sp."`, `"spp."`) before matching.
#' The tree is then built once and traversed for every name.
#'
#' Matching is exact and case-sensitive, consistent with
#' [utl_mg_get_functional_groups()]. `NA` cells never match, so a node with no
#' `type` is never returned by `type = "primary"`.
#'
#' The queried node itself is eligible: asking for `rank = "Species"` with a
#' species name returns that name's own label. Traversal then proceeds upward
#' and stops at the first — that is, the most specific — matching ancestor.
#'
#' Root nodes are not matchable. The lookup table stores one row per *child*,
#' so a tree's root (e.g. `"Vegetation"`) has no row of its own and therefore
#' no attributes to match against.
#'
#' Note that `rank` and `type` partition the tree: `rank` is populated only on
#' taxonomic nodes and is `NA` on `FUNCTIONAL:` group nodes, while `type` and
#' `code` are populated only on curated group nodes.
#'
#' Two WoRMS rank spellings are normalized before matching, mirroring
#' [utl_mg_get_taxonomic_classifications()]: `"Phylum (Division)"` is treated
#' as `"Phylum"` and `"Subphylum (Subdivision)"` as `"Subphylum"`, so
#' `rank = "Phylum"` also matches plant and algae lineages.
#'
#' @seealso [utl_mg_assign_functional_groups()] to match against a known set of
#'   candidate group labels instead.
#'
#' @export
#'
#' @examples
#' # Nearest primary functional group
#' utl_mg_assign_ancestor_labels(
#'   fg_tree = "vegetation",
#'   scientific_names = c("Zostera marina", "Halodule wrightii"),
#'   type = "primary"
#' )
#'
#' # Nearest ancestor at a given taxonomic rank
#' utl_mg_assign_ancestor_labels(
#'   fg_tree = "vegetation",
#'   scientific_names = "Zostera marina",
#'   rank = "Family"
#' )
#'
#' # Inside mutate()
#' seagrass_cover_example |>
#'   dplyr::mutate(
#'     primary_group = utl_mg_assign_ancestor_labels(
#'       fg_tree = "vegetation",
#'       scientific_names = scientific_name,
#'       type = "primary"
#'     )
#'   )
utl_mg_assign_ancestor_labels <- function(fg_tree, scientific_names, ...) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(fg_tree) || length(fg_tree) != 1 || is.na(fg_tree)) {
    stop("`fg_tree` must be a single non-NA character value.")
  }
  if (!is.character(scientific_names)) {
    stop("`scientific_names` must be a character vector.")
  }

  criteria <- list(...)

  if (length(criteria) == 0) {
    stop(
      "At least one `column = value` criterion must be supplied to `...`, ",
      "for example `type = \"primary\"` or `rank = \"Family\"`."
    )
  }

  criteria_names <- names(criteria)
  if (is.null(criteria_names) || any(criteria_names == "")) {
    stop(
      "All arguments in `...` must be named, for example `type = \"primary\"`."
    )
  }
  if (anyDuplicated(criteria_names) > 0) {
    stop(
      "Duplicate criteria supplied to `...`: ",
      paste(
        unique(criteria_names[duplicated(criteria_names)]),
        collapse = ", "
      ),
      "."
    )
  }

  bad_values <- !vapply(
    criteria,
    function(v) is.atomic(v) && length(v) == 1 && !is.na(v),
    logical(1)
  )
  if (any(bad_values)) {
    stop(
      "Each value in `...` must be a length-1, non-NA atomic value. ",
      "Problem argument(s): ",
      paste(criteria_names[bad_values], collapse = ", "),
      "."
    )
  }

  # --- Build the tree ---------------------------------------------------------
  built <- .mg_build_fg_tree(fg_tree)
  fg <- built$fg
  fg_tree_obj <- built$tree

  # `from`/`to` are the graph edges and `tree_name` is already fixed by
  # `fg_tree`, so matching on them is always a mistake.
  reserved <- c("from", "to", "tree_name")
  matchable <- setdiff(names(fg), reserved)

  unknown <- setdiff(criteria_names, matchable)
  if (length(unknown) > 0) {
    stop(
      "Unknown criteria column(s) for tree '",
      fg_tree,
      "': ",
      paste(unknown, collapse = ", "),
      ". Matchable columns are: ",
      paste(sort(matchable), collapse = ", "),
      "."
    )
  }

  # Normalize the two WoRMS rank spellings that carry a parenthetical, so
  # `rank = "Phylum"` also matches plant and algae lineages. Mirrors the
  # remapping in `.get_taxonomic_classifications()`.
  if ("rank" %in% names(fg)) {
    fg$rank <- dplyr::case_when(
      fg$rank == "Phylum (Division)" ~ "Phylum",
      fg$rank == "Subphylum (Subdivision)" ~ "Subphylum",
      TRUE ~ fg$rank
    )
  }

  # Labels of every node satisfying all criteria. NA cells never match.
  satisfies <- rep(TRUE, nrow(fg))
  for (col in criteria_names) {
    values <- fg[[col]]
    satisfies <- satisfies & !is.na(values) & values == criteria[[col]]
  }
  matching_labels <- fg$from[satisfies]

  # --- Resolve scientific names to scientific_ids -----------------------------
  ids <- utl_mg_get_scientific_id(scientific_names)

  obs_lookup <- dplyr::tibble(
    scientific_name = scientific_names,
    scientific_id = ids
  ) |>
    dplyr::filter(!is.na(.data$scientific_name), !is.na(.data$scientific_id)) |>
    dplyr::distinct()

  unique_ids <- unique(obs_lookup$scientific_id)

  if (length(unique_ids) == 0 || length(matching_labels) == 0) {
    return(rep(NA_character_, length(scientific_names)))
  }

  # --- Walk each name's path upward, nearest ancestor first -------------------
  id_labels <- stats::setNames(
    vapply(
      unique_ids,
      function(id) {
        node_display_name <- fg$from[fg$scientific_id == id]
        if (length(node_display_name) == 0) {
          return(NA_character_)
        }

        found <- data.tree::FindNode(fg_tree_obj, node_display_name[1])
        if (is.null(found)) {
          return(NA_character_)
        }

        # `$path` runs root -> node, so reverse it to walk upward from the
        # node itself and stop at the most specific match.
        upward <- rev(found$path)
        hit <- upward[upward %in% matching_labels]
        if (length(hit) == 0) NA_character_ else hit[1]
      },
      character(1)
    ),
    unique_ids
  )

  name_label <- obs_lookup |>
    dplyr::mutate(label = unname(id_labels[.data$scientific_id]))

  # --- Report names with no matching ancestor ---------------------------------
  criteria_text <- paste0(
    criteria_names,
    " = ",
    vapply(criteria, function(v) paste0("\"", v, "\""), character(1)),
    collapse = ", "
  )

  no_match_names <- unique(name_label$scientific_name[is.na(name_label$label)])
  if (length(no_match_names) > 0) {
    message(
      length(no_match_names),
      " scientific name(s) had no ancestor matching ",
      criteria_text,
      " in tree '",
      fg_tree,
      "' and will be assigned NA: ",
      paste(no_match_names, collapse = ", ")
    )
  }

  # --- Expand to full-length output -------------------------------------------
  assignment_map <- stats::setNames(
    name_label$label,
    name_label$scientific_name
  )

  result <- unname(assignment_map[scientific_names])
  result[is.na(scientific_names)] <- NA_character_

  result
}
