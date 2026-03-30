# Prototype: Nearest Common Ancestor (NCA) for functional group / taxonomic trees
#
# Explores two operations:
#
#   1. find_nearest_common_ancestor()
#      Given a flat vector of scientific IDs, return the deepest node that is
#      an ancestor of all supplied IDs (i.e., the classic LCA / NCA).
#
#   2. find_shared_levels()
#      Given a named list of ID vectors (one per site), return all nodes that
#      are ancestral to at least one species in EVERY site. This answers:
#      "at what taxonomic or functional level(s) are all sites represented?"
#
# Both functions rely on data.tree::FindNode() and node$path, matching the
# pattern already used in utl_mg_get_functional_groups().
#
# Usage:
#   source this file after devtools::load_all(), or run sections interactively.

library(devtools)
devtools::load_all()
library(tidyverse)

# ── Build tree ─────────────────────────────────────────────────────────────────
# Swap "vegetation" for any tree_name in marinegeo_metadata$functional_group_lookup

TREE_NAME <- "vegetation"

fg_lookup <- marinegeo_metadata$functional_group_lookup |>
  filter(tree_name == TREE_NAME)

fg_tree <- data.tree::FromDataFrameNetwork(
  fg_lookup,
  check = c("check", "no-warn", "no-check")
)

# Inspect tree (prints node_name and rank if those columns exist in fg_lookup)
print(fg_tree, "node_name")


# ── Helper: get ancestor path for one node ─────────────────────────────────────
# Returns a character vector of node IDs from root to the queried node (inclusive).
# Returns NULL silently if the node is not found in the tree.

.get_node_path <- function(tree, node_id) {
  node <- data.tree::FindNode(tree, node_id)
  if (is.null(node)) return(NULL)
  node$path  # character vector: root → ... → node_id
}


# ── Helper: resolve node metadata from the edge-list ──────────────────────────
# node_name and rank live in the edge-list as attributes of the `from` (parent)
# node, so look them up by matching node_id to the `from` column.

.node_metadata <- function(fg_lookup, node_ids) {
  # Each node_id appears as `from`; grab its display name (and rank if present).
  meta_cols <- intersect(c("from", "node_name", "rank"), colnames(fg_lookup))
  fg_lookup |>
    select(all_of(meta_cols)) |>
    distinct() |>
    filter(from %in% node_ids) |>
    rename(node_id = from)
}


# ── 1. find_nearest_common_ancestor() ─────────────────────────────────────────
#
# Algorithm:
#   • For each ID, retrieve the full ancestor path (root → node) via node$path
#   • Intersect all paths — the intersection is the set of shared ancestors
#   • Keep the node(s) with the greatest depth (level) — that is the NCA
#
# @param tree      data.tree Node object built via FromDataFrameNetwork()
# @param fg_lookup Edge-list data frame used to build `tree` (for metadata lookup)
# @param ids       Character vector of scientific IDs (APHIA: or FUNCTIONAL:)
#
# @return A tibble: node_id, node_name, [rank], level
#         Zero rows if no common ancestor is found or no IDs resolve to nodes.

find_nearest_common_ancestor <- function(tree, fg_lookup, ids) {
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) stop("`ids` must contain at least one non-NA value.")

  paths <- purrr::map(ids, \(id) .get_node_path(tree, id))

  not_found <- ids[purrr::map_lgl(paths, is.null)]
  if (length(not_found) > 0) {
    warning("IDs not found in tree, ignored: ", paste(not_found, collapse = ", "))
  }
  paths <- purrr::compact(paths)

  if (length(paths) == 0) stop("None of the supplied IDs were found in the tree.")

  # Intersection: nodes ancestral to every supplied ID
  common_ids <- purrr::reduce(paths, intersect)

  if (length(common_ids) == 0) {
    message("No common ancestor found.")
    return(tibble(node_id = character(0), node_name = character(0), level = integer(0)))
  }

  # Attach depth (level) from data.tree, then keep only the deepest node(s)
  tibble(node_id = common_ids) |>
    mutate(
      level = purrr::map_int(node_id, \(id) data.tree::FindNode(tree, id)$level)
    ) |>
    filter(level == max(level)) |>    # nearest = deepest
    left_join(.node_metadata(fg_lookup, common_ids), by = "node_id") |>
    arrange(desc(level))
}


# ── 2. find_shared_levels() ────────────────────────────────────────────────────
#
# Answers: "at what taxonomic / functional level is every site represented?"
#
# Algorithm:
#   • For each site, take the UNION of all ancestor paths across its species
#     (a node enters the union if ANY species in that site descends from it)
#   • INTERSECT those unions across all sites
#     (a node survives only if every site has ≥ 1 species under it)
#   • Return ALL nodes in the intersection, ordered deepest-first, so callers
#     can choose the resolution level they need
#
# @param tree      data.tree Node object
# @param fg_lookup Edge-list data frame (for metadata lookup)
# @param site_ids  Named list of character vectors, one per site
#
# @return A tibble: node_id, node_name, [rank], level (ordered deepest first)
#         Zero rows if no node is ancestral to at least one species in all sites.

find_shared_levels <- function(tree, fg_lookup, site_ids) {
  if (!is.list(site_ids) || length(site_ids) == 0) {
    stop("`site_ids` must be a named list of character vectors, one per site.")
  }

  # Per-site ancestor union
  site_ancestor_unions <- purrr::imap(site_ids, function(ids, site_name) {
    ids <- ids[!is.na(ids)]

    paths <- purrr::map(ids, \(id) .get_node_path(tree, id))

    not_found <- ids[purrr::map_lgl(paths, is.null)]
    if (length(not_found) > 0) {
      warning(
        "Site '", site_name, "': IDs not found in tree, ignored: ",
        paste(not_found, collapse = ", ")
      )
    }
    paths <- purrr::compact(paths)

    if (length(paths) == 0) {
      warning("Site '", site_name, "' has no recognized IDs; it will block all shared levels.")
      return(character(0))
    }

    purrr::reduce(paths, union)   # union: present in ≥ 1 species' path
  })

  # Intersection across sites: present in every site's ancestor union
  shared_ids <- purrr::reduce(site_ancestor_unions, intersect)

  if (length(shared_ids) == 0) {
    message("No shared ancestor levels found across all sites.")
    return(tibble(node_id = character(0), node_name = character(0), level = integer(0)))
  }

  tibble(node_id = shared_ids) |>
    mutate(
      level = purrr::map_int(node_id, \(id) data.tree::FindNode(tree, id)$level)
    ) |>
    left_join(.node_metadata(fg_lookup, shared_ids), by = "node_id") |>
    arrange(desc(level))   # deepest (most specific) first
}


# ══════════════════════════════════════════════════════════════════════════════
# Worked examples — run interactively after building fg_tree above
# ══════════════════════════════════════════════════════════════════════════════

# ── Example 1: NCA within a genus ─────────────────────────────────────────────
# Halodule uninervis + Halodule wrightii → expect Halodule (Genus node)

find_nearest_common_ancestor(
  fg_tree, fg_lookup,
  c("APHIA:208924", "APHIA:208925")
)


# ── Example 2: NCA across genera within same family ───────────────────────────
# Halodule sp. + Cymodocea nodosa → expect Cymodoceaceae (Family node)

find_nearest_common_ancestor(
  fg_tree, fg_lookup,
  c("APHIA:208924", "APHIA:145793")
)


# ── Example 3: NCA across functional groups ───────────────────────────────────
# Halodule (Seagrass) + Potamogeton (Non-marine SAV) → expect SAV or Macrophytes

find_nearest_common_ancestor(
  fg_tree, fg_lookup,
  c("APHIA:208924", "APHIA:416212")
)


# ── Example 4: single ID → NCA is the node itself ─────────────────────────────

find_nearest_common_ancestor(
  fg_tree, fg_lookup,
  "APHIA:374715"   # Syringodium filiforme
)


# ── Example 5: shared levels across 3 sites ────────────────────────────────────
# Site A: two Halodule species (Seagrass, Cymodoceaceae)
# Site B: Cymodocea nodosa + Potamogeton perfoliatus (Seagrass + Non-marine SAV)
# Site C: Syringodium filiforme (Seagrass, Cymodoceaceae)
#
# Expected shared levels: at minimum FUNCTIONAL:SAV and FUNCTIONAL:MACROPHYTES
# (both Seagrass and Non-marine SAV are under SAV, all three sites have SAV spp.)

site_list <- list(
  site_a = c("APHIA:208924", "APHIA:208925"),  # Halodule uninervis + wrightii
  site_b = c("APHIA:145793", "APHIA:416212"),  # Cymodocea nodosa + Potamogeton
  site_c = c("APHIA:374715")                   # Syringodium filiforme
)

find_shared_levels(fg_tree, fg_lookup, site_list)


# ── Example 6: two seagrass-only sites → shared levels should include Seagrass ─

find_shared_levels(
  fg_tree, fg_lookup,
  list(
    site_a = c("APHIA:208924", "APHIA:208925"),  # Halodule spp.
    site_c = c("APHIA:374715")                   # Syringodium
  )
)


# ── Example 7: site with no recognized IDs ────────────────────────────────────
# Should warn and return zero rows (unrecognized site blocks all shared levels)

find_shared_levels(
  fg_tree, fg_lookup,
  list(
    site_a  = c("APHIA:208924"),
    site_unk = c("APHIA:9999999")
  )
)
