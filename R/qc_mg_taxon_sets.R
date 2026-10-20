# ---------------------------------------------------------------------------
# Taxonomy-aware condition targets.
#
# A condition can name a clade (`Genus:Zostera`) or a functional group
# (`vegetation:Marine Seagrass`) instead of listing species. The reference is
# expanded ONCE to the set of `scientific_id`s it contains, and each row's
# taxon is resolved to an id and tested against that set -- so the species list
# is derived from the registry rather than maintained by hand, and a newly
# recorded species falls inside every condition covering its clade with no CSV
# edit.
#
# Expansion, not per-row traversal, is what makes this affordable. The existing
# walker `.get_parent_rank()` (R/utl_mg_traverse_adjacency_tables.R) runs one
# `dplyr::filter()` over the whole table per ancestry level, per node, and
# `utl_mg_get_taxonomic_classifications()` calls it once per ROW with no
# `unique()` first -- about 10 ms per id, so ~100 s on a 10,000-row table. The
# closure below does the whole 3,226-node table in ~36 ms, independent of row
# count, which is why the QC path does not reuse those two functions.
#
# Nothing here is memoised. The cost is already negligible per condition, and a
# cache on a zero-argument function reading `marinegeo_metadata` would go stale
# the moment that object is mocked or the registry refetched.
# ---------------------------------------------------------------------------

# WoRMS spells a few ranks with a parenthetical synonym. Normalising both sides
# lets a condition say `Phylum:Tracheophyta` and match a stored
# "Phylum (Division)" row. NA-safe, and vectorised so it can normalise a whole
# column; shared with the functional-group and classification code, which each
# applied this mapping inline.
.qc_norm_rank <- function(x) {
  x <- as.character(x)
  repl <- c(
    "Phylum (Division)" = "Phylum",
    "Subphylum (Subdivision)" = "Subphylum"
  )
  hit <- !is.na(x) & x %in% names(repl)
  x[hit] <- unname(repl[x[hit]])
  x
}

# Registry accessors, guarded the same defensive way the rule resolvers are: a
# table absent from metadata or missing its columns yields NULL rather than an
# error, so an offline run against a bundled copy that predates a column does
# not abort the QC run.
.qc_taxonomic_lookup <- function() {
  tl <- .mg_get_registry_table("taxonomic_lookup")
  required <- c("scientific_id", "name", "rank", "parent_id")
  if (
    is.null(tl) ||
      !is.data.frame(tl) ||
      !all(required %in% colnames(tl)) ||
      nrow(tl) == 0
  ) {
    return(NULL)
  }
  tl
}

.qc_fg_lookup <- function() {
  fg <- .mg_get_registry_table("functional_group_lookup")
  required <- c("from", "to", "scientific_id", "tree_name")
  if (
    is.null(fg) ||
      !is.data.frame(fg) ||
      !all(required %in% colnames(fg)) ||
      nrow(fg) == 0
  ) {
    return(NULL)
  }
  fg
}

# Every (node, ancestor) pair in `taxonomic_lookup`, by iterated vectorised
# parent lookup. Each node is its own ancestor, so `Species:Zostera marina`
# matches that species itself.
#
# `max_depth` is a guard, not a limit: the real tree is 17 deep, and the cap
# means a `parent_id` cycle introduced upstream terminates with a warning
# instead of hanging a QC run.
.qc_taxon_closure <- function(tl = .qc_taxonomic_lookup(), max_depth = 50L) {
  if (is.null(tl)) {
    return(NULL)
  }

  parent <- stats::setNames(
    as.character(tl$parent_id),
    as.character(tl$scientific_id)
  )
  node <- as.character(tl$scientific_id)

  out_node <- node
  out_anc <- node
  cur_node <- node
  cur_anc <- node
  exhausted <- FALSE

  for (i in seq_len(max_depth)) {
    nxt <- unname(parent[cur_anc])
    keep <- !is.na(nxt)
    if (!any(keep)) {
      exhausted <- TRUE
      break
    }
    cur_node <- cur_node[keep]
    cur_anc <- nxt[keep]
    out_node <- c(out_node, cur_node)
    out_anc <- c(out_anc, cur_anc)
  }

  if (!exhausted) {
    warning(
      "Walking `taxonomic_lookup` hit the depth limit of ",
      max_depth,
      "; the ancestry may contain a cycle. Taxon conditions may be incomplete."
    )
  }

  list(node = out_node, anc = out_anc)
}

# Resolve one reference -- `Rank:Name` or a bare LSID -- to a single
# `scientific_id`. Returns NULL with a warning rather than erroring, matching
# how every other piece of bad condition metadata degrades to "rule not
# applied".
.qc_resolve_taxon_node <- function(ref, tl = .qc_taxonomic_lookup()) {
  if (is.null(tl)) {
    return(NULL)
  }
  ref <- trimws(as.character(ref))

  if (startsWith(ref, "urn:lsid:")) {
    if (!ref %in% tl$scientific_id) {
      warning(
        "QC taxon condition references '",
        ref,
        "', which is not in `taxonomic_lookup`; it was not applied."
      )
      return(NULL)
    }
    return(ref)
  }

  parts <- strsplit(ref, ":", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    warning(
      "QC taxon condition value '",
      ref,
      "' is not understood. Expected 'Rank:Name' (e.g. 'Genus:Zostera') or a ",
      "full LSID. It was not applied."
    )
    return(NULL)
  }

  rank <- .qc_norm_rank(trimws(parts[1]))
  taxon <- trimws(paste(parts[-1], collapse = ":"))

  hit <- which(
    .qc_norm_rank(tl$rank) == rank &
      !is.na(tl$name) &
      tl$name == taxon
  )

  if (length(hit) == 0L) {
    # Separate the two likely typos, since the fix differs.
    at_other_rank <- unique(.qc_norm_rank(tl$rank[
      !is.na(tl$name) & tl$name == taxon
    ]))
    warning(
      "QC taxon condition found no '",
      rank,
      "' named '",
      taxon,
      "' in `taxonomic_lookup`",
      if (length(at_other_rank) > 0) {
        paste0(
          " (that name exists at rank: ",
          paste(at_other_rank, collapse = ", "),
          ")"
        )
      } else {
        ""
      },
      "; the condition was not applied."
    )
    return(NULL)
  }
  if (length(hit) > 1L) {
    warning(
      "QC taxon condition '",
      ref,
      "' is ambiguous: ",
      length(hit),
      " rows in `taxonomic_lookup` match. Use the LSID instead. ",
      "The condition was not applied."
    )
    return(NULL)
  }

  as.character(tl$scientific_id[hit])
}

# Expand taxon references to the ids they contain, plus the universe of ids the
# hierarchy can place at all.
#
# The universe matters: a row whose taxon resolves to a `FUNCTIONAL:` or
# `UNDEFINED:` id (114 of the 1,696 registry entries) cannot be placed
# taxonomically, so it matches neither `taxon_within` nor `taxon_not_within`
# rather than being swept into the negation by accident.
.qc_taxon_sets <- function(refs) {
  tl <- .qc_taxonomic_lookup()
  if (is.null(tl)) {
    warning(
      "`taxonomic_lookup` is unavailable, so taxon conditions were not applied."
    )
    return(NULL)
  }

  nodes <- unlist(
    lapply(refs, .qc_resolve_taxon_node, tl = tl),
    use.names = FALSE
  )
  if (length(nodes) == 0L) {
    return(NULL)
  }

  closure <- .qc_taxon_closure(tl)
  if (is.null(closure)) {
    return(NULL)
  }

  list(
    set = unique(closure$node[closure$anc %in% nodes]),
    universe = unique(as.character(tl$scientific_id))
  )
}

# Expand functional-group references (`tree:Label`) to the ids beneath them.
#
# Walks the edge list by display label -- `from` is the child, `to` the parent
# -- so the QC path needs no `data.tree` work at all.
.qc_fg_sets <- function(refs) {
  fg <- .qc_fg_lookup()
  if (is.null(fg)) {
    warning(
      "`functional_group_lookup` is unavailable, so functional group ",
      "conditions were not applied."
    )
    return(NULL)
  }

  trees <- unique(as.character(fg$tree_name[!is.na(fg$tree_name)]))
  ids <- character(0)
  universe <- character(0)
  resolved_any <- FALSE

  for (ref in refs) {
    raw <- trimws(as.character(ref))
    parts <- strsplit(raw, ":", fixed = TRUE)[[1]]

    # A tree prefix is required, not optional: 540 of the 1,579 labels occur in
    # more than one tree (`Algae` is in both oyster_composition and
    # vegetation), so a bare label is ambiguous for a third of the vocabulary.
    if (length(parts) < 2L || !trimws(parts[1]) %in% trees) {
      in_trees <- sort(unique(as.character(
        fg$tree_name[
          (!is.na(fg$from) & fg$from == raw) | (!is.na(fg$to) & fg$to == raw)
        ]
      )))
      warning(
        "QC functional group condition value '",
        raw,
        "' needs a 'tree:Label' prefix",
        if (length(in_trees) > 0) {
          paste0(
            " -- that label occurs in tree(s): ",
            paste(in_trees, collapse = ", ")
          )
        } else {
          paste0(". Known trees: ", paste(sort(trees), collapse = ", "))
        },
        ". The condition was not applied."
      )
      next
    }

    tree <- trimws(parts[1])
    label <- trimws(paste(parts[-1], collapse = ":"))
    sub <- fg[!is.na(fg$tree_name) & fg$tree_name == tree, , drop = FALSE]

    if (!label %in% c(sub$from, sub$to)) {
      warning(
        "QC functional group condition references label '",
        label,
        "', which is not in tree '",
        tree,
        "'; the condition was not applied."
      )
      next
    }

    # Descend by label. The frontier shrinks to nothing on a finite tree; the
    # `setdiff` against what we have already seen also makes a cycle terminate.
    acc <- label
    frontier <- label
    repeat {
      kids <- unique(sub$from[!is.na(sub$to) & sub$to %in% frontier])
      kids <- setdiff(kids, acc)
      if (length(kids) == 0L) {
        break
      }
      acc <- c(acc, kids)
      frontier <- kids
    }

    ids <- c(ids, sub$scientific_id[!is.na(sub$from) & sub$from %in% acc])
    universe <- c(universe, sub$scientific_id)
    resolved_any <- TRUE
  }

  if (!resolved_any) {
    return(NULL)
  }

  list(
    set = unique(ids[!is.na(ids)]),
    universe = unique(universe[!is.na(universe)])
  )
}

# Resolve a column of scientific names to `scientific_id`s.
#
# `utl_mg_get_scientific_id()` is reused here rather than joining
# `observation_lookup` directly, because it also strips rank abbreviations
# ("Halodule sp." -> "Halodule"), so an abbreviated entry still lands inside
# its genus. It is called on the distinct values only -- it rebuilds its lookup
# vector on every call.
#
# Its unmatched-name warning is muffled: an unregistered name is already
# reported, properly and once, by `qc_check_lookup_values()`, and repeating it
# per condition would add noise to every QC run.
.qc_resolve_column_ids <- function(values) {
  chr <- as.character(values)
  uniq <- unique(chr)
  if (length(uniq) == 0L) {
    return(character(0))
  }

  ids <- withCallingHandlers(
    utl_mg_get_scientific_id(uniq),
    warning = function(w) invokeRestart("muffleWarning")
  )
  unname(stats::setNames(as.character(ids), uniq)[chr])
}

# Build the row mask for one taxonomy-aware operator.
.qc_hierarchy_mask <- function(values, sets, within) {
  ids <- .qc_resolve_column_ids(values)
  placeable <- !is.na(ids) & ids %in% sets$universe
  if (within) {
    placeable & ids %in% sets$set
  } else {
    placeable & !(ids %in% sets$set)
  }
}

#' Expand a clade or functional group to the taxa it contains
#'
#' @description
#' Shows which registry taxa a taxonomy-aware QC condition will actually match.
#' `qc_conditions` can target a clade (`"Genus:Zostera"`) or a functional group
#' (`"vegetation:Marine Seagrass"`) instead of listing species by hand; this is
#' how you check what such a reference resolves to **before** relying on it.
#'
#' @param reference Character vector of one or more references. For
#'   `hierarchy = "taxonomy"`, either `"Rank:Name"` (e.g. `"Genus:Zostera"`,
#'   `"Family:Zosteraceae"`) or a full LSID. For
#'   `hierarchy = "functional_group"`, `"tree:Label"` (e.g.
#'   `"vegetation:Marine Seagrass"`) — the tree prefix is required, because
#'   many labels occur in more than one tree. Several references give the union
#'   of their sets.
#' @param hierarchy Which hierarchy to expand against: `"taxonomy"` (the
#'   default, using `taxonomic_lookup`) or `"functional_group"` (using
#'   `functional_group_lookup`). Stated explicitly rather than inferred,
#'   because `"Genus:Zostera"` and `"vegetation:Algae"` are the same shape.
#'
#' @return A data frame with columns `scientific_name` and `scientific_id`, one
#'   row per matching entry in `observation_lookup`, ordered by name. Zero rows
#'   if the reference resolves to nothing — accompanied by a warning saying why.
#'
#' @details
#' The result is derived from the registry, so it reflects whatever
#' `taxonomic_lookup` / `functional_group_lookup` is in force. Offline, that is
#' the copy bundled in the package, which may be older than the live one — so a
#' set can be narrower than it would be online. `getOption(
#' "marinegeo.utils.live_registry")` tells you which you are seeing.
#'
#' @seealso [utl_mg_get_registry()] for the `qc_conditions` schema and the full
#'   operator vocabulary.
#'
#' @export
#'
#' @examples
#' # Every registry taxon in genus Zostera -- including Heterozostera
#' # nigricaulis, which a hand-written "Zostera *" list would miss.
#' # utl_mg_expand_taxon_set("Genus:Zostera")
#'
#' # By identifier instead of name
#' # utl_mg_expand_taxon_set("urn:lsid:marinespecies.org:taxname:144233")
#'
#' # A functional group, which needs its tree
#' # utl_mg_expand_taxon_set(
#' #   "vegetation:Marine Seagrass",
#' #   hierarchy = "functional_group"
#' # )
utl_mg_expand_taxon_set <- function(
  reference,
  hierarchy = c("taxonomy", "functional_group")
) {
  hierarchy <- match.arg(hierarchy)

  if (
    !is.character(reference) || length(reference) == 0L || any(is.na(reference))
  ) {
    stop("`reference` must be a non-empty character vector with no NA values.")
  }

  sets <- if (identical(hierarchy, "taxonomy")) {
    .qc_taxon_sets(reference)
  } else {
    .qc_fg_sets(reference)
  }

  empty <- data.frame(
    scientific_name = character(0),
    scientific_id = character(0),
    stringsAsFactors = FALSE
  )

  obs <- .mg_get_registry_table("observation_lookup")
  if (
    is.null(sets) ||
      length(sets$set) == 0L ||
      is.null(obs) ||
      !all(c("scientific_name", "scientific_id") %in% colnames(obs))
  ) {
    return(empty)
  }

  keep <- !is.na(obs$scientific_id) & obs$scientific_id %in% sets$set
  out <- data.frame(
    scientific_name = as.character(obs$scientific_name[keep]),
    scientific_id = as.character(obs$scientific_id[keep]),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$scientific_name), , drop = FALSE]
  rownames(out) <- NULL
  out
}
