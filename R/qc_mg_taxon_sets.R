# ---------------------------------------------------------------------------
# Taxonomy-aware range scopes.
#
# A numeric range rule's scope can name a clade (`Genus:Zostera`) or a functional
# group (`vegetation:Marine Seagrass`) instead of listing species. The reference is
# expanded ONCE to the set of `scientific_id`s it contains, and each row's
# taxon is resolved to an id and tested against that set -- so the species list
# is derived from the registry rather than maintained by hand, and a newly
# recorded species falls inside every scope covering its clade with no CSV
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
# Nothing here is memoised. The cost is already negligible per scope, and a
# cache on a zero-argument function reading `marinegeo_metadata` would go stale
# the moment that object is mocked or the registry refetched.
# ---------------------------------------------------------------------------

# `.qc_norm_rank()` lived here originally; it now sits in R/utl_mg_taxonomy.R,
# shared with the adjacency traversal and the ancestor-label code.

# Registry accessors, guarded the same defensive way the rule resolvers are: a
# table absent from metadata or missing its columns yields NULL rather than an
# error, so an offline run against an in-package copy that predates a column does
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
  fg <- .mg_get_registry_table("nested_labelsets")
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
      "; the ancestry may contain a cycle. Scoped range rules may be incomplete."
    )
  }

  list(node = out_node, anc = out_anc)
}

# Resolve one reference -- `Rank:Name` or a bare LSID -- to a single
# `scientific_id`. Returns NULL with a warning rather than erroring, matching
# how every other piece of bad scope metadata degrades to "rule not
# applied".
.qc_resolve_taxon_node <- function(ref, tl = .qc_taxonomic_lookup()) {
  if (is.null(tl)) {
    return(NULL)
  }
  ref <- trimws(as.character(ref))

  if (startsWith(ref, "urn:lsid:")) {
    if (!ref %in% tl$scientific_id) {
      warning(
        "QC range scope references '",
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
      "QC range scope value '",
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
      "QC range scope found no '",
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
      "; the rule was not applied."
    )
    return(NULL)
  }
  if (length(hit) > 1L) {
    warning(
      "QC range scope '",
      ref,
      "' is ambiguous: ",
      length(hit),
      " rows in `taxonomic_lookup` match. Use the LSID instead. ",
      "The rule was not applied."
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
      "`taxonomic_lookup` is unavailable, so scoped range rules were not applied."
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

# Expand labelset references to the ids beneath them. Two forms, both prefixed
# by the tree:
#
#   tree:Label      one node and its descendants
#   tree:type=<v>   every node in that tree whose `type` is <v>, and their
#                   descendants -- the union, which is what makes
#                   `vegetation:type=primary` mean "seagrass or algae" without
#                   naming either
#
# Walks the edge list by display label -- `from` is the child, `to` the parent
# -- so the QC path needs no `data.tree` work at all.
.qc_fg_sets <- function(refs) {
  fg <- .qc_fg_lookup()
  if (is.null(fg)) {
    warning(
      "`nested_labelsets` is unavailable, so functional group ",
      "scoped range rules were not applied."
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
        "QC functional group scope value '",
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
        ". The rule was not applied."
      )
      next
    }

    tree <- trimws(parts[1])
    selector <- trimws(paste(parts[-1], collapse = ":"))
    sub <- fg[!is.na(fg$tree_name) & fg$tree_name == tree, , drop = FALSE]

    # The seeds the descent starts from: one named node, or every node carrying
    # a given `type`. A `type=` selector matching nothing resolves to no seeds
    # rather than to the whole tree -- same direction as every other failure
    # path here.
    if (startsWith(selector, "type=")) {
      # `substring()`, not `sub()`: `sub` is the tree subset in this scope, and
      # relying on R to skip a non-function binding in call position is a trap
      # for the next reader.
      want <- trimws(substring(selector, nchar("type=") + 1L))
      if (!"type" %in% colnames(sub)) {
        warning(
          "QC labelset scope references type '",
          want,
          "', but `nested_labelsets` has no `type` column; the rule was not ",
          "applied."
        )
        next
      }
      seeds <- unique(as.character(sub$from[
        !is.na(sub$type) & trimws(as.character(sub$type)) == want
      ]))
      if (length(seeds) == 0L) {
        present <- sort(unique(trimws(as.character(sub$type[
          !is.na(sub$type)
        ]))))
        warning(
          "QC labelset scope references type '",
          want,
          "', which no node in tree '",
          tree,
          "' carries",
          if (length(present) > 0) {
            paste0(" (types present: ", paste(present, collapse = ", "), ")")
          } else {
            ""
          },
          "; the rule was not applied."
        )
        next
      }
    } else {
      if (!selector %in% c(sub$from, sub$to)) {
        warning(
          "QC functional group scope references label '",
          selector,
          "', which is not in tree '",
          tree,
          "'; the rule was not applied."
        )
        next
      }
      seeds <- selector
    }

    # Descend by label. The frontier shrinks to nothing on a finite tree; the
    # `setdiff` against what we have already seen also makes a cycle terminate.
    # Seeding with several labels gives the union of their subtrees for free.
    acc <- seeds
    frontier <- seeds
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
# per scope would add noise to every QC run.
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

# The closed set of scope operators a numeric_ranges rule may name. Closed on
# purpose: the rule table is fetched over the network, so dispatching on an
# arbitrary string would be remote code execution.
.qc_scope_types <- c(
  "taxon_within",
  "taxon_not_within",
  "in_functional_group",
  "not_in_functional_group"
)

# The closed set of narrowing operators a `column_lookups` row may name, kept
# beside its numeric_ranges counterpart above so the two cannot drift.
#
# Separate from `.qc_scope_types` because the two tables scope different things:
# a range scope picks ROWS, so it needs negations, while a lookup scope picks a
# permitted SET of values. "Every vegetation name except the seagrasses" has no
# caller, so `not_in_labelset` is deliberately absent rather than reserved.
.qc_lookup_scope_types <- c("in_labelset")

# Split a `|`-delimited scope value into its references. `|` is the only way to
# express OR; everything else about a scope is a single term.
.qc_split_scope <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(character(0))
  }
  parts <- trimws(strsplit(as.character(x), "|", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}

# Which rows a scoped range rule applies to, as a logical mask of length
# nrow(data). A blank scope_type means the rule is unconditional and applies to
# every row.
#
# Every failure path returns all-FALSE with a warning, never all-TRUE. All-FALSE
# means "the rule did not fire"; all-TRUE would mean "it fired on every row",
# which for a Zostera leaf band would validate every species against Zostera
# bounds. That asymmetry is the whole safety property of this function.
#
# The predicate column is always `scientific_name`: each scope_type names its
# own predicate, so there is nothing to declare per rule.
.qc_scope_mask <- function(data, scope_type, scope_value) {
  n <- nrow(data)
  no_match <- rep(FALSE, n)

  blank <- function(x) length(x) != 1L || is.na(x) || !nzchar(trimws(x))
  if (blank(scope_type)) {
    # An unconditional rule. A scope_value without a scope_type is metadata that
    # cannot be acted on either way, so refuse it rather than guess.
    if (!blank(scope_value)) {
      stop(
        "a numeric range rule has scope_value '",
        scope_value,
        "' but no scope_type.",
        call. = FALSE
      )
    }
    return(rep(TRUE, n))
  }

  scope_type <- trimws(as.character(scope_type))
  if (!scope_type %in% .qc_scope_types) {
    warning(
      "Unknown QC range scope_type '",
      scope_type,
      "'; the rule was not applied. Valid values are: ",
      paste(.qc_scope_types, collapse = ", "),
      "."
    )
    return(no_match)
  }

  if (!"scientific_name" %in% colnames(data)) {
    warning(
      "QC range scope '",
      scope_type,
      "' needs a `scientific_name` column, which is not present in the data; ",
      "the rule was not applied."
    )
    return(no_match)
  }

  refs <- .qc_split_scope(scope_value)
  if (length(refs) == 0L) {
    warning(
      "QC range scope '",
      scope_type,
      "' has no scope_value; the rule was not applied."
    )
    return(no_match)
  }

  x <- data[["scientific_name"]]
  set_fn <- if (startsWith(scope_type, "taxon_")) {
    .qc_taxon_sets
  } else {
    .qc_fg_sets
  }
  within <- !grepl("not", scope_type, fixed = TRUE)

  # Expand the reference to an id set once, then test every row against it. A
  # reference that cannot be resolved has already warned inside set_fn().
  sets <- set_fn(refs)
  if (is.null(sets)) {
    return(no_match)
  }
  mask <- .qc_hierarchy_mask(x, sets, within)

  # An NA taxon is not evidence that the scope holds, so it never matches --
  # including under negation, matching how the value checks skip NA rather than
  # flag it.
  mask[is.na(x)] <- FALSE
  mask & !is.na(mask)
}

#' Expand a clade or functional group to the taxa it contains
#'
#' @description
#' Shows which registry taxa a taxonomy-aware range scope will actually match. A
#' `numeric_ranges` rule's `scope_value` can target a clade (`"Genus:Zostera"`)
#' or a functional group
#' (`"vegetation:Marine Seagrass"`) instead of listing species by hand; this is
#' how you check what such a reference resolves to **before** relying on it.
#'
#' @param reference Character vector of one or more references. For
#'   `hierarchy = "taxonomy"`, either `"Rank:Name"` (e.g. `"Genus:Zostera"`,
#'   `"Family:Zosteraceae"`) or a full LSID. For
#'   `hierarchy = "functional_group"`, either `"tree:Label"` (e.g.
#'   `"vegetation:Marine Seagrass"`) or `"tree:type=<value>"` (e.g.
#'   `"vegetation:type=primary"`), which selects every node in that tree
#'   carrying that `type` — today `Seagrass` and `Algae` — and their
#'   descendants. The tree prefix is required either way, because many labels
#'   occur in more than one tree. Several references give the union of their
#'   sets.
#' @param hierarchy Which hierarchy to expand against: `"taxonomy"` (the
#'   default, using `taxonomic_lookup`) or `"functional_group"` (using
#'   `nested_labelsets`). Stated explicitly rather than inferred,
#'   because `"Genus:Zostera"` and `"vegetation:Algae"` are the same shape.
#'
#' @return A data frame with columns `scientific_name` and `scientific_id`, one
#'   row per matching entry in `observation_lookup`, ordered by name. Zero rows
#'   if the reference resolves to nothing — accompanied by a warning saying why.
#'
#' @details
#' A `column_lookups` row can carry the same kind of reference in its
#' `scope_value`, which narrows the names that column accepts; this is the way
#' to see what such a binding will permit before relying on it.
#'
#' The result is derived from the registry, so it reflects whatever
#' `taxonomic_lookup` / `nested_labelsets` is in force. Offline, that is
#' the copy shipped in the package, which may be older than the live one — so a
#' set can be narrower than it would be online. `getOption(
#' "marinegeo.utils.live_registry")` tells you which you are seeing.
#'
#' @seealso [utl_mg_get_registry()] for the `numeric_ranges` schema and the
#'   `scope_type` vocabulary.
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
#'
#' # Every node typed `primary` in the vegetation tree, and their descendants
#' # -- what `scientific_name_seagrass_cover` accepts
#' # utl_mg_expand_taxon_set(
#' #   "vegetation:type=primary",
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
