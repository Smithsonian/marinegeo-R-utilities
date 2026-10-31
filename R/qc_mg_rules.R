# ---------------------------------------------------------------------------
# Shared metadata -> QC rule resolution.
#
# Both QC entry points -- qc_run() for whole tables and qc_check_variable() for
# a single field -- need the same per-column rules out of `marinegeo_metadata`.
# This file owns that lookup so the two cannot drift apart, and declares which
# checks can run on a column in isolation.
#
# Rules are authored against `column_id` -- what a column holds -- not against
# (`table_id`, `column_name`) -- where it sits. Resolution happens here: each
# rule table is joined to this table's `database_structure` rows, and the rule
# frames handed to the checks are still keyed by `column_name`, so the checks
# themselves are unaware of `column_id`.
# ---------------------------------------------------------------------------

# Which checks need the whole table and which work on a single column.
# `qc_check_lookup_values` is table-aware like the rest: its registry bindings
# are resolved from this table's `column_lookups` rows. Column scope still
# works because qc_check_variable() supplies `table_id`.
.qc_check_scopes <- c(
  qc_check_columns = "table",
  qc_check_row_uniqueness = "table",
  qc_check_data_types = "column",
  qc_check_categorical_values = "column",
  qc_check_missing_values = "column",
  qc_check_numeric_ranges = "column",
  qc_check_lookup_values = "column"
)

.qc_table_scoped_checks <- function() {
  names(.qc_check_scopes)[.qc_check_scopes == "table"]
}

# Every registry table a QC run reads. Used to ask
# .mg_stale_registry_tables() which of them fell back to the package's offline
# snapshot, so the result can say which rules are stale instead of reporting a
# clean pass over rules it never loaded.
#
# `taxonomic_lookup` and `nested_labelsets` belong here because they are
# now rule *inputs*: a scoped range resolves its taxa through them, so a stale
# one narrows the matched set and the rule under-fires on a clean-looking pass.
.qc_registry_tables <- c(
  "database_structure",
  "column_ids",
  "numeric_ranges",
  "controlled_vocabularies",
  "column_lookups",
  "partner_codes",
  "site_codes",
  "observation_lookup",
  "taxonomic_lookup",
  "nested_labelsets"
)

# The columns a resolved numeric-range rule frame carries, shared by the
# resolver below and the .qc_keep_cols() call in .qc_dispatch().
#
# .qc_keep_cols() is a whitelist, so any rule column missing from this list is
# stripped before qc_check_numeric_ranges() sees it -- and a stripped
# `scope_type` silently turns a scoped rule unconditional. One shared constant,
# never a second list at the call site.
.qc_range_rule_cols <- c(
  "column_name",
  "column_id",
  "rule_id",
  "scope_type",
  "scope_value",
  "range_type",
  "max_fail",
  "min_fail",
  "max_warn",
  "min_warn",
  "description"
)

# The columns each rule table must carry to be read at all. Short of these the
# table is treated as absent rather than as "no rules".
.qc_range_required_cols <- c(
  "rule_id",
  "scope_type",
  "scope_value",
  "range_type",
  "max_fail",
  "min_fail",
  "max_warn",
  "min_warn"
)

# Filter a registry table to one table_id, tolerating a table that is absent
# from metadata entirely (NULL) or that carries no `table_id` column.
.qc_filter_by_table <- function(tbl, table_id) {
  if (is.null(tbl) || !is.data.frame(tbl) || !"table_id" %in% colnames(tbl)) {
    return(data.frame())
  }
  tbl[!is.na(tbl$table_id) & tbl$table_id == table_id, , drop = FALSE]
}

# Restrict a rule table to rules that target one of `cols`. Used in column
# scope so a check is only reported as run when a rule actually applied.
.qc_filter_by_column <- function(tbl, cols) {
  if (
    is.null(tbl) ||
      !is.data.frame(tbl) ||
      nrow(tbl) == 0L ||
      !"column_name" %in% colnames(tbl)
  ) {
    return(tbl)
  }
  tbl[tbl$column_name %in% cols, , drop = FALSE]
}

# Narrow a rule table to the columns its check requires. The checks validate
# `rules` by required columns and ignore extras, so this only keeps the result
# tidy -- but it is also what guarantees a check never sees a column it does
# not expect.
.qc_keep_cols <- function(tbl, cols) {
  tbl[, intersect(cols, colnames(tbl)), drop = FALSE]
}

# Resolve one metadata-backed rule table for a table_id, tolerating a table that
# is absent from metadata entirely or that lacks the expected columns -- the
# normal state for a newly added rule table whose CSVs are not yet in the
# metadata repo, and for an offline run falling back to an in-package copy that
# predates it.
#
# The is.null()/colnames() guards must precede nrow() because nrow(NULL) is
# itself NULL.
.qc_registry_rules <- function(table, required_cols, table_id) {
  tbl <- .mg_get_registry_table(table)
  if (
    is.null(tbl) ||
      !is.data.frame(tbl) ||
      !all(c("table_id", required_cols) %in% colnames(tbl)) ||
      nrow(tbl) == 0
  ) {
    return(data.frame())
  }
  .qc_filter_by_table(tbl, table_id)
}

# The column_id-keyed counterpart: restrict a rule table to the column_ids a
# table actually defines. Same fail-closed guard shape -- a table without a
# `column_id` column is "absent", not "no rules".
#
# .qc_registry_rules() keeps its own `table_id` guard deliberately: that is what
# makes a table-keyed rule table fail closed here.
.qc_registry_rules_by_column <- function(table, required_cols, column_ids) {
  tbl <- .mg_get_registry_table(table)
  if (
    is.null(tbl) ||
      !is.data.frame(tbl) ||
      !all(c("column_id", required_cols) %in% colnames(tbl)) ||
      nrow(tbl) == 0
  ) {
    return(data.frame())
  }
  tbl[!is.na(tbl$column_id) & tbl$column_id %in% column_ids, , drop = FALSE]
}

# Attach `column_name` to a column_id-keyed rule frame.
#
# This join is the expansion: one rule x N columns in this table carrying that
# column_id -> N rule rows. inner_join() rather than merge(), which reorders;
# the explicit order() then makes issue order deterministic. Issue order within
# a result differs from the table-keyed era, so comparisons must be on sorted
# sets, not row positions.
.qc_bind_column_names <- function(rules, map, order_by = character(0)) {
  if (!is.data.frame(rules) || nrow(rules) == 0 || nrow(map) == 0) {
    return(data.frame())
  }
  out <- dplyr::inner_join(rules, map, by = "column_id")
  if (nrow(out) == 0) {
    return(out)
  }
  keys <- c("column_name", intersect(order_by, colnames(out)))
  out[
    do.call(order, unname(as.list(out[, keys, drop = FALSE]))),
    ,
    drop = FALSE
  ]
}

# The columns a resolved lookup binding carries. As with the range rules, a
# column missing from this whitelist is stripped before it is read, and a
# stripped `scope_type` silently widens a narrowed binding back to the whole
# registry.
.qc_lookup_rule_cols <- c(
  "registry_table",
  "registry_column",
  "scope_type",
  "scope_value"
)

# The permitted subset of one registry column, for a binding that carries a
# labelset scope. Returns NULL when the scope cannot be resolved, which the
# caller reads as "no narrowing".
#
# Narrowing is an intersection on `scientific_id`: the labelset places ids, the
# registry maps ids to the values the data actually holds. A registry without
# that column therefore cannot be narrowed at all -- the metadata repo's
# validator rejects such a binding, so reaching this branch means the rules in
# force are older than the check.
.qc_lookup_scope_names <- function(registry_table, registry_column, refs) {
  reg <- .mg_get_registry_table(registry_table)
  if (
    is.null(reg) ||
      !is.data.frame(reg) ||
      !all(c("scientific_id", registry_column) %in% colnames(reg))
  ) {
    warning(
      "A column_lookups scope narrows registry '",
      registry_table,
      "', which has no `scientific_id` column to intersect on; the column was ",
      "validated against the whole registry instead."
    )
    return(NULL)
  }

  sets <- .qc_fg_sets(refs)
  if (is.null(sets) || length(sets$set) == 0L) {
    warning(
      "A column_lookups scope ('",
      paste(refs, collapse = " | "),
      "') resolved to no taxa; the column was validated against the whole ",
      "registry instead."
    )
    return(NULL)
  }

  keep <- !is.na(reg$scientific_id) & reg$scientific_id %in% sets$set
  unique(as.character(reg[[registry_column]][keep]))
}

# The entity registries this table's columns are bound to, and the narrower sets
# some of them are scoped to -- both keyed by the column name they validate.
#
# Bindings are declared per `column_id` in `column_lookups`, not system-wide: a
# column simply absent from that table gets no registry QC, which is how a
# partner-coded table opts out of the MarineGEO registries.
#
# Returns `list(lookups = <named list>, scopes = <named list>)`. `scopes` holds
# an entry only for a binding whose labelset scope resolved, so an unresolvable
# scope leaves the column checked against the whole registry rather than against
# nothing. That is the permissive direction, chosen to match how every other
# piece of unusable rule metadata degrades; it is also why the metadata repo
# fails the build on a scope that does not resolve there.
.qc_lookup_registries <- function(map) {
  lk <- .qc_bind_column_names(
    .qc_registry_rules_by_column(
      "column_lookups",
      .qc_lookup_rule_cols,
      map$column_id
    ),
    map
  )
  if (nrow(lk) == 0) {
    return(list(lookups = list(), scopes = list()))
  }

  blank <- function(x) {
    length(x) != 1L || is.na(x) || !nzchar(trimws(x))
  }

  lookups <- stats::setNames(
    lapply(
      seq_len(nrow(lk)),
      function(i) {
        .mg_get_registry_table(lk$registry_table[i])[[lk$registry_column[i]]]
      }
    ),
    lk$column_name
  )

  scopes <- stats::setNames(
    lapply(
      seq_len(nrow(lk)),
      function(i) {
        st <- lk$scope_type[i]
        sv <- lk$scope_value[i]
        if (blank(st)) {
          # A scope_value with no scope_type is metadata nothing can act on.
          if (!blank(sv)) {
            warning(
              "column_lookups binding for '",
              lk$column_name[i],
              "' has scope_value '",
              trimws(sv),
              "' but no scope_type; it was ignored."
            )
          }
          return(NULL)
        }
        st <- trimws(as.character(st))
        if (!st %in% .qc_lookup_scope_types) {
          warning(
            "Unknown column_lookups scope_type '",
            st,
            "'; the column was validated against the whole registry. Valid ",
            "values are: ",
            paste(.qc_lookup_scope_types, collapse = ", "),
            "."
          )
          return(NULL)
        }
        refs <- .qc_split_scope(sv)
        if (length(refs) == 0L) {
          warning(
            "column_lookups scope_type '",
            st,
            "' for '",
            lk$column_name[i],
            "' has no scope_value; the column was validated against the whole ",
            "registry."
          )
          return(NULL)
        }
        .qc_lookup_scope_names(
          lk$registry_table[i],
          lk$registry_column[i],
          refs
        )
      }
    ),
    lk$column_name
  )

  list(
    lookups = Filter(Negate(is.null), lookups),
    scopes = Filter(Negate(is.null), scopes)
  )
}

# Resolve every rule set a QC run needs for one table_id.
#
# Returns a list with:
#   table_id    - the requested identifier, echoed back
#   structure   - `database_structure` rows (column_name, column_id, data_type,
#                 missing_values, uuid_identity, ...)
#   column_ids  - `column_ids` rows (column_id, label, unit) for the ids this
#                 table uses. Unused by the checks; carried for exporters.
#   categorical - `controlled_vocabularies` rows, keyed back to column_name
#   ranges      - `numeric_ranges` rows with a non-NA `range_type`, keyed back
#                 to column_name
#   lookups     - named list of the registries this table's columns bind to
#   lookup_scopes - named list of the narrower permitted sets, for those
#                 bindings that carry a labelset scope. A column absent from it
#                 is checked against the whole of its registry.
.qc_resolve_rules <- function(table_id) {
  tbl_struct <- .qc_filter_by_table(
    .mg_get_registry_table("database_structure"),
    table_id
  )

  # The column_id -> column_name map for this table. Everything below is a join
  # against it, so a structure table without column_id resolves to no rules.
  map <- if ("column_id" %in% colnames(tbl_struct)) {
    m <- tbl_struct[
      !is.na(tbl_struct$column_id),
      c("column_name", "column_id"),
      drop = FALSE
    ]
    m[!duplicated(m), , drop = FALSE]
  } else {
    data.frame(
      column_name = character(0),
      column_id = character(0),
      stringsAsFactors = FALSE
    )
  }
  col_ids <- unique(map$column_id)

  tbl_ids <- .qc_registry_rules_by_column(
    "column_ids",
    c("label", "unit"),
    col_ids
  )

  tbl_cats <- .qc_bind_column_names(
    .qc_registry_rules_by_column("controlled_vocabularies", "value", col_ids),
    map,
    order_by = "value"
  )

  tbl_ranges <- .qc_registry_rules_by_column(
    "numeric_ranges",
    .qc_range_required_cols,
    col_ids
  )
  if (nrow(tbl_ranges) > 0) {
    tbl_ranges <- tbl_ranges[!is.na(tbl_ranges$range_type), , drop = FALSE]
  }
  tbl_ranges <- .qc_bind_column_names(tbl_ranges, map, order_by = "rule_id")

  tbl_lookups <- .qc_lookup_registries(map)

  list(
    table_id = table_id,
    structure = tbl_struct,
    column_ids = tbl_ids,
    categorical = tbl_cats,
    ranges = tbl_ranges,
    lookups = tbl_lookups$lookups,
    lookup_scopes = tbl_lookups$scopes
  )
}

# Every column name defined for a table_id across the three metadata tables.
.qc_known_columns <- function(table_id) {
  rules <- .qc_resolve_rules(table_id)
  cols <- c(
    rules$structure$column_name,
    rules$categorical$column_name,
    rules$ranges$column_name
  )
  unique(cols[!is.na(cols)])
}
