# ---------------------------------------------------------------------------
# Shared metadata -> QC rule resolution.
#
# Both QC entry points -- qc_run() for whole tables and qc_check_variable() for
# a single field -- need the same per-column rules out of `marinegeo_metadata`.
# This file owns that lookup so the two cannot drift apart, and declares which
# checks can run on a column in isolation.
# ---------------------------------------------------------------------------

# Which checks need the whole table and which work on a single column.
# `qc_check_lookup_values` is column-scoped on purpose: it is already
# table_id-independent, keyed only on the column name.
.qc_check_scopes <- c(
  qc_check_columns = "table",
  qc_check_row_uniqueness = "table",
  qc_check_aggregate = "table",
  qc_check_data_types = "column",
  qc_check_categorical_values = "column",
  qc_check_missing_values = "column",
  qc_check_numeric_ranges = "column",
  qc_check_lookup_values = "column"
)

.qc_table_scoped_checks <- function() {
  names(.qc_check_scopes)[.qc_check_scopes == "table"]
}

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

# Narrow a rule table to the columns its check requires, carrying `condition_id`
# along when the table has one. The checks validate `rules` by required columns
# and ignore extras, so passing the id through costs them nothing -- and it is
# what lets .qc_apply_conditional() group on it. A plain `tbl[, cols]` would
# drop it and silently turn every conditional rule into an unconditional one.
.qc_keep_cols <- function(tbl, cols) {
  keep <- intersect(c(cols, "condition_id"), colnames(tbl))
  tbl[, keep, drop = FALSE]
}

# Split a `|`-delimited metadata list into a character vector. The single
# definition of the delimiter shared by every rule table that encodes a list in
# one cell (condition values, group columns, join columns), so surrounding
# whitespace and empty segments are handled in exactly one place.
.qc_split_list <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(character(0))
  }
  parts <- trimws(strsplit(as.character(x), "|", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}

# Resolve one metadata-backed rule table for a table_id, tolerating a table that
# is absent from metadata entirely or that lacks the expected columns -- the
# normal state for a newly added rule table whose CSVs are not yet in the
# metadata repo, and for an offline run falling back to a bundled copy that
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

# The global entity registries, keyed by the column name they validate. These
# are deliberately not declared in metadata -- they are system-wide.
.qc_lookup_registries <- function() {
  Filter(
    Negate(is.null),
    list(
      partner_code = .mg_get_registry_table("partner_codes")$partner_code,
      site_name = .mg_get_registry_table("site_codes")$site_name,
      site_code = .mg_get_registry_table("site_codes")$site_code,
      scientific_name = .mg_get_registry_table(
        "observation_lookup"
      )$scientific_name
    )
  )
}

# Resolve every rule set a QC run needs for one table_id.
#
# Returns a list with:
#   table_id    - the requested identifier, echoed back
#   structure   - `database_structure` rows (column_name, data_type,
#                 missing_values, uuid_identity, ...)
#   categorical - `categorical_values` rows (column_name, value)
#   ranges      - `numeric_ranges` rows with a non-NA `range_type`
#   conditions  - `qc_conditions` rows (condition_id, column_name, operator,
#                 value): the predicate terms that conditional rules reference
#   aggregates  - `aggregate_rules` rows: grouped-aggregate rules for the table
#   lookups     - named list of global entity registries
.qc_resolve_rules <- function(table_id) {
  tbl_struct <- .qc_filter_by_table(
    .mg_get_registry_table("database_structure"),
    table_id
  )
  tbl_cats <- .qc_filter_by_table(
    .mg_get_registry_table("categorical_values"),
    table_id
  )

  tbl_ranges <- .qc_registry_rules(
    "numeric_ranges",
    c(
      "column_name",
      "max_fail",
      "min_fail",
      "max_warn",
      "min_warn",
      "range_type"
    ),
    table_id
  )
  if (nrow(tbl_ranges) > 0) {
    tbl_ranges <- tbl_ranges[!is.na(tbl_ranges$range_type), , drop = FALSE]
  }

  tbl_conds <- .qc_registry_rules(
    "qc_conditions",
    c("condition_id", "column_name", "operator"),
    table_id
  )

  tbl_aggs <- .qc_registry_rules(
    "aggregate_rules",
    c("group_columns", "value_column", "aggregate_function", "comparison"),
    table_id
  )

  list(
    table_id = table_id,
    structure = tbl_struct,
    categorical = tbl_cats,
    ranges = tbl_ranges,
    conditions = tbl_conds,
    aggregates = tbl_aggs,
    lookups = .qc_lookup_registries()
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
