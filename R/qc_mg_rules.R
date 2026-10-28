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
.qc_registry_tables <- c(
  "database_structure",
  "categorical_values",
  "numeric_ranges",
  "partner_codes",
  "site_codes",
  "observation_lookup"
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

  list(
    table_id = table_id,
    structure = tbl_struct,
    categorical = tbl_cats,
    ranges = tbl_ranges,
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
