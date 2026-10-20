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
  qc_check_point_counts = "table",
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

  # `numeric_ranges` is checked defensively: it may be absent from metadata, and
  # the is.null()/colnames() guards must precede nrow() because nrow(NULL) is
  # itself NULL.
  num_ranges <- .mg_get_registry_table("numeric_ranges")
  range_cols <- c(
    "column_name",
    "max_fail",
    "min_fail",
    "max_warn",
    "min_warn",
    "range_type"
  )
  if (
    !is.null(num_ranges) &&
      is.data.frame(num_ranges) &&
      all(c("table_id", range_cols) %in% colnames(num_ranges)) &&
      nrow(num_ranges) > 0
  ) {
    tbl_ranges <- .qc_filter_by_table(num_ranges, table_id)
    tbl_ranges <- tbl_ranges[!is.na(tbl_ranges$range_type), , drop = FALSE]
  } else {
    tbl_ranges <- data.frame()
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
