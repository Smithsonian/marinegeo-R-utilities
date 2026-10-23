#' Access MarineGEO global entity registry tables
#'
#' @description
#' Returns a registry table from the internal `marinegeo_metadata` object,
#' optionally filtered to a subset of rows. All data frame tables stored in
#' `marinegeo_metadata` are supported.
#'
#' @param table Character scalar. The registry table to retrieve. Must be one
#'   of `"partner_codes"`, `"site_codes"`, `"observation_lookup"`,
#'   `"taxonomic_lookup"`, `"functional_group_lookup"`, `"data_index"`,
#'   `"database_structure"`, `"categorical_values"`, `"numeric_ranges"`,
#'   `"qc_conditions"`, `"aggregate_rules"`, or `"cross_table_rules"`.
#'
#'   Wide-form taxonomic classifications are no longer a registry table; use
#'   [utl_mg_get_taxonomic_classifications()] instead.
#' @param ... Named filter arguments. Each name must match a column in the
#'   requested table, and each value is matched with `%in%`, so character
#'   vectors can be supplied to match multiple values (e.g.,
#'   `partner_code = c("USA-MDA", "BLZ-CBC")`). Multiple filter arguments are
#'   applied with AND logic.
#'
#' @return A data frame (tibble) containing the requested table, filtered to
#'   rows that satisfy all supplied filter conditions. Returns a 0-row data
#'   frame with a `message()` if no rows match.
#'
#' @details
#' ## Column reference by table
#'
#' **`partner_codes`**
#' - `partner_code` — Unique partner identifier (e.g., `"USA-MDA"`).
#' - `name` — Human-readable partner name.
#' - `institution` — Affiliated institution.
#' - `country` — Country of the partner.
#' - `type` — Partner type: `"observatory"` or `"project"`.
#'
#' **`site_codes`**
#' - `partner_code` — Links to `partner_codes`.
#' - `site_code` — Machine-readable unique site identifier (e.g., `"BIS-001"`).
#' - `site_name` — Human-readable site name.
#' - `habitat` — Dominant habitat type (e.g., `"seagrass"`, `"coral reef"`).
#' - `latitude` — Decimal latitude (may be `NA`).
#' - `longitude` — Decimal longitude (may be `NA`).
#'
#' **`observation_lookup`**
#' - `scientific_name` — Species or functional group name as entered in
#'   MarineGEO datasets.
#' - `scientific_id` — Unique identifier: an Aphia LSID (e.g.,
#'   `"urn:lsid:marinespecies.org:taxname:495077"`), a functional group ID
#'   (e.g., `"FUNCTIONAL:SEAGRASS"`), or an `"UNDEFINED:"` placeholder.
#'
#' **`taxonomic_lookup`**
#' - `id` — Raw adjacency-table key.
#' - `scientific_id` — Unique identifier for this node (e.g.,
#'   `"urn:lsid:marinespecies.org:taxname:374534"`).
#' - `parent_id` — `scientific_id` of the parent taxonomic node (`NA` at root).
#' - `rank` — Taxonomic rank label (e.g., `"Species"`, `"Genus"`).
#' - `name` — Taxon name at this rank.
#'
#' **`functional_group_lookup`**
#'
#' An edge list holding several independent trees, one per `tree_name`. Each
#' row describes a single child node and the parent it hangs from, so a tree's
#' root has no row of its own and appears only in the `to` column.
#' - `from` — Display label of the child node (e.g., `"Marine Seagrass"`).
#' - `to` — Display label of that node's parent.
#' - `scientific_id` — Identifier of the child node: an Aphia LSID (e.g.,
#'   `"urn:lsid:marinespecies.org:taxname:143770"`) for taxonomic nodes, or a
#'   synthetic functional group ID (e.g., `"FUNCTIONAL:SEAGRASS"`).
#' - `type` — `"primary"` on curated group-level nodes, otherwise `NA`. Flags
#'   group nodes that carry a real Aphia ID rather than a `FUNCTIONAL:` one.
#' - `code` — Short protocol code for the node, where one exists (currently
#'   only in the `fouling` tree), otherwise `NA`.
#' - `rank` — Taxonomic rank of the node (e.g., `"Family"`, `"Genus"`).
#'   `NA` on `FUNCTIONAL:` group nodes.
#' - `definition` — Prose definition of the group, where one exists.
#' - `tree_name` — Which tree this edge belongs to (e.g., `"vegetation"`,
#'   `"fouling"`, `"oyster_density"`, `"oyster_composition"`).
#'
#' **`data_index`**
#' - `table_id` — Versioned table identifier used across MarineGEO metadata
#'   and QC functions (e.g., `"sav_cover_v1"`).
#' - `protocol` — Monitoring or experiment program (e.g., `"seagrass"`).
#' - `table_name` — Human-readable label for the table.
#'   Additional columns may be present (e.g., data level, storage location).
#'
#' **`database_structure`**
#' - `protocol` — Monitoring or experiment program.
#' - `table_id` — Links to `data_index`.
#' - `column_name` — Expected column name in the data table.
#' - `data_type` — SQL-style type name (e.g., `"STRING"`, `"DOUBLE"`, `"DATE"`).
#'
#' **`categorical_values`**
#' - `table_id` — Links to `data_index` and `database_structure`.
#' - `column_name` — Column whose values are restricted.
#' - `value` — A permitted value for that column.
#'
#' **`numeric_ranges`**
#' - `table_id` — Links to `data_index` and `database_structure`.
#' - `column_name` — Column whose values are range-checked.
#' - `min_fail` — Lower bound that triggers a fail (may be `NA`).
#' - `max_fail` — Upper bound that triggers a fail (may be `NA`).
#' - `min_warn` — Lower bound that triggers a warning (may be `NA`).
#' - `max_warn` — Upper bound that triggers a warning (may be `NA`).
#' - `range_type` — `"inclusive"` or `"exclusive"` boundary evaluation; `NA`
#'   rows are skipped by QC functions.
#' - `condition_id` — Links to `qc_conditions`. `NA` (the usual case) means the
#'   range applies to every row. A non-`NA` value restricts it to the rows
#'   matching that condition, so a column can carry both a loose unconditional
#'   bound and a tighter conditional one.
#'
#' **`qc_conditions`**
#'
#' Predicate terms that conditional rules in `numeric_ranges` and
#' `categorical_values` reference by `condition_id`. One row is one term;
#' several rows sharing a `condition_id` are combined with AND.
#' - `condition_id` — Identifier referenced from the rule tables.
#' - `table_id` — Links to `data_index`; the table the condition is evaluated
#'   against. Scoping per table keeps ids from colliding across protocols.
#' - `column_name` — The column the predicate tests.
#' - `operator` — One of `"in"`, `"not_in"`, `"=="`, `"!="`, `">"`, `">="`,
#'   `"<"`, `"<="`, `"is_na"`, `"not_na"`, `"matches"` (regex), or one of the
#'   four taxonomy-aware operators below. The set is closed; an unrecognized
#'   operator warns and the rule is not applied.
#' - `value` — Right-hand side. For `"in"`/`"not_in"`, a `|`-delimited list.
#'   Empty for `"is_na"`/`"not_na"`.
#'
#' \subsection{Taxonomy-aware operators}{
#'
#' These let a condition name a clade or a functional group instead of listing
#' species, so the set is derived from `taxonomic_lookup` /
#' `functional_group_lookup` and a newly recorded species falls inside every
#' condition covering its clade with no CSV edit. `column_name` is the column
#' holding scientific names (normally `scientific_name`).
#' \itemize{
#'   \item `taxon_within` / `taxon_not_within` — `value` is `"Rank:Name"`
#'     (e.g. `"Genus:Zostera"`, `"Family:Zosteraceae"`) or a full LSID. True
#'     when the row's taxon is at or below that node of `taxonomic_lookup`; a
#'     node counts as its own ancestor, so `"Species:Zostera marina"` matches
#'     that species.
#'   \item `in_functional_group` / `not_in_functional_group` — `value` is
#'     `"tree:Label"` (e.g. `"vegetation:Marine Seagrass"`). The tree prefix is
#'     **required**: many labels occur in more than one tree, so a bare label
#'     warns and the rule is not applied.
#' }
#' Several references may be `|`-delimited, giving the union of their sets —
#' which is how an OR is expressed, since two rows sharing a `condition_id` are
#' combined with AND.
#'
#' A row whose taxon the hierarchy cannot place — an unregistered name, or one
#' mapping to a `FUNCTIONAL:`/`UNDEFINED:` id with no taxonomic chain — matches
#' **neither** the positive nor the negative form, so a conditional rule never
#' fires on a row that could not be classified. Unregistered names are reported
#' separately by [qc_check_lookup_values()].
#'
#' Use [utl_mg_expand_taxon_set()] to see exactly which taxa a reference
#' resolves to before relying on it.
#' }
#' - `description` — Prose, surfaced in the `detail` column of [qc_issues].
#'
#' **`aggregate_rules`**
#'
#' Rules about a *group* of rows rather than a single value, evaluated by
#' [qc_check_aggregate()]. One row is one rule.
#' - `rule_id` — Identifier, reported in the `rule_id` column of [qc_issues].
#' - `table_id` — Links to `data_index`.
#' - `group_columns` — `|`-delimited grouping key, e.g.
#'   `"site_name|transect|quadrat"`.
#' - `value_column` — The column aggregated.
#' - `aggregate_function` — `"sum"`, `"mean"`, `"min"`, `"max"`, `"n"` or
#'   `"n_distinct"`.
#' - `comparison` — `"=="`, `"!="`, `">"`, `">="`, `"<"` or `"<="`, read as
#'   "the aggregate must be _ the expected value".
#' - `target_column` — Column holding the expected value, assumed constant
#'   within a group. Mutually exclusive with `target_value`.
#' - `target_value` — Literal expected value. Mutually exclusive with
#'   `target_column`.
#' - `tolerance` — Numeric slack for the comparison; `NA` means exact.
#' - `condition_id` — Optional link to `qc_conditions`, applied as a row filter
#'   *before* aggregating.
#' - `severity` — `"fail"` or `"warn"`.
#' - `description` — Prose.
#'
#' **`cross_table_rules`**
#'
#' Referential-integrity relations between two tables, evaluated by
#' [qc_check_cross_table()] and dispatched by [qc_run_dataset()]. One row is one
#' relation. Note this table is keyed on `left_table_id`/`right_table_id`
#' rather than a single `table_id`, since a relation belongs to the pair.
#' - `rule_id` — Identifier, reported in the `rule_id` column of [qc_issues].
#' - `relation_type` — `"subset"` (every left key must exist in the right table)
#'   or `"equal_set"` (the key sets must match both ways).
#' - `left_table_id`, `right_table_id` — The two tables, linking to
#'   `data_index`.
#' - `group_columns` — `|`-delimited columns shared by both tables that scope
#'   the comparison, e.g. `"site_name|survey_date|transect"`.
#' - `left_columns`, `right_columns` — `|`-delimited compared columns, paired
#'   positionally.
#' - `left_condition_id`, `right_condition_id` — Optional links to
#'   `qc_conditions`, restricting which rows of each side participate.
#' - `severity` — `"fail"` or `"warn"`.
#' - `description` — Prose.
#'
#' ## Filtering
#' Filter values are matched with `%in%`, so a character vector matches any of
#' the supplied values. All filter conditions must be satisfied simultaneously
#' (AND logic). An empty result produces a `message()` rather than a warning or
#' error.
#'
#' @export
#'
#' @examples
#' # Retrieve the full partner registry
#' # utl_mg_get_registry("partner_codes")
#'
#' # Filter sites for a single partner
#' # utl_mg_get_registry("site_codes", partner_code = "USA-MDA")
#'
#' # Filter sites for multiple partners at once
#' # utl_mg_get_registry("site_codes", partner_code = c("USA-MDA", "BLZ-CBC"))
#'
#' # Combine filters (AND logic): seagrass sites for a specific partner
#' # utl_mg_get_registry("site_codes", partner_code = "USA-MDA", habitat = "seagrass")
#'
#' # Look up observation IDs for a known species
#' # utl_mg_get_registry("observation_lookup", scientific_name = "Zostera marina")
#'
#' # Retrieve categorical values for a specific table
#' # utl_mg_get_registry("categorical_values", table_id = "sav_cover_v1")
#'
#' # Retrieve numeric range rules for a specific table
#' # utl_mg_get_registry("numeric_ranges", table_id = "sav_cover_v1")
#'
#' # Retrieve the conditional predicates for a specific table
#' # utl_mg_get_registry("qc_conditions", table_id = "sav_cover_v1")
#'
#' # Retrieve the grouped-aggregate rules for a specific table
#' # utl_mg_get_registry("aggregate_rules", table_id = "sav_cover_v1")
#'
#' # Retrieve database structure for a specific protocol
#' # utl_mg_get_registry("database_structure", protocol = "seagrass")
utl_mg_get_registry <- function(table, ...) {
  valid_tables <- c(
    "partner_codes",
    "site_codes",
    "observation_lookup",
    "taxonomic_lookup",
    "functional_group_lookup",
    "data_index",
    "database_structure",
    "categorical_values",
    "numeric_ranges",
    "qc_conditions",
    "aggregate_rules",
    "cross_table_rules"
  )

  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    stop("`table` must be a single non-NA character string.")
  }

  if (!table %in% valid_tables) {
    stop(
      "'",
      table,
      "' is not a recognized registry table. ",
      "Valid options are: ",
      paste(paste0('"', valid_tables, '"'), collapse = ", "),
      "."
    )
  }

  tbl <- .mg_get_registry_table(table)

  filters <- list(...)

  if (length(filters) > 0) {
    unknown_cols <- setdiff(names(filters), colnames(tbl))
    if (length(unknown_cols) > 0) {
      stop(
        "Unknown filter column(s) for table '",
        table,
        "': ",
        paste(paste0('"', unknown_cols, '"'), collapse = ", "),
        ". ",
        "Valid columns are: ",
        paste(paste0('"', colnames(tbl), '"'), collapse = ", "),
        "."
      )
    }

    for (col in names(filters)) {
      tbl <- tbl[tbl[[col]] %in% filters[[col]], , drop = FALSE]
    }
  }

  if (nrow(tbl) == 0L) {
    message("No rows matched the supplied filter(s) in table '", table, "'.")
  }

  tbl
}
