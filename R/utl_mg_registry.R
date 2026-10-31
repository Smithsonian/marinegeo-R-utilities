#' Access MarineGEO global entity registry tables
#'
#' @description
#' Returns a registry table from the internal `marinegeo_metadata` object,
#' optionally filtered to a subset of rows. All data frame tables stored in
#' `marinegeo_metadata` are supported.
#'
#' @param table Character scalar. The registry table to retrieve. Must be one
#'   of `"partner_codes"`, `"site_codes"`, `"observation_lookup"`,
#'   `"taxonomic_lookup"`, `"nested_labelsets"`, `"data_index"`,
#'   `"database_structure"`, `"column_ids"`, `"numeric_ranges"`,
#'   `"controlled_vocabularies"`, or `"column_lookups"`.
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
#' **`nested_labelsets`**
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
#' - `missing_values` — `"fail"`, `"warn"`, or `"allow"`: how a missing value
#'   in this column is reported.
#' - `uuid_identity` — Marks the columns whose values together identify a row.
#' - `column_id` — What the column *holds*, independent of where it sits.
#'   Required and non-blank on every row. This is the key the three rule tables
#'   below are addressed by, so two columns that share a `column_id` share
#'   their rules even under different names or tables, and two columns that
#'   share only a name do not.
#'
#' **`column_ids`**
#'
#' The dictionary of `column_id` values: one row per identifier, regardless of
#' how many columns use it.
#' - `column_id` — The identifier.
#' - `label` — Human-readable label (e.g., `"Decimal latitude"`).
#' - `definition` — Prose definition, where one exists, otherwise `NA`.
#' - `unit` — Unit of measure (e.g., `"mm"`, `"degree"`), otherwise `NA`.
#'
#' **`numeric_ranges`**
#' - `rule_id` — Unique identifier of the rule, derived from its `column_id`
#'   and scope (e.g., `"leaf_length_mm__taxon_within__genus_zostera"`).
#'   Reported in the `rule_id` column of a [qc_issues] result.
#' - `column_id` — Links to `column_ids` and `database_structure`.
#' - `scope_type` — `NA` for a rule that applies wherever the `column_id`
#'   does, or `"taxon_within"` / `"taxon_not_within"` for a rule restricted to
#'   a clade.
#' - `scope_value` — `Rank:Name` or `tree:Label` naming the clade a scoped rule
#'   applies within (e.g., `"Genus:Zostera"`); `NA` when `scope_type` is.
#'   Expand one with [utl_mg_expand_taxon_set()].
#' - `min_fail` — Lower bound that triggers a fail (may be `NA`).
#' - `max_fail` — Upper bound that triggers a fail (may be `NA`).
#' - `min_warn` — Lower bound that triggers a warning (may be `NA`).
#' - `max_warn` — Upper bound that triggers a warning (may be `NA`).
#' - `range_type` — `"inclusive"` or `"exclusive"` boundary evaluation; `NA`
#'   rows are skipped by QC functions.
#'
#'   A `column_id` may carry more than one rule — typically one unconditional
#'   and one per clade — and every applicable rule is evaluated, so a single
#'   value can produce more than one issue.
#'
#' **`controlled_vocabularies`**
#' - `column_id` — Links to `column_ids`; the values of every column carrying
#'   this `column_id` are restricted to this vocabulary.
#' - `value` — A permitted value.
#'
#' **`column_lookups`**
#'
#' Binds a `column_id` to a registry table its values must appear in. A
#' `column_id` absent from this table gets no registry QC at all, which is how
#' a table using a partner organisation's own site coding opts out.
#' - `column_id` — Links to `column_ids`.
#' - `registry_table` — Registry the values are looked up in (e.g.,
#'   `"site_codes"`).
#' - `registry_column` — Column of that registry to match against (e.g.,
#'   `"site_code"`).
#' - `scope_type` — Optional narrowing operator. Blank means the column admits
#'   the whole `registry_column`. The only operator is `"in_labelset"`, which
#'   restricts it to the entries a nested labelset places at or below a
#'   reference.
#' - `scope_value` — One or more `|`-separated references into
#'   `nested_labelsets`, each either `"tree:Label"` (that node and its
#'   descendants) or `"tree:type=<value>"` (every node in that tree carrying
#'   that `type`, and their descendants). Several references give the union.
#'   Set together with `scope_type` or not at all.
#'
#'   Narrowing is an intersection on `scientific_id`, so a scoped binding must
#'   name a registry that has one — in practice `observation_lookup`. A value
#'   that is in the registry but outside the scope is reported as
#'   `out_of_labelset` rather than `unknown_lookup`. Use
#'   [utl_mg_expand_taxon_set()] to see what a reference resolves to.
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
#' # Retrieve the controlled vocabulary for one column_id
#' # utl_mg_get_registry("controlled_vocabularies", column_id = "cover_code")
#'
#' # Retrieve numeric range rules for one column_id
#' # utl_mg_get_registry("numeric_ranges", column_id = "leaf_length_mm")
#'
#' # Which registry bindings carry a labelset scope
#' # utl_mg_get_registry("column_lookups", scope_type = "in_labelset")
#'
#' # Which column_ids are bound to a registry lookup
#' # utl_mg_get_registry("column_lookups")
#'
#' # Retrieve database structure for a specific protocol
#' # utl_mg_get_registry("database_structure", protocol = "seagrass")
utl_mg_get_registry <- function(table, ...) {
  valid_tables <- c(
    "partner_codes",
    "site_codes",
    "observation_lookup",
    "taxonomic_lookup",
    "nested_labelsets",
    "data_index",
    "database_structure",
    "column_ids",
    "numeric_ranges",
    "controlled_vocabularies",
    "column_lookups"
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
