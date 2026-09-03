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
#'   `"database_structure"`, `"categorical_values"`, or `"numeric_ranges"`.
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
#' - `level` — Data processing level.
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
    "numeric_ranges"
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
