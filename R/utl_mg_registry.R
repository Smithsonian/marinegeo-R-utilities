#' Access MarineGEO global entity registry tables
#'
#' @description
#' Returns a registry table from the internal `marinegeo_metadata` object,
#' optionally filtered to a subset of rows. Supported tables are
#' `"partner_codes"`, `"site_names"`, and `"observation_lookup"`.
#'
#' @param table Character scalar. The registry table to retrieve. Must be one
#'   of `"partner_codes"`, `"site_names"`, or `"observation_lookup"`.
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
#' **`site_names`**
#' - `partner_code` — Links to `partner_codes`.
#' - `site_name` — Human-readable site name.
#' - `habitat` — Dominant habitat type (e.g., `"seagrass"`, `"coral reef"`).
#' - `latitude` — Decimal latitude (may be `NA`).
#' - `longitude` — Decimal longitude (may be `NA`).
#'
#' **`observation_lookup`**
#' - `scientific_name` — Species or functional group name as entered in
#'   MarineGEO datasets.
#' - `scientific_id` — Unique identifier (Aphia ID or functional group ID).
#'   Additional columns may be present depending on the version of the metadata.
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
#' # utl_mg_get_registry("site_names", partner_code = "USA-MDA")
#'
#' # Filter sites for multiple partners at once
#' # utl_mg_get_registry("site_names", partner_code = c("USA-MDA", "BLZ-CBC"))
#'
#' # Combine filters (AND logic): seagrass sites for a specific partner
#' # utl_mg_get_registry("site_names", partner_code = "USA-MDA", habitat = "seagrass")
#'
#' # Look up observation IDs for a known species
#' # utl_mg_get_registry("observation_lookup", scientific_name = "Zostera marina")
utl_mg_get_registry <- function(table, ...) {
  valid_tables <- c("partner_codes", "site_names", "observation_lookup")

  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    stop("`table` must be a single non-NA character string.")
  }

  if (!table %in% valid_tables) {
    stop(
      "'", table, "' is not a recognized registry table. ",
      "Valid options are: ",
      paste(paste0('"', valid_tables, '"'), collapse = ", "), "."
    )
  }

  tbl <- marinegeo_metadata[[table]]

  filters <- list(...)

  if (length(filters) > 0) {
    unknown_cols <- setdiff(names(filters), colnames(tbl))
    if (length(unknown_cols) > 0) {
      stop(
        "Unknown filter column(s) for table '", table, "': ",
        paste(paste0('"', unknown_cols, '"'), collapse = ", "), ". ",
        "Valid columns are: ",
        paste(paste0('"', colnames(tbl), '"'), collapse = ", "), "."
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
