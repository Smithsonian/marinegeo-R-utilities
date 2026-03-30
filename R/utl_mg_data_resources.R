#' Return the expected column order for a MarineGEO table
#'
#' @description
#' Looks up the expected column names for a given MarineGEO table identifier
#' from the internal `database_structure` metadata. The returned vector defines
#' the canonical column order for that table and can be used to reorder or
#' validate columns in a data frame.
#'
#' @param table_id Character scalar. A MarineGEO table identifier (e.g.,
#'   `"sav_cover_v1"`). Must match a `table_id` present in
#'   `marinegeo_metadata$database_structure`. Use [utl_mg_list_tables()] to
#'   browse valid table identifiers.
#'
#' @return A character vector of column names in the expected order for the
#'   given table. Returns an empty character vector if the table has no column
#'   metadata (with a warning).
#'
#' @details
#' Column order is determined by the row order of the `database_structure`
#' metadata for the given `table_id`. This matches the order enforced by
#' [qc_check_columns()].
#'
#' If `table_id` is not found in `database_structure`, the function stops with
#' an informative error. Call [utl_mg_list_tables()] to see all valid table
#' identifiers with their protocol and name.
#'
#' @export
#'
#' @examples
#' # utl_mg_column_order("sav_cover_v1")
#'
#' # Browse available table IDs first:
#' # utl_mg_list_tables()
utl_mg_column_order <- function(table_id) {
  if (!is.character(table_id) || length(table_id) != 1L || is.na(table_id)) {
    stop("`table_id` must be a single non-NA character string.")
  }

  valid_ids <- unique(marinegeo_metadata$database_structure$table_id)

  if (!table_id %in% valid_ids) {
    stop(
      "'",
      table_id,
      "' is not a recognized table_id. ",
      "Call `utl_mg_list_tables()` to see all available table IDs, ",
      "protocols, and names."
    )
  }

  marinegeo_metadata$database_structure |>
    dplyr::filter(table_id == !!table_id) |>
    dplyr::pull(column_name)
}


#' List all available MarineGEO table identifiers
#'
#' @description
#' Returns a data frame of all table identifiers registered in the MarineGEO
#' data index, with their associated protocol and human-readable table name.
#' Useful for discovering valid `table_id` values to pass to other functions
#' such as [utl_mg_column_order()] and [qc_run()].
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{`table_id`}{Character. The versioned table identifier used across
#'       MarineGEO metadata and QC functions.}
#'     \item{`protocol`}{Character. The monitoring or experiment program the
#'       table belongs to (e.g., `"seagrass"`, `"oyster_reef"`).}
#'     \item{`table_name`}{Character. A human-readable label for the table.}
#'   }
#'
#' @export
#'
#' @examples
#' utl_mg_list_tables()
utl_mg_list_tables <- function() {
  marinegeo_metadata$data_index |>
    dplyr::select(table_id, protocol, table_name)
}
