#' Generate stable row UUIDs from identity columns
#'
#' @description
#' Adds a `row_uuid` column to a data frame by hashing the values of the
#' columns designated as row identity for the given table. Identity columns are
#' defined in `marinegeo_metadata$database_structure` via the `uuid_identity`
#' logical flag.
#'
#' The UUID is deterministic: the same combination of identity-column values
#' always produces the same `row_uuid`, making it safe to regenerate across
#' pipeline runs and to use as a stable join key.
#'
#' @param data A data frame containing at least the identity columns defined
#'   for `table_id` in `marinegeo_metadata$database_structure`.
#' @param table_id Character scalar. The MarineGEO table identifier (from
#'   `marinegeo_metadata$data_index$table_id`). Used to look up which columns
#'   carry `uuid_identity = TRUE` in `database_structure`.
#'
#' @return `data` with a `row_uuid` character column prepended as the first
#'   column. Each value is a UUID-formatted string derived from a SHA1 hash of
#'   the row's identity column values (e.g.,
#'   `"3b4a1c2d-e5f6-7a8b-9c0d-e1f2a3b4c5d6"`).
#'
#' @details
#' ## How UUIDs are generated
#' For each row, the values of the `uuid_identity` columns are coerced to
#' character and concatenated with `"|"` as a separator. That string is hashed
#' with SHA1 via [digest::digest()] (`serialize = FALSE`). The first 32
#' hexadecimal characters of the hash are formatted as a standard UUID
#' (`8-4-4-4-12`).
#'
#' ## NA handling
#' `NA` values in identity columns are coerced to the string `"NA"` before
#' hashing. A row with all-`NA` identity columns will receive a valid UUID, but
#' it will be identical to every other such row — uniqueness is only guaranteed
#' when identity columns are non-`NA`.
#'
#' ## Overwriting an existing column
#' If `data` already contains a `row_uuid` column it will be dropped and
#' regenerated with a warning.
#'
#' @export
#'
#' @examples
#' db_struct <- data.frame(
#'   table_id      = c("sav_cover_v1", "sav_cover_v1", "sav_cover_v1"),
#'   column_name   = c("site_code", "transect_id", "percent_cover"),
#'   uuid_identity = c(TRUE, TRUE, FALSE),
#'   stringsAsFactors = FALSE
#' )
#'
#' df <- data.frame(
#'   site_code     = c("BIS-001", "BIS-001"),
#'   transect_id   = c(1L, 2L),
#'   percent_cover = c(45.2, 30.1),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Not run (requires internal metadata):
#' # utl_mg_generate_row_uuid(df, table_id = "sav_cover_v1")
utl_mg_generate_row_uuid <- function(data, table_id) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(table_id) || length(table_id) != 1L || is.na(table_id)) {
    stop("`table_id` must be a single non-NA character string.")
  }

  # --- Metadata lookup --------------------------------------------------------
  tbl_struct <- marinegeo_metadata$database_structure |>
    dplyr::filter(table_id == !!table_id)

  if (nrow(tbl_struct) == 0L) {
    stop(
      "No rows found in `database_structure` for table_id '",
      table_id,
      "'. ",
      "Check `marinegeo_metadata$data_index` for valid table_id values."
    )
  }

  if (!"uuid_identity" %in% colnames(tbl_struct)) {
    stop(
      "`database_structure` does not have a `uuid_identity` column. ",
      "Rebuild the internal metadata with `data-raw/assemble_marinegeo_metadata_sysdata.R`."
    )
  }

  # --- Identity columns -------------------------------------------------------
  id_cols <- tbl_struct |>
    dplyr::filter(uuid_identity) |>
    dplyr::pull(column_name)

  if (length(id_cols) == 0L) {
    stop(
      "No columns with `uuid_identity = TRUE` found for table_id '",
      table_id,
      "' in `database_structure`."
    )
  }

  # --- Column presence check --------------------------------------------------
  missing_cols <- setdiff(id_cols, colnames(data))
  if (length(missing_cols) > 0L) {
    stop(
      "Identity column(s) required for '",
      table_id,
      "' are not present in `data`: ",
      paste(paste0('"', missing_cols, '"'), collapse = ", "),
      "."
    )
  }

  # --- Overwrite guard --------------------------------------------------------
  if ("row_uuid" %in% colnames(data)) {
    warning(
      "`data` already contains a `row_uuid` column; it will be regenerated."
    )
    data[["row_uuid"]] <- NULL
  }

  # --- Generate UUIDs ---------------------------------------------------------
  content <- apply(data[, id_cols, drop = FALSE], 1L, function(row) {
    paste(as.character(row), collapse = "|")
  })

  row_uuid <- vapply(
    content,
    function(x) {
      hash <- digest::digest(x, algo = "sha1", serialize = FALSE)
      .format_as_uuid(hash)
    },
    character(1L),
    USE.NAMES = FALSE
  )

  # --- Prepend row_uuid as first column ---------------------------------------
  data <- dplyr::mutate(data, row_uuid = row_uuid)
  data <- dplyr::relocate(data, row_uuid)

  data
}

# ---------------------------------------------------------------------------
# Internal helper: format a SHA1 hex string as a UUID (8-4-4-4-12)
# ---------------------------------------------------------------------------
.format_as_uuid <- function(hash) {
  # SHA1 produces 40 hex chars; take the first 32 for UUID formatting
  h <- substr(hash, 1L, 32L)
  paste(
    substr(h, 1L, 8L),
    substr(h, 9L, 12L),
    substr(h, 13L, 16L),
    substr(h, 17L, 20L),
    substr(h, 21L, 32L),
    sep = "-"
  )
}
