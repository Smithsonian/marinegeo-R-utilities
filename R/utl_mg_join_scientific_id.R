#' Join scientific ID to a data frame by scientific name
#'
#' @description
#' Looks up the `scientific_id` for each row in `df` by matching the
#' `scientific_name` column against the MarineGEO `observation_lookup` table
#' (available via `marinegeo_metadata$observation_lookup`).
#'
#' `scientific_id` is the primary identifier linking observations across
#' MarineGEO metadata tables. It is either an Aphia ID (e.g. `"APHIA:123"`) for
#' species/taxonomic rank observations, or a functional group ID (e.g.
#' `"FUNCTIONAL:2"`) for observations recorded at a functional group level.
#' See `docs/taxonomy_and_functional_groups.md` for full details.
#'
#' @param df A data frame containing a column of scientific names.
#' @param scientific_name_col Character. Name of the column in `df` that
#'   contains scientific names. Defaults to `"scientific_name"`.
#'
#' @return `df` with a `scientific_id` column appended. Rows whose
#'   `scientific_name` could not be matched in `observation_lookup` will have
#'   `NA` for `scientific_id`. A warning is issued when any names are
#'   unmatched.
#'
#' @details
#' The join is performed against `marinegeo_metadata$observation_lookup`, which
#' maps every known MarineGEO observation name to a `scientific_id`. The lookup
#' table is distributed with the package as internal data (see
#' `data-raw/assemble_marinegeo_metadata_sysdata.R`).
#'
#' If `df` already contains a `scientific_id` column it will be overwritten with
#' a warning.
#'
#' @export
#'
#' @examples
#' # seagrass_cover_example ships with the package and has a scientific_name column
#' utl_mg_join_scientific_id(seagrass_cover_example)
#'
#' # Use a different column name
#' df <- seagrass_cover_example
#' names(df)[names(df) == "scientific_name"] <- "Species"
#' utl_mg_join_scientific_id(df, scientific_name_col = "Species")
utl_mg_join_scientific_id <- function(
  df,
  scientific_name_col = "scientific_name"
) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  if (!is.character(scientific_name_col) || length(scientific_name_col) != 1) {
    stop("`scientific_name_col` must be a single character string.")
  }

  if (!scientific_name_col %in% colnames(df)) {
    stop("Column '", scientific_name_col, "' not found in `df`.")
  }

  if ("scientific_id" %in% colnames(df)) {
    warning(
      "`df` already contains a `scientific_id` column; it will be overwritten."
    )
    df[["scientific_id"]] <- NULL
  }

  # --- Lookup -----------------------------------------------------------------
  obs_lookup <- marinegeo_metadata$observation_lookup |>
    dplyr::select(scientific_name, scientific_id) |>
    dplyr::distinct()

  # Temporarily rename the target column to "scientific_name" for the join,
  # then restore the original name afterwards.
  original_col <- scientific_name_col
  using_alias <- original_col != "scientific_name"

  if (using_alias) {
    df <- df |> dplyr::rename(scientific_name = dplyr::all_of(original_col))
  }

  result <- df |>
    dplyr::left_join(obs_lookup, by = "scientific_name")

  # --- Report unmatched names -------------------------------------------------
  unmatched <- result |>
    dplyr::filter(is.na(scientific_id)) |>
    dplyr::pull(scientific_name) |>
    unique()

  if (length(unmatched) > 0) {
    warning(
      length(unmatched),
      " scientific name(s) could not be matched in ",
      "`observation_lookup` and will have NA for `scientific_id`: ",
      paste(unmatched, collapse = ", ")
    )
  }

  # Restore the original column name
  if (using_alias) {
    result <- result |>
      dplyr::rename(dplyr::all_of(stats::setNames(
        "scientific_name",
        original_col
      )))
  }

  result
}
