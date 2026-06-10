#' Look up scientific IDs for a vector of scientific names
#'
#' @description
#' Given a character vector of scientific names, returns the corresponding
#' `scientific_id` values from the MarineGEO `observation_lookup` table.
#' Designed for use inside `dplyr::mutate()` or standalone verification of a name vector.
#'
#' `scientific_id` is the primary identifier linking observations across
#' MarineGEO metadata tables. It is either an Aphia ID (e.g. `"urn:lsid:marinespecies.org:taxname:123"`) for
#' species/taxonomic rank observations, a functional group ID (e.g.
#' `"FUNCTIONAL:SEAGRASS"`) for observations recorded at a functional group level,
#' or a temporary undefined label (`"UNDEFINED:SOMETHINGNOVEL"``)
#'
#' @param scientific_name Character vector of scientific names to look up.
#' @param drop_abbreviations Logical. If `TRUE` (default), trailing rank
#'   abbreviations such as `sp.`, `spp.`, `sp. A`, or `sp. 1` are stripped
#'   before the lookup so that e.g. `"Halodule sp."` resolves the same as
#'   `"Halodule"`. The cleaned name is used for matching.
#'
#' @return A character vector the same length as `scientific_name`. Each
#'   element is the matched `scientific_id`, or `NA` when a name could not be
#'   found in `observation_lookup`. A warning lists any unmatched names.
#'
#' @details
#' The lookup is performed against `marinegeo_metadata$observation_lookup`,
#' distributed with the package as internal data (see
#' `data-raw/assemble_marinegeo_metadata_sysdata.R`).
#'
#' `NA` elements in `scientific_name` pass through as `NA` in the result
#' without triggering an unmatched-name warning.
#'
#' @export
#'
#' @examples
#' # Standalone verification
#' utl_mg_get_scientific_id(c("Halodule wrightii", "Thalassia testudinum"))
#'
#' # Inside mutate()
#' dplyr::mutate(seagrass_cover_example,
#'               scientific_id = utl_mg_get_scientific_id(scientific_name))
#'
#' # Abbreviations are stripped before matching
#' utl_mg_get_scientific_id("Halodule sp.")
.strip_rank_abbreviations <- function(x) {
  trimws(
    stringr::str_remove(
      x,
      stringr::regex("\\s+spp?\\.?\\b.*$", ignore_case = TRUE)
    )
  )
}

utl_mg_get_scientific_id <- function(
  scientific_name,
  drop_abbreviations = TRUE
) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(scientific_name)) {
    stop("`scientific_name` must be a character vector.")
  }

  if (!is.logical(drop_abbreviations) || length(drop_abbreviations) != 1) {
    stop("`drop_abbreviations` must be a single logical value (TRUE or FALSE).")
  }

  # --- Build lookup table -----------------------------------------------------
  obs_lookup <- marinegeo_metadata$observation_lookup |>
    dplyr::select(scientific_name, scientific_id) |>
    dplyr::distinct()

  lookup_vec <- stats::setNames(
    obs_lookup$scientific_id,
    obs_lookup$scientific_name
  )

  # --- Optionally strip trailing rank abbreviations ---------------------------
  names_to_match <- scientific_name
  if (drop_abbreviations) {
    names_to_match <- .strip_rank_abbreviations(names_to_match)
  }

  # --- Perform lookup (preserves NA positions) --------------------------------
  result <- lookup_vec[names_to_match]
  result <- unname(result)

  # NA from unmatched names and NA from original NAs both land as NA here;
  # only warn about the former.
  unmatched <- unique(scientific_name[!is.na(scientific_name) & is.na(result)])

  if (length(unmatched) > 0) {
    warning(
      length(unmatched),
      " scientific name(s) could not be matched in `observation_lookup` ",
      "and will return NA: ",
      paste(unmatched, collapse = ", ")
    )
  }

  result
}
