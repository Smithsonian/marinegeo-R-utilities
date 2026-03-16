#' Assign a functional group to each scientific name from a candidate set
#'
#' @description
#' For use inside [dplyr::mutate()]. Given a set of candidate functional group
#' names (`fg`) and a vector of scientific names (`scientific_names`), returns a
#' character vector of the same length indicating which candidate group each
#' name belongs to.
#'
#' Returns `NA` for a name when it matches none of the candidate groups or when
#' it matches more than one. A `message()` is issued in both cases listing the
#' affected names.
#'
#' @param fg Non-empty character vector of functional group names to test
#'   membership against (e.g. `c("Seagrass", "Macroalgae")`). Names are
#'   matched exactly (case-sensitive) against the `parent_name` column returned
#'   by [utl_mg_get_functional_groups()].
#' @param scientific_names Character vector of scientific names to classify,
#'   typically a column from a data frame passed through [dplyr::mutate()].
#'   `NA` values are preserved as `NA` in the output without a message.
#'
#' @return A character vector of the same length as `scientific_names`. Each
#'   element is one of the values in `fg` (the matched group name), or
#'   `NA_character_` if:
#'   \itemize{
#'     \item The name is `NA`.
#'     \item The name is not found in `observation_lookup`.
#'     \item The name does not belong to any group in `fg`.
#'     \item The name belongs to more than one group in `fg`.
#'   }
#'
#' @details
#' Functional group membership is resolved via two lookups from
#' `marinegeo_metadata`:
#' \enumerate{
#'   \item `observation_lookup` maps scientific names to `scientific_id` values.
#'   \item The functional group enrollment tree is walked by
#'     [utl_mg_get_functional_groups()] to find all ancestor group names for
#'     each ID.
#' }
#' Group name matching is exact and case-sensitive.
#'
#' The function processes all unique names in a single batch, making it
#' efficient for use in [dplyr::mutate()] over large data frames.
#'
#' @export
#'
#' @examples
#' df <- utl_mg_join_scientific_id(seagrass_cover_example)
#' df |>
#'   dplyr::mutate(
#'     functional_group = utl_mg_assign_functional_groups(
#'       fg = c("Seagrass", "Macroalgae"),
#'       scientific_names = scientific_name
#'     )
#'   )
utl_mg_assign_functional_groups <- function(fg, scientific_names) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(fg) || length(fg) == 0) {
    stop("`fg` must be a non-empty character vector of functional group names.")
  }
  if (!is.character(scientific_names)) {
    stop("`scientific_names` must be a character vector.")
  }

  # --- Resolve scientific names to scientific_ids ----------------------------
  unique_names <- unique(scientific_names[!is.na(scientific_names)])

  if (length(unique_names) == 0) {
    return(rep(NA_character_, length(scientific_names)))
  }

  obs_lookup <- marinegeo_metadata$observation_lookup |>
    dplyr::filter(scientific_name %in% unique_names) |>
    dplyr::select(scientific_name, scientific_id) |>
    dplyr::distinct()

  unresolved <- setdiff(unique_names, obs_lookup$scientific_name)
  if (length(unresolved) > 0) {
    message(
      length(unresolved),
      " scientific name(s) not found in observation_lookup and will be ",
      "assigned NA: ",
      paste(unresolved, collapse = ", ")
    )
  }

  unique_ids <- unique(obs_lookup$scientific_id[!is.na(obs_lookup$scientific_id)])

  if (length(unique_ids) == 0) {
    return(rep(NA_character_, length(scientific_names)))
  }

  # --- Get functional group memberships (batched) ----------------------------
  fg_memberships <- utl_mg_get_functional_groups(unique_ids)

  relevant <- fg_memberships[fg_memberships$parent_name %in% fg,
                             c("scientific_id", "parent_name"),
                             drop = FALSE]

  # --- Summarize matches per scientific name ----------------------------------
  name_fg <- obs_lookup |>
    dplyr::left_join(relevant, by = "scientific_id") |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarize(
      matched = list(unique(parent_name[!is.na(parent_name)])),
      .groups = "drop"
    )

  n_matched <- vapply(name_fg$matched, length, integer(1))

  # Report names that matched no group
  no_match_names <- name_fg$scientific_name[n_matched == 0]
  if (length(no_match_names) > 0) {
    message(
      length(no_match_names),
      " scientific name(s) did not match any of the provided functional ",
      "groups and will be assigned NA: ",
      paste(no_match_names, collapse = ", ")
    )
  }

  # Report names that matched multiple groups
  multi_idx <- which(n_matched > 1)
  if (length(multi_idx) > 0) {
    multi_names  <- name_fg$scientific_name[multi_idx]
    multi_groups <- vapply(
      name_fg$matched[multi_idx],
      paste, character(1), collapse = ", "
    )
    message(
      length(multi_idx),
      " scientific name(s) matched multiple functional groups and will be ",
      "assigned NA: ",
      paste0(multi_names, " (", multi_groups, ")", collapse = "; ")
    )
  }

  # Build named assignment vector: name -> single matched group or NA
  assignment_map <- stats::setNames(
    vapply(name_fg$matched, function(m) {
      if (length(m) == 1L) m else NA_character_
    }, character(1)),
    name_fg$scientific_name
  )

  # --- Expand to full-length output ------------------------------------------
  result <- assignment_map[scientific_names]
  result[is.na(scientific_names)] <- NA_character_
  names(result) <- NULL

  result
}
