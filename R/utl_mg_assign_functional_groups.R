#' Assign a functional group to each scientific name from a candidate set
#'
#' @description
#' For use inside [dplyr::mutate()]. Given a set of candidate functional group
#' names (`fg_labels`) and a vector of scientific names (`scientific_names`),
#' returns a character vector of the same length indicating which candidate
#' group each name belongs to.
#'
#' Returns `NA` for a name when it matches none of the candidate groups or when
#' it matches more than one. A `message()` is issued in both cases listing the
#' affected names.
#'
#' @param fg_tree Character scalar. Name of the functional group tree to query
#'   (e.g. `"vegetation"`). Passed to [utl_mg_get_functional_groups()] as
#'   `functional_group_tree`.
#' @param fg_labels Non-empty character vector of functional group names to test
#'   membership against (e.g. `c("Seagrass", "Macroalgae")`). Names are
#'   matched exactly (case-sensitive) against the `group_name` column returned
#'   by [utl_mg_get_functional_groups()].
#' @param scientific_names Character vector of scientific names to classify,
#'   typically a column from a data frame passed through [dplyr::mutate()].
#'   `NA` values are preserved as `NA` in the output without a message.
#'
#' @return A character vector of the same length as `scientific_names`. Each
#'   element is one of the values in `fg_labels` (the matched group name), or
#'   `NA_character_` if:
#'   \itemize{
#'     \item The name is `NA`.
#'     \item The name is not found in `observation_lookup`.
#'     \item The name does not belong to any group in `fg_labels`.
#'     \item The name belongs to more than one group in `fg_labels`.
#'   }
#'
#' @details
#' Functional group membership is resolved via two lookups:
#' \enumerate{
#'   \item [utl_mg_get_scientific_id()] maps scientific names to `scientific_id`
#'     values, with trailing rank abbreviations (e.g. `"sp."`, `"spp."`) stripped
#'     by default before matching. Unresolved names trigger a `warning()`.
#'   \item The functional group tree is traversed by [utl_mg_get_functional_groups()]
#'     to find all ancestor `group_name` values for each ID.
#' }
#' Group name matching is exact and case-sensitive.
#'
#' The function processes all unique names in a single batch, making it
#' efficient for use in [dplyr::mutate()] over large data frames.
#'
#' @export
#'
#' @examples
#' df <- seagrass_cover_example
#' df |>
#'   dplyr::mutate(
#'     functional_group = utl_mg_assign_functional_groups(
#'       fg_tree = "vegetation",
#'       fg_labels = c("Seagrass", "Algae"),
#'       scientific_names = scientific_name
#'     )
#'   )
utl_mg_assign_functional_groups <- function(
  fg_tree,
  fg_labels,
  scientific_names
) {
  # --- Input validation -------------------------------------------------------
  if (!is.character(fg_labels) || length(fg_labels) == 0) {
    stop(
      "`fg_labels` must be a non-empty character vector of functional group names."
    )
  }
  if (!is.character(scientific_names)) {
    stop("`scientific_names` must be a character vector.")
  }

  # --- Resolve scientific names to scientific_ids ----------------------------
  ids <- utl_mg_get_scientific_id(scientific_names)

  obs_lookup <- dplyr::tibble(
    scientific_name = scientific_names,
    scientific_id   = ids
  ) |>
    dplyr::filter(!is.na(scientific_name)) |>
    dplyr::distinct()

  unique_ids <- unique(ids[!is.na(ids)])

  if (length(unique_ids) == 0) {
    return(rep(NA_character_, length(scientific_names)))
  }

  # --- Get functional group memberships (batched) ----------------------------
  fg_memberships <- utl_mg_get_functional_groups(
    unique_ids,
    functional_group_tree = fg_tree
  )

  relevant <- fg_memberships |>
    dplyr::filter(group_name %in% fg_labels) |>
    dplyr::select(scientific_id, group_name)

  # --- Summarize matches per scientific name ----------------------------------
  name_fg <- obs_lookup |>
    dplyr::left_join(relevant, by = "scientific_id") |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarize(
      matched = list(unique(group_name[!is.na(group_name)])),
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
    multi_names <- name_fg$scientific_name[multi_idx]
    multi_groups <- vapply(
      name_fg$matched[multi_idx],
      paste,
      character(1),
      collapse = ", "
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
    vapply(
      name_fg$matched,
      function(m) {
        if (length(m) == 1L) m else NA_character_
      },
      character(1)
    ),
    name_fg$scientific_name
  )

  # --- Expand to full-length output ------------------------------------------
  result <- assignment_map[scientific_names]
  result[is.na(scientific_names)] <- NA_character_
  names(result) <- NULL

  result
}
