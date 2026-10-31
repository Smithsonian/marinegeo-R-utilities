# ---------------------------------------------------------------------------
# Seagrass protocol aggregations. See R/qc_mg_aggregations.R for the registry
# and the contract these functions satisfy: take `data`, return a tidy frame of
# failing groups only.
# ---------------------------------------------------------------------------

# Cover: the point counts scored in a quadrat must sum to the number of points
# that quadrat declares.
#
# Only quadrats recorded with `cover_method == "point count"` are checked. Both
# `point_count` and `points_in_quadrat` are optional in this table and most
# partners record cover by Braun-Blanquet or percent bins instead, leaving them
# empty -- summing those would compare 0 against NA, which the comparison then
# drops. That reads as a pass but checked nothing, so the method filter makes
# the restriction explicit.
.qc_agg_seagrass_cover_points <- function(data) {
  data |>
    dplyr::filter(cover_method %in% "point count") |>
    dplyr::group_by(
      sample_event_id,
      transect,
      quadrat,
      sample_collection_date
    ) |>
    dplyr::summarise(
      observed = sum(point_count, na.rm = TRUE),
      expected = .qc_agg_declared(points_in_quadrat),
      n_expected = dplyr::n_distinct(points_in_quadrat, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(expected)) |>
    dplyr::mutate(
      issue = dplyr::if_else(
        n_expected > 1L,
        "inconsistent_total",
        "aggregate_mismatch"
      )
    ) |>
    dplyr::filter(observed != expected | n_expected > 1L) |>
    dplyr::select(-n_expected)
}
