# ---------------------------------------------------------------------------
# Fouling protocol aggregations. See R/qc_mg_aggregations.R for the registry and
# the contract these functions satisfy: take `data`, return a tidy frame of
# failing groups only.
# ---------------------------------------------------------------------------

# Cover: the point counts scored on a panel must sum to the number of points in
# that panel's scoring grid.
#
# Grouped by panel and deployment period rather than by quadrat, and with no
# taxon or method filter: both columns are required in this table, so the check
# always applies. Note that utl_fouling_backfill_cover() adds absent taxa with
# `point_count = 0`, which leaves a panel's total unchanged.
.qc_agg_fouling_cover_points <- function(data) {
  data |>
    dplyr::group_by(sample_event_id, panel_id, deployment_period) |>
    dplyr::summarise(
      observed = sum(point_count, na.rm = TRUE),
      expected = .qc_agg_declared(points_in_grid),
      n_expected = dplyr::n_distinct(points_in_grid, na.rm = TRUE),
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
