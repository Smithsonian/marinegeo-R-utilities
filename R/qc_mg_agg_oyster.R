# ---------------------------------------------------------------------------
# Oyster protocol aggregations. See R/qc_mg_aggregations.R for the registry and
# the contract these functions satisfy: take `data`, return a tidy frame of
# failing groups only.
# ---------------------------------------------------------------------------

# Substrate composition: the point counts scored in a quadrat must sum to the
# number of points that quadrat declares.
#
# Canopy taxa are excluded. They are scored over the top of the primary layer,
# so counting them would push a correct quadrat past its declared total -- this
# is the one piece of the retired metadata-driven check that needed a whole
# second rule table to express, and is a single filter here.
.qc_agg_oyster_substrate_points <- function(data) {
  data |>
    dplyr::filter(!(cover_type %in% "canopy taxa")) |>
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
    # A quadrat that declares no total is not checkable. Dropped explicitly
    # rather than left to NA-propagation in the comparison below, which would
    # pass it by accident.
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
