#' Example MarineGEO seagrass percent-cover data
#'
#' A subset of the `seagrass-cover-monitoring-v1` table, used in examples and
#' tests. Covers the USA-IRL (Indian River Lagoon) partner, sample collection
#' dates after 2021.
#'
#' @format A tibble with 1,321 rows and 19 columns:
#' \describe{
#'   \item{sample_event_id}{Sample event identifier, e.g. `"USA-IRL_BPS-001_2025"`.}
#'   \item{partner_code}{Partner code. Always `"USA-IRL"` in this subset.}
#'   \item{site_code}{Site code. Three sites are represented.}
#'   \item{site_name}{Human-readable site name.}
#'   \item{transect}{Transect number.}
#'   \item{quadrat}{Quadrat number along the transect.}
#'   \item{sample_collection_date}{Date of collection, ranging from 2022-07-14
#'     to 2025-06-30.}
#'   \item{scientific_name}{Scientific name of the observed taxon. Nine taxa
#'     are represented.}
#'   \item{cover_method}{Cover estimation method: `"Braun-Blanquet"` or
#'     `"percent bins"`.}
#'   \item{point_count}{Points at which the taxon was present, for point-count
#'     methods.}
#'   \item{points_in_quadrat}{Total points sampled in the quadrat.}
#'   \item{cover_code}{Ordinal cover class code, as recorded by `cover_method`.}
#'   \item{percent_cover}{Percent cover.}
#'   \item{sediment_type}{Sediment type recorded for the quadrat.}
#'   \item{cover_quadrat_dimensions}{Quadrat dimensions, e.g. `"50x50cm"`.}
#'   \item{cover_sample_notes}{Free-text notes.}
#'   \item{table_id}{Source table identifier.}
#'   \item{row_uuid}{Row-level UUID.}
#'   \item{input_filename}{Name of the file the row was ingested from.}
#' }
#' @source Assembled by `data-raw/example_data.R` from the MarineGEO Arrow
#'   dataset via [db_arrow_marinegeo()].
"seagrass_cover_example"
