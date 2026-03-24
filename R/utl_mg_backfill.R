#' Backfill seagrass cover data with zero-cover absence rows
#'
#' @description
#' Accepts a seagrass cover data frame and adds new rows to ensure that every
#' Seagrass or Macroalgae species observed anywhere within a sample event is
#' represented at every transect × quadrat combination within that event. The
#' `percent_cover` and `cover_code` for backfilled rows are set to `0`.
#' Non-macrophyte rows (functional group is neither Seagrass nor Macroalgae)
#' are passed through unchanged.
#'
#' @param df A data frame containing seagrass cover observations. Must include
#'   the following columns:
#'   \describe{
#'     \item{`scientific_name`}{Character. Species or taxon name; used to
#'       determine functional group membership.}
#'     \item{`sample_event_id`}{Character. Unique identifier for each sampling
#'       event; used to group observations before expanding.}
#'     \item{`partner_code`}{Character. MarineGEO partner identifier.}
#'     \item{`site_name`}{Character. Site name.}
#'     \item{`sample_collection_date`}{Date. Date of sample collection.}
#'     \item{`transect`}{Transect identifier within a sample event.}
#'     \item{`quadrat`}{Quadrat identifier within a transect.}
#'     \item{`cover_method`}{Character. Method used to estimate cover.}
#'     \item{`cover_quadrat_dimensions`}{Character. Dimensions of the cover
#'       quadrat.}
#'     \item{`site_code`}{Character. Machine-readable site identifier (e.g.,
#'       `"BIS-001"`).}
#'     \item{`table_id`}{Character. Versioned identifier for the source data
#'       table; links to the MarineGEO data index.}
#'     \item{`input_filename`}{Character. Source file name.}
#'     \item{`percent_cover`}{Numeric. Percent cover value; set to `0` for
#'       backfilled rows.}
#'     \item{`cover_code`}{Cover code value; set to `0` for backfilled rows.}
#'   }
#'
#' @return A data frame with the same columns as `df`, sorted by
#'   `sample_event_id`, year, `site_name`, `transect`, `quadrat`, and
#'   `scientific_name`. Backfilled rows have `percent_cover = 0` and
#'   `cover_code = 0`; all other columns for backfilled rows are filled with
#'   the single value observed for that sample event, or `NA` with a
#'   `message()` if multiple values are present.
#'
#' @details
#' Functional group assignment is performed via
#' [utl_mg_assign_functional_groups()] with `fg = c("Seagrass", "Macroalgae")`.
#' Rows whose `scientific_name` resolves to neither group (including unknowns
#' and non-macrophyte taxa) are collected in a separate data frame and
#' re-appended to the output without modification.
#'
#' Within each sample event the function:
#' \enumerate{
#'   \item Uses [tidyr::expand()] with [tidyr::nesting()] to produce all
#'     combinations of `transect` × `quadrat` × `scientific_name` within the
#'     event's existing transect–quadrat pairs.
#'   \item Uses [dplyr::anti_join()] to identify combinations absent from the
#'     original data and inserts them with `percent_cover = 0` and
#'     `cover_code = 0`.
#' }
#'
#' If `cover_method`, `cover_quadrat_dimensions`, or `input_filename` is not
#' unique within a sample event, the backfilled rows for that event receive
#' `NA` for the ambiguous field and a `message()` is emitted.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # seagrass_cover_example is a built-in package dataset
#' backfilled <- utl_sav_backfill_cover(seagrass_cover_example)
#' nrow(backfilled) >= nrow(seagrass_cover_example)  # TRUE
#' }
utl_sav_backfill_cover <- function(df) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  required_cols <- c(
    "scientific_name",
    "sample_event_id",
    "partner_code",
    "site_code",
    "site_name",
    "table_id",
    "sample_collection_date",
    "transect",
    "quadrat",
    "cover_method",
    "cover_quadrat_dimensions",
    "input_filename",
    "percent_cover",
    "cover_code"
  )
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!is.character(df$scientific_name)) {
    stop("`scientific_name` must be a character column.")
  }

  if (nrow(df) == 0) {
    message("Input data frame has zero rows. Returning as-is.")
    return(df)
  }

  # --- Assign functional groups -----------------------------------------------
  df <- df |>
    dplyr::mutate(
      functional_group = utl_mg_assign_functional_groups(
        fg = c("Seagrass", "Macroalgae"),
        scientific_names = scientific_name
      )
    )

  df_non_macrophyte <- df |>
    dplyr::filter(!functional_group %in% c("Seagrass", "Macroalgae"))

  df_macrophyte <- df |>
    dplyr::filter(functional_group %in% c("Seagrass", "Macroalgae"))

  if (nrow(df_macrophyte) == 0) {
    message("No Seagrass or Macroalgae rows found. Returning input unchanged.")
    return(df)
  }

  # --- Backfill by sample event -----------------------------------------------
  sample_events <- unique(df_macrophyte$sample_event_id)

  df_out <- lapply(sample_events, function(i) {
    df_se <- df_macrophyte |>
      dplyr::filter(sample_event_id == i)

    # Resolve per-event metadata fields; warn if ambiguous
    cover_method <- unique(df_se$cover_method)
    quadrat_dimension <- unique(df_se$cover_quadrat_dimensions)
    input_filename <- unique(df_se$input_filename)

    if (length(cover_method) > 1) {
      cover_method <- NA_character_
      message("Unable to backfill cover method for ", i)
    }

    if (length(quadrat_dimension) > 1) {
      quadrat_dimension <- NA_character_
      message("Unable to backfill quadrat dimensions for ", i)
    }

    if (length(input_filename) > 1) {
      input_filename <- NA_character_
      message("Unable to backfill input filename for ", i)
    }

    # All transect x quadrat x scientific_name combinations implied by the data
    backfilled_grid <- df_se |>
      tidyr::expand(
        tidyr::nesting(
          sample_event_id,
          partner_code,
          site_code,
          site_name,
          table_id,
          sample_collection_date,
          transect
        ),
        quadrat,
        scientific_name
      )

    # New rows: combinations present in the grid but absent from the original
    new_rows <- dplyr::anti_join(
      backfilled_grid,
      df_se,
      by = dplyr::join_by(
        sample_event_id,
        partner_code,
        site_code,
        site_name,
        table_id,
        sample_collection_date,
        transect,
        quadrat,
        scientific_name
      )
    ) |>
      dplyr::mutate(
        cover_method = cover_method,
        cover_quadrat_dimensions = quadrat_dimension,
        input_filename = input_filename,
        percent_cover = 0,
        cover_code = 0
      )

    dplyr::bind_rows(df_se, new_rows)
  }) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(df_non_macrophyte) |>
    dplyr::arrange(
      sample_event_id,
      lubridate::year(sample_collection_date),
      site_code,
      site_name,
      transect,
      quadrat,
      scientific_name
    )

  df_out
}


#' Backfill seagrass density data with zero-density absence rows
#'
#' @description
#' Accepts a seagrass density data frame and adds new rows to ensure that every
#' Seagrass species observed anywhere within a sample event is represented at
#' every transect × quadrat combination within that event. The `shoot_count`
#' and `shoot_density_m2` for backfilled rows are set to `0`. Non-seagrass
#' rows (functional group is not Seagrass) are passed through unchanged.
#'
#' @param df A data frame containing seagrass density observations. Must include
#'   the following columns:
#'   \describe{
#'     \item{`scientific_name`}{Character. Species or taxon name; used to
#'       determine functional group membership.}
#'     \item{`sample_event_id`}{Character. Unique identifier for each sampling
#'       event; used to group observations before expanding.}
#'     \item{`partner_code`}{Character. MarineGEO partner identifier.}
#'     \item{`site_name`}{Character. Site name.}
#'     \item{`sample_collection_date`}{Date. Date of sample collection.}
#'     \item{`transect`}{Transect identifier within a sample event.}
#'     \item{`quadrat`}{Quadrat identifier within a transect.}
#'     \item{`density_quadrat_dimensions`}{Character. Dimensions of the density
#'       quadrat.}
#'     \item{`site_code`}{Character. Machine-readable site identifier (e.g.,
#'       `"BIS-001"`).}
#'     \item{`table_id`}{Character. Versioned identifier for the source data
#'       table; links to the MarineGEO data index.}
#'     \item{`input_filename`}{Character. Source file name.}
#'     \item{`shoot_count`}{Numeric. Raw shoot count; set to `0` for backfilled
#'       rows.}
#'     \item{`shoot_density_m2`}{Numeric. Shoot density per square metre; set
#'       to `0` for backfilled rows.}
#'   }
#'
#' @return A data frame with the same columns as `df`, sorted by
#'   `sample_event_id`, year, `site_name`, `transect`, `quadrat`, and
#'   `scientific_name`. Backfilled rows have `shoot_count = 0` and
#'   `shoot_density_m2 = 0`; all other columns for backfilled rows are filled
#'   with the single value observed for that sample event, or `NA` with a
#'   `message()` if multiple values are present.
#'
#' @details
#' Functional group assignment is performed via
#' [utl_mg_assign_functional_groups()] with `fg = "Seagrass"`. Rows whose
#' `scientific_name` resolves to a group other than Seagrass (including
#' unknowns and non-seagrass taxa) are collected in a separate data frame and
#' re-appended to the output without modification.
#'
#' Within each sample event the function:
#' \enumerate{
#'   \item Uses [tidyr::expand()] with [tidyr::nesting()] to produce all
#'     combinations of `transect` × `quadrat` × `scientific_name` within the
#'     event's existing transect–quadrat pairs.
#'   \item Uses [dplyr::anti_join()] to identify combinations absent from the
#'     original data and inserts them with `shoot_count = 0` and
#'     `shoot_density_m2 = 0`.
#' }
#'
#' If `density_quadrat_dimensions` or `input_filename` is not unique within a
#' sample event, the backfilled rows for that event receive `NA` for the
#' ambiguous field and a `message()` is emitted.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # seagrass_density_example is a built-in package dataset
#' backfilled <- utl_sav_backfill_density(seagrass_density_example)
#' nrow(backfilled) >= nrow(seagrass_density_example)  # TRUE
#' }
utl_sav_backfill_density <- function(df) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  required_cols <- c(
    "scientific_name",
    "sample_event_id",
    "partner_code",
    "site_code",
    "site_name",
    "table_id",
    "sample_collection_date",
    "transect",
    "quadrat",
    "density_quadrat_dimensions",
    "input_filename",
    "shoot_count",
    "shoot_density_m2"
  )

  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!is.character(df$scientific_name)) {
    stop("`scientific_name` must be a character column.")
  }

  if (nrow(df) == 0) {
    message("Input data frame has zero rows. Returning as-is.")
    return(df)
  }

  # --- Assign functional groups -----------------------------------------------
  df <- df |>
    dplyr::mutate(
      functional_group = utl_mg_assign_functional_groups(
        fg = "Seagrass",
        scientific_names = scientific_name
      )
    )

  df_non_seagrass <- df |>
    dplyr::filter(!functional_group %in% "Seagrass")

  df_seagrass <- df |>
    dplyr::filter(functional_group == "Seagrass")

  if (nrow(df_seagrass) == 0) {
    message("No Seagrass rows found. Returning input unchanged.")
    return(df)
  }

  # --- Backfill by sample event -----------------------------------------------
  sample_events <- unique(df_seagrass$sample_event_id)

  df_out <- lapply(sample_events, function(i) {
    df_se <- df_seagrass |>
      dplyr::filter(sample_event_id == i)

    # Resolve per-event metadata fields; warn if ambiguous
    quadrat_dimension <- unique(df_se$density_quadrat_dimensions)
    input_filename <- unique(df_se$input_filename)

    if (length(quadrat_dimension) > 1) {
      quadrat_dimension <- NA_character_
      message("Unable to backfill quadrat dimensions for ", i)
    }

    if (length(input_filename) > 1) {
      input_filename <- NA_character_
      message("Unable to backfill input filename for ", i)
    }

    # All transect x quadrat x scientific_name combinations implied by the data
    backfilled_grid <- df_se |>
      tidyr::expand(
        tidyr::nesting(
          sample_event_id,
          partner_code,
          site_code,
          site_name,
          table_id,
          sample_collection_date,
          transect
        ),
        quadrat,
        scientific_name
      )

    # New rows: combinations present in the grid but absent from the original
    new_rows <- dplyr::anti_join(
      backfilled_grid,
      df_se,
      by = dplyr::join_by(
        sample_event_id,
        partner_code,
        site_code,
        site_name,
        table_id,
        sample_collection_date,
        transect,
        quadrat,
        scientific_name
      )
    ) |>
      dplyr::mutate(
        density_quadrat_dimensions = quadrat_dimension,
        input_filename = input_filename,
        shoot_count = 0,
        shoot_density_m2 = 0
      )

    dplyr::bind_rows(df_se, new_rows)
  }) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(df_non_seagrass) |>
    dplyr::arrange(
      sample_event_id,
      lubridate::year(sample_collection_date),
      site_code,
      site_name,
      transect,
      quadrat,
      scientific_name
    )

  df_out
}
