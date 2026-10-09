#' Backfill seagrass cover data with zero-cover absence rows
#'
#' @description
#' Accepts a seagrass cover data frame and adds new rows to ensure that every
#' Seagrass or Algae species observed anywhere within a sample event is
#' represented at every transect × quadrat combination within that event. The
#' `percent_cover` and `cover_code` for backfilled rows are set to `0`.
#' Non-vegetation rows (functional group is neither Seagrass nor Algae)
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
#' [utl_mg_assign_functional_groups()] with `fg = c("Seagrass", "Algae")`.
#' Rows whose `scientific_name` resolves to neither group (including unknowns
#' and non-vegetation taxa) are collected in a separate data frame and
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
        fg_tree = "vegetation",
        fg_labels = c("Seagrass", "Algae"),
        scientific_names = scientific_name
      )
    )

  df_non_macrophyte <- df |>
    dplyr::filter(!functional_group %in% c("Seagrass", "Algae"))

  df_macrophyte <- df |>
    dplyr::filter(functional_group %in% c("Seagrass", "Algae"))

  if (nrow(df_macrophyte) == 0) {
    message("No Seagrass or Algae rows found. Returning input unchanged.")
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
        fg_tree = "vegetation",
        fg_labels = "Seagrass",
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


#' Backfill oyster and countable non-oyster mollusk density data with zero-density absence rows
#'
#' @description
#' Accepts an oyster density data frame and adds new rows to ensure that every
#' oyster or countable non-oyster bivalve species observed anywhere within a sample event is represented at
#' every transect × quadrat combination within that event. The `count`
#' and `density_m2` for backfilled rows are set to `0`. Other sessile species that are not counted (presence/absence)
#' rows (functional group is not Oyster, Non-Oyster Bivalve, or Gastropod) are passed through unchanged.
#'
#' @param df A data frame containing oyster density observations. Must include
#'   the following columns:
#'   \describe{
#'     \item{`scientific_name`}{Character. Species or taxon name; used to
#'       determine functional group membership.}
#'     \item{`live_or_box`}{Character. categorical value of "live" or "box" distinguishing between live and dead oysters.
#'     set to NA for all non-oyster species.}
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
#'     \item{`count`}{Numeric. Raw count of a given countable species; set to `0` for backfilled
#'       rows.}
#'     \item{`density_m2`}{Numeric. density of countable species per square metre; set
#'       to `0` for backfilled rows.}
#'   }
#'
#' @return A data frame with the same columns as `df`, sorted by
#'   `sample_event_id`, year, `site_name`, `transect`, `quadrat`, and
#'   `scientific_name`. Backfilled rows have `count = 0` and
#'   `density_m2 = 0`; all other columns for backfilled rows are filled
#'   with the single value observed for that sample event, or `NA` with a
#'   `message()` if multiple values are present.
#'
#' @details
#' Functional group assignment is performed via
#' [utl_mg_assign_functional_groups()] with `fg in c("Oysters, Non-Oyster Bivalves, gastropods")`. Rows whose
#' `scientific_name` resolves to a group other than those listed (including
#' unknowns) are collected in a separate data frame and
#' re-appended to the output without modification.
#'
#' Within each sample event the function:
#' \enumerate{
#'   \item Uses [tidyr::expand()] with [tidyr::nesting()] to produce all
#'     combinations of `transect` × `quadrat` × `scientific_name` within the
#'     event's existing transect–quadrat pairs.
#'   \item Uses [dplyr::anti_join()] to identify combinations absent from the
#'     original data and inserts them with `count = 0` and
#'     `density_m2 = 0`.
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
#' # oyster_density_example is a built-in package dataset
#' backfilled <- utl_oyster_backfill_density(oyster_density_example)
#' nrow(backfilled) >= nrow(oyster_density_example)  # TRUE
#' }

utl_oyster_backfill_density <- function(df) {
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
    "count",
    "density_m2",
    "live_or_box",
    "density_methodology"
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

  # ---------- Assign functional groups to the data ---------------------------
  df <- df |>
    dplyr::mutate(
      functional_group = utl_mg_assign_functional_groups(
        fg_tree = "oyster_density",
        fg_labels = c("Oysters", "Non-oyster bivalves", "Gastropods"),
        scientific_names = scientific_name
      )
    )

  if (all(is.na(df$functional_group))) {
    message(
      "No Oyster, Non-Oyster Bivalve, or Gastropod rows found. Returning input unchanged."
    )
    return(df)
  }

  #--------------- Backfill by sample event ------------------
  sample_events <- unique(df$sample_event_id)

  df_out <- lapply(sample_events, function(i) {
    df_se <- df |>
      dplyr::filter(sample_event_id == i)

    # Resolve per-event metadata fields; warn if ambiguous
    quadrat_dimension <- unique(df_se$density_quadrat_dimensions)
    input_filename <- unique(df_se$input_filename)
    density_methodology <- unique(df_se$density_methodology)

    if (length(quadrat_dimension) > 1) {
      quadrat_dimension <- NA_character_
      message("Unable to backfill quadrat dimensions for ", i)
    }

    if (length(input_filename) > 1) {
      input_filename <- NA_character_
      message("Unable to backfill input filename for ", i)
    }

    if (length(density_methodology) > 1) {
      input_filename <- NA_character_
      message("Unable to backfill density methodology for ", i)
    }

    # All transect x quadrat x scientific_name x live_or_box combinations implied by the data
    backfilled_grid <- df_se |>
      tidyr::expand(
        tidyr::nesting(
          sample_event_id,
          partner_code,
          site_code,
          site_name,
          table_id,
          sample_collection_date,
        ),
        transect,
        scientific_name,
        live_or_box
      )

    #Assign functional groups to the backfill grid
    backfilled_grid <- backfilled_grid |>
      dplyr::mutate(
        functional_group = utl_mg_assign_functional_groups(
          fg_tree = "oyster_density",
          fg_labels = c("Oysters", "Non-oyster bivalves", "Gastropods"),
          scientific_names = scientific_name
        )
      )

    # get new oyster rows for the sampling event
    grid_oysters <- backfilled_grid |>
      dplyr::filter(functional_group == "Oysters" & !is.na(live_or_box)) %>%
      dplyr::left_join(
        df_se |>
          dplyr::distinct(sample_event_id, transect, quadrat),
        by = c("sample_event_id", "transect")
      )

    df_se_oysters <- df_se |>
      dplyr::filter(functional_group == "Oysters")

    # New rows: combinations present in the grid but absent from the original
    new_rows_oysters <- dplyr::anti_join(
      grid_oysters,
      df_se_oysters,
      by = dplyr::join_by(
        sample_event_id,
        partner_code,
        site_code,
        site_name,
        table_id,
        sample_collection_date,
        transect,
        scientific_name,
        live_or_box
      )
    ) |>
      dplyr::mutate(
        density_quadrat_dimensions = quadrat_dimension,
        input_filename = input_filename,
        density_methodology = density_methodology,
        count = 0,
        density_m2 = 0
      )

    # get new countable non-oyster bivalve rows for the sampling event.
    grid_countable_nonoysters <- backfilled_grid |>
      dplyr::filter(
        functional_group %in%
          c("Non-oyster bivalves", "Gastropods") &
          is.na(live_or_box)
      ) |>
      dplyr::left_join(
        df_se |>
          dplyr::distinct(sample_event_id, transect, quadrat),
        by = c("sample_event_id", "transect")
      )

    df_se_countable_nonoysters <- df_se |>
      dplyr::filter(
        functional_group %in% c("Non-oyster bivalves", "Gastropods")
      )

    new_rows_nonoysters <- dplyr::anti_join(
      grid_countable_nonoysters,
      df_se_countable_nonoysters,
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
        density_methodology = density_methodology,
        count = 0,
        density_m2 = 0
      )

    ### Join all new rows with the original
    new_rows <- dplyr::bind_rows(df_se, new_rows_nonoysters, new_rows_oysters)
  }) |>
    dplyr::bind_rows() |>
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

#' Resolve a field to its single value within a sample event
#'
#' @description
#' Internal helper for the backfill functions. Returns the one unique value of
#' `x`, or a typed `NA` plus a `message()` when `x` holds more than one value —
#' the backfilled rows then carry `NA` for that field rather than an arbitrary
#' pick.
#'
#' @param x Vector of values observed for one field within one grouping unit.
#' @param field Character scalar naming the field, used in the message.
#' @param context Character scalar identifying the grouping unit (e.g. a
#'   `sample_event_id`), used in the message.
#'
#' @return A length-1 vector of the same type as `x`.
#'
#' @keywords internal
#' @noRd
.mg_single_value <- function(x, field, context) {
  values <- unique(x)
  if (length(values) > 1) {
    message("Unable to backfill ", field, " for ", context)
    return(values[NA_integer_])
  }
  values
}


#' Backfill fouling panel cover data with zero-cover absence rows
#'
#' @description
#' Accepts a fouling panel cover data frame and adds new rows to ensure that
#' every taxon enrolled in a primary fouling group anywhere within a sample
#' event is represented on every panel image observed anywhere within that
#' event — that is, on every `deployment_period` × `panel_id` combination the
#' event actually contains, regardless of which panel or period the taxon was
#' originally recorded on.
#' The `percent_cover` for backfilled rows is set to `0`. Taxa that resolve to
#' no primary fouling group (e.g. `"biofilm"`, `"shadow"`, `"zip tie"`) are
#' passed through unchanged.
#'
#' @param df A data frame containing fouling cover observations. Must include
#'   the following columns:
#'   \describe{
#'     \item{`sample_event_id`}{Character. Unique identifier for each sampling
#'       event; used to group observations before expanding.}
#'     \item{`partner_code`}{Character. MarineGEO partner identifier.}
#'     \item{`site_code`}{Character. Machine-readable site identifier (e.g.,
#'       `"QDL-003"`).}
#'     \item{`site_name`}{Character. Site name.}
#'     \item{`table_id`}{Character. Versioned identifier for the source data
#'       table; links to the MarineGEO data index.}
#'     \item{`panel_id`}{Character. Settlement panel identifier within a sample
#'       event.}
#'     \item{`deployment_date`}{Date. Date the panels were deployed.}
#'     \item{`retrieval_date`}{Date. Date the panels were retrieved and
#'       photographed; one value per `deployment_period` within an event.}
#'     \item{`deployment_period`}{Character. Deployment time of the panel image
#'       (e.g., `"30 day"`, `"60 day"`, `"90 day"`).}
#'     \item{`scientific_name`}{Character. Species or taxon name; used to
#'       determine fouling group membership.}
#'     \item{`point_count`}{Numeric. Points landing on the taxon; set to `0` for
#'       backfilled rows whose `points_in_grid` is known.}
#'     \item{`points_in_grid`}{Numeric. Total points in the scoring grid.}
#'     \item{`percent_cover`}{Numeric. Percent cover value; set to `0` for
#'       backfilled rows.}
#'     \item{`photo_filename`}{Character. Panel photograph file name.}
#'     \item{`habitat`}{Character. Habitat the panels were deployed in;
#'       constant within a sample event.}
#'     \item{`input_filename`}{Character. Source file name.}
#'   }
#'
#' @return A data frame with the same columns as `df`, sorted by
#'   `sample_event_id`, year, `site_code`, `site_name`, `deployment_period`,
#'   `panel_id`, and `scientific_name`. Backfilled rows have
#'   `percent_cover = 0` and `point_count = 0` (or `NA` where the panel image
#'   has no `points_in_grid`); `photo_filename` and `points_in_grid` are
#'   inherited from the panel image, `retrieval_date` from the deployment
#'   period, and `habitat` together with the remaining required columns from
#'   the sample event. Columns not used by the expansion —
#'   including `identification_notes`, `percent_cover_notes`, `row_uuid`, and
#'   any extra column the caller supplied — are left `NA` on backfilled rows.
#'   `row_uuid` is generated upstream of this function, so re-run
#'   [utl_mg_generate_row_uuid()] afterwards if the backfilled rows need
#'   identifiers.
#'
#' @details
#' Group membership is resolved with [utl_mg_assign_ancestor_labels()] against
#' the `"fouling"` tree with `type = "primary"`, which returns whichever primary
#' group a name falls under without the candidate labels being enumerated here.
#' Rows whose `scientific_name` resolves to no primary group are collected in a
#' separate data frame and re-appended to the output without modification. Note
#' that `"open space"` and `"sediment"` *are* primary fouling groups and are
#' therefore backfilled.
#'
#' Within each sample event the function:
#' \enumerate{
#'   \item Builds a panel image inventory: the distinct `deployment_period` ×
#'     `panel_id` pairs that were actually observed anywhere in the event —
#'     including pairs represented only by ungrouped taxa — each carrying its
#'     `photo_filename` and `points_in_grid`. Pairs absent from the data are
#'     never created — a panel retrieved at 30 and 60 days but lost before 90
#'     gains no 90-day rows.
#'   \item Crosses that inventory with every grouped taxon observed anywhere in
#'     the event — on any panel, in any deployment period — using
#'     [tidyr::expand_grid()]. A taxon seen only on panel A therefore reaches
#'     every deployment period of panel B that panel B actually has.
#'   \item Uses [dplyr::anti_join()] to identify combinations absent from the
#'     original data and inserts them with `percent_cover = 0`.
#' }
#'
#' Fields are inherited at the level they are actually fixed at, rather than
#' all from the panel image:
#' \itemize{
#'   \item Event level (`partner_code`, `site_code`, `site_name`, `table_id`,
#'     `deployment_date`, `habitat`, `input_filename`) — the site the panels
#'     hung at and the paperwork describing the deployment.
#'   \item Deployment period level (`retrieval_date`) — one retrieval covers
#'     every panel pulled at that deployment time, so a 30/60/90-day event carries
#'     three retrieval dates rather than one.
#'   \item Panel image level (`photo_filename`, `points_in_grid`) — the two
#'     fields that describe the photograph itself.
#' }
#'
#' If a field is not unique within the level it is resolved at, the backfilled
#' rows receive `NA` for that field and a `message()` is emitted.
#'
#' @seealso [utl_sav_backfill_cover()] for the seagrass transect × quadrat
#'   equivalent.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' backfilled <- utl_fouling_backfill_cover(fouling_cover)
#' nrow(backfilled) >= nrow(fouling_cover) # TRUE
#' }
utl_fouling_backfill_cover <- function(df) {
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }

  # Fields constant across a whole sample event:
  event_fields <- c(
    "partner_code",
    "site_code",
    "site_name",
    "table_id",
    "deployment_date",
    "habitat",
    "input_filename"
  )

  # Fields set by the retrieval
  deployment_period_fields <- c("retrieval_date")

  # Fields that genuinely describe the photograph of one panel: the file itself and the scoring grid laid over it.
  panel_image_fields <- c(
    "photo_filename",
    "points_in_grid"
  )

  required_cols <- c(
    "sample_event_id",
    "panel_id",
    "deployment_period",
    "scientific_name",
    "point_count",
    "percent_cover",
    event_fields,
    deployment_period_fields,
    panel_image_fields
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

  # --- Assign primary fouling groups ------------------------------------------
  df <- df |>
    dplyr::mutate(
      .fouling_group = utl_mg_assign_ancestor_labels(
        fg_tree = "fouling",
        scientific_names = scientific_name,
        type = "primary"
      )
    )

  df_ungrouped <- df |>
    dplyr::filter(is.na(.fouling_group))

  df_grouped <- df |>
    dplyr::filter(!is.na(.fouling_group))

  if (nrow(df_grouped) == 0) {
    message(
      "No rows enrolled in a primary fouling group. Returning input unchanged."
    )
    return(dplyr::select(df, -".fouling_group"))
  }

  # `point_count` is an INT column in the table contract; keep it one.
  point_count_is_integer <- is.integer(df$point_count)

  # --- Backfill by sample event -----------------------------------------------
  sample_events <- unique(df_grouped$sample_event_id)

  df_out <- lapply(sample_events, function(i) {
    # Every row in the event, including rows whose taxon is enrolled in no
    # primary group: a panel image scored only as "biofilm" or "shadow" is
    # still a panel image that was photographed, so it belongs in the
    # inventory below and must receive zero-cover rows.
    df_se_all <- df |>
      dplyr::filter(sample_event_id == i)

    df_se <- df_grouped |>
      dplyr::filter(sample_event_id == i)

    # Resolve per-event metadata fields; warn if ambiguous
    event_values <- lapply(
      stats::setNames(event_fields, event_fields),
      function(field) .mg_single_value(df_se_all[[field]], field, i)
    )

    # The panel images actually observed in this event, each with the fields
    # that describe the photograph rather than the observation.
    panel_images <- df_se_all |>
      dplyr::group_by(deployment_period, panel_id) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(panel_image_fields),
          \(x) {
            .mg_single_value(
              x,
              dplyr::cur_column(),
              paste(i, panel_id[1], deployment_period[1], sep = " / ")
            )
          }
        ),
        .groups = "drop"
      )

    # Fields fixed by the retrieval of a given panel
    deployment_period_values <- df_se_all |>
      dplyr::group_by(deployment_period) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(deployment_period_fields),
          \(x) {
            .mg_single_value(
              x,
              dplyr::cur_column(),
              paste(i, deployment_period[1], sep = " / ")
            )
          }
        ),
        .groups = "drop"
      )

    # Every grouped taxon seen anywhere in the event, at every panel image
    backfilled_grid <- tidyr::expand_grid(
      panel_images,
      scientific_name = unique(df_se$scientific_name)
    ) |>
      dplyr::left_join(
        deployment_period_values,
        by = dplyr::join_by(deployment_period)
      ) |>
      dplyr::mutate(
        sample_event_id = i,
        partner_code = event_values$partner_code,
        site_code = event_values$site_code,
        site_name = event_values$site_name,
        table_id = event_values$table_id,
        deployment_date = event_values$deployment_date,
        habitat = event_values$habitat,
        input_filename = event_values$input_filename
      )

    # New rows: combinations present in the grid but absent from the original
    new_rows <- dplyr::anti_join(
      backfilled_grid,
      df_se_all,
      by = dplyr::join_by(
        sample_event_id,
        deployment_period,
        panel_id,
        scientific_name
      )
    ) |>
      dplyr::mutate(
        percent_cover = 0,
        point_count = dplyr::if_else(is.na(points_in_grid), NA_real_, 0)
      )

    if (point_count_is_integer) {
      new_rows <- new_rows |>
        dplyr::mutate(point_count = as.integer(point_count))
    }

    dplyr::bind_rows(df_se, new_rows)
  }) |>
    dplyr::bind_rows() |>
    dplyr::bind_rows(df_ungrouped) |>
    dplyr::select(-".fouling_group") |>
    dplyr::arrange(
      sample_event_id,
      lubridate::year(deployment_date),
      site_code,
      site_name,
      deployment_period,
      panel_id,
      scientific_name
    )

  df_out
}
