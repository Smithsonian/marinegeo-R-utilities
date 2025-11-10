#' Backfill seagrass cover data
#' Ensures absence (cover = 0) row present for each species observed at a site
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
utl_sav_backfill_cover <- function(df){

  sample_events <- unique(df$sample_event_id)

  df_out <- lapply(sample_events, function(i){

    df_se <- df |>
      dplyr::filter(sample_event_id == i)

    cover_method <- unique(df_se$cover_method)
    quadrat_dimension <- unique(df_se$cover_quadrat_dimensions)
    input_filename <- unique(df_se$input_filename)

    if(length(cover_method) > 1){
      cover_method <- NA
      message(paste0("Unable to backfill cover method for ", i))
    }

    if(length(quadrat_dimension) > 1){
      quadrat_dimension <- NA
      message(paste0("Unable to backfill quadrat dimensions for ", i))
    }

    if(length(input_filename) > 1){
      input_filename <- NA
      message(paste0("Unable to backfill input filename for ", i))
    }

    backfilled_df <- df_se |>
      tidyr::expand(tidyr::nesting(sample_event_id, partner_code, site_name, sample_collection_date, transect), quadrat, tidyr::nesting(scientific_name, taxonomic_id))

    df_out <- dplyr::bind_rows(
      df_se,
      dplyr::anti_join(backfilled_df, df_se,
                       by = dplyr::join_by(sample_event_id, partner_code, site_name, sample_collection_date, transect,
                                           quadrat, scientific_name, taxonomic_id)) |>
        dplyr::mutate(cover_method = !!cover_method,
                      cover_quadrat_dimensions = quadrat_dimension,
                      input_filename = !!input_filename,
                      percent_cover = 0,
                      cover_code = 0)
    ) |>
      dplyr::filter(taxonomic_id != 0)

  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(sample_event_id, lubridate::year(sample_collection_date), site_name, transect, quadrat, scientific_name)


  return(df_out)
}
