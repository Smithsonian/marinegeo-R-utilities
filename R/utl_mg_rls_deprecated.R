#' Load MarineGEO Excel data into R
#'
#' Load one or more sheets from a MarineGEO Excel file into R.The `output_table`
#' argument is used to select the correct sheet from the Excel file.
#'
#' @param filepath the path to the rls file being loaded
#' @param output_table the name of the intended L2 final product
#' @param sheet_name the name of the sheet int he excel file where the data can be found.
#'
#' @returns a dataframe
#' @export
#'
#' @examples
#'
#' utl_mg_load_excel_deprecated('L1/rls_example.xlsx',"reef-life-survey-data-marinegeo-v1", 'DATA')
utl_mg_load_excel_deprecated <- function(filepath, output_table, sheet_name){

  # Check that the output table matches a table_id in the data structure

  # Check that the file exists at filepath

  tryCatch({

    if(output_table == "reef-life-survey-data-marinegeo-v1") {

      df_raw <- readxl::read_excel(filepath, sheet = sheet_name)

      # Check for necessary columns to process dataframe
      missing_columns <- dplyr::setdiff(c("Total", "Method", "Site No.", "P-Qs"), colnames(df_raw))

      if (length(missing_columns) > 0) {
        message(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
        return(NULL)
      }

      # Filter out header rows, files have either 1 or 2 rows
      # Total, Inverts, and size class columns should remain as numeric data types
      df <- df_raw |>
        dplyr::filter(Method != "0, 1, 2")

      if(nrow(df) == 0){
        warning("Dataframe has 0 rows after dropping header rows")
      }

      # Calculate the number of rows that were dropped by filter()
      dropped_rows <- nrow(df_raw) - nrow(df)

      if(dropped_rows == 1){
        print("1 header row dropped")
      } else if(dropped_rows == 2){
        print("2 header rows dropped")
      } else {
        print("Unexpected number of rows dropped")
      }

      # This section removes the many blank rows from the bottom of the datasheet
      # rle() identifies runs of consecutive values within a vector
      # In rle() output:
      # The final item in the values vector should be TRUE (evaluated as Total == 0)
      # and the final item in the lengths vector represents the number of rows from the bottom that should be dropped
      runs <- rle(df$Total == 0)

      if(dplyr::last(runs$values) & length(runs$lengths) == 1){

        warning("Dataframe only has rows with a Total equal to 0")

      } else if(dplyr::last(runs$values)){

        # last row to keep
        last_row <- nrow(df) - dplyr::last(runs$lengths)

        df <- df[1:last_row,]

        print(paste0("dropped ", as.character(dplyr::last(runs$lengths)), " rows containing 0 total count from the end of the datasheet"))

      } else {
        print("No rows were dropped from the end of the datasheet")

      }

      # Rename some columns to match MarineGEO column standards
      df <- df |>
        dplyr::rename(
          site_code = any_of("Site No."),
          photoquadrats = any_of("P-Qs")
        ) |>
        # Input filename is metadata used to track file curation
        dplyr::mutate(input_filename = basename(filepath)) |>
        dplyr::mutate(Method = as.numeric(Method),
                      Block = as.numeric(Block),
                      Time = as.character(Time)) |>
        dplyr::mutate(Time = dplyr::case_when(
          stringr::str_starts(Time, "1899-12-31") ~ trimws(
            gsub("T", "",
                 gsub("Z", "",
                      gsub("1899-12-31", "", Time)
                 ))
          ),
          T ~ Time
        )) |>
        marinegeo.utils::utl_join_taxonomy_by_scientific_name_deprecated(identification_column_name = "Species",
                                                                         taxonomic_levels = "phylum") |>
        dplyr::relocate(taxonomic_id, .after = "Species")


    } else {

      message(paste("Target table is not defined in utl_mg_load_excel_deprecated(): ", output_table))
      return(NULL)

    }

    ## Convert to lower case and remove spaces ####

    # Convert to lower case and remove spaces
    colnames(df) <- tolower(gsub(" ", "_", colnames(df)))

    # Check for unexpected columns
    # missing_columns <- dplyr::setdiff(c("Total", "Method", "Site No.", "P-Qs"), colnames(df_raw))
    #
    # if (length(missing_columns) > 0) {
    #   message(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
    #   return(NULL)
    # }

    if(nrow(df) == 0){
      warning("Dataframe has 0 rows")
    }

    return(df)

  }, error = function(e) {
    message(paste("Error reading Excel file:", e$message))
    return(NULL)
  })
}





#' Join taxonomic classification to input dataframe
#'
#'@description
#' The function calls `utl_get_taxonomy_by_scientific_name_deprecated()` to query the MarineGEO taxonomic
#' database by scientific name. If the sample is identified by a column not named "scientific_name",
#' it should be defined in the `identification_column_name` argument.
#'
#' @param df A data frame to join taxonomic levels to
#' @param identification_column_name Defaults to "scientific_name"
#' @param taxonomic_levels Vector of classification levels. By default, returns all options
#'
#' @returns A dataframe with taxonomic levels as columns
#' @export
#' @examples
#' df <- utl_rls_load_excel_deprecated("inst/extdata/test_rls_data_EPA.xlsx", sheet = "DATA") |>
#'  dplyr::filter(Method == "1" | Method == "2")
#' utl_join_taxonomy_by_scientific_name_deprecated(df, identification_column_name = "Species")
#' utl_join_taxonomy_by_scientific_name+_deprecated(df, identification_column_name = "Species", "phylum")
utl_join_taxonomy_by_scientific_name_deprecated <- function(df,
                                                            identification_column_name = "scientific_name",
                                                            taxonomic_levels = NULL){

  # Test inputs
  stopifnot("`df` must be a dataframe" = is.data.frame(df))
  stopifnot("`identification_column_name` must be a character" = is.character(identification_column_name))

  # Check for necessary columns to process dataframe
  missing_columns <- dplyr::setdiff(identification_column_name, colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required column(s):", paste(missing_columns, collapse = ", ")))
  }

  tryCatch({

    # If necessary, rename column to allow join with taxonomic database
    if(identification_column_name != "scientific_name"){

      df <- df |>
        dplyr::rename(scientific_name := dplyr::all_of(identification_column_name))

    }

    scientific_names <- df |>
      dplyr::select(scientific_name) |>
      dplyr::distinct() |>
      dplyr::pull(scientific_name)

    taxonomic_lookup_df <- marinegeo.utils::db_get_taxonomy_by_scientific_name_deprecated(scientific_names, taxonomic_levels)

    df_with_taxonomy <- df |>
      dplyr::left_join(taxonomic_lookup_df, by = "scientific_name")

    # If necessary, rename column to original
    if(identification_column_name != "scientific_name"){

      df_with_taxonomy <- df_with_taxonomy |>
        dplyr::rename_with(~identification_column_name, matches("scientific_name"))

    }

    return(df_with_taxonomy)

  }, error = function(e){

    cat("Error in utl_join_taxonomy_by_scientific_name_deprecated:", e$message, "\n")
    return(NULL)

  })

}




#' Get taxonomic classifications by scientific name
#'
#' @param scientific_names Vector of scientific names to get taxonomic classifications for
#' @param taxonomic_levels Vector of classification levels. By default, returns all available levels
#'
#' @returns dataframe of taxonomic classifications and taxonomic ID (usually Aphia ID)
#' @export
#' @examples
#' utl_get_taxonomy_by_scientific_name(c("Gastropoda", "Dardanus venosus"))
#' utl_get_taxonomy_by_scientific_name("Dardanus venosus", taxonomic_levels = "phylum")
#' utl_get_taxonomy_by_scientific_name("Dardanus venosus", taxonomic_levels = "pilum") # misspelling

db_get_taxonomy_by_scientific_name_deprecated <- function(scientific_names,
                                                          taxonomic_levels = NULL){

  # Test inputs
  stopifnot("`scientific_names` must be a character vector" = is.character(scientific_names))

  tryCatch({

    taxonomy_subset <- marinegeo_metadata$legacy_taxonomic_ids |>
      dplyr::filter(scientific_name %in% scientific_names) |>
      dplyr::left_join(marinegeo_metadata$legacy_taxonomic_classifications, by = "taxonomic_id")

    no_matches <- scientific_names[!scientific_names %in% taxonomy_subset$scientific_name]

    if(nrow(taxonomy_subset) == 0){

      warning("There are no matches in the taxonomic database")

    } else if(length(no_matches) > 1){

      warning(
        length(no_matches), " scientific names could not be matched in taxonomic database"
      )

    }

    if(!is.null(taxonomic_levels)){

      # Check for necessary taxonomic columns
      missing_columns <- dplyr::setdiff(taxonomic_levels, colnames(taxonomy_subset))

      if(length(missing_columns) > 0) {

        allowed_columns <- tibble::tibble(column_names = colnames(taxonomy_subset)) |>
          dplyr::filter(!column_names %in% c("taxonomic_id", "scientific_name", "level")) |>
          dplyr::distinct() |>
          dplyr::pull(column_names)

        warning(paste("The following taxonomic level(s) is not defined in the database table:", paste(missing_columns, collapse = ", ")))

        message("The following taxonomic levels are accepted: ", paste(allowed_columns, collapse = ", "))

      }

      taxonomy_level_subset <- taxonomy_subset |>
        dplyr::select(
          dplyr::any_of(c("scientific_name", "taxonomic_id", taxonomic_levels))
        )

      return(taxonomy_level_subset)

    } else return(taxonomy_subset)

  }, error = function(e){

    cat("Error in db_get_taxonomy_by_scientific_name_deprecated():", e$message, "\n")
    return(NULL)

  })
}


#' Filling missing metadata columns based on other processed RLS data
#'
#' For columns "vis", "direction", "time", and "photoquadrats", the function
#' converts "BLANK" to NA and checks processed L2 data for fill values from
#' the same survey.
#'
#' @param df the data frame to check for missing metadata.
#'
#' @returns dataframe with all metadata
#' @export
#'
#'
#'
#'
utl_rls_fill_missing_metadata_deprecated <- function(df){

  columns <- c("sample_event_id", "vis", "direction", "time", "photoquadrats")

  ids <- unique(df$sample_event_id)

  df_in <- df %>%
    mutate(vis = case_when(
      vis == "BLANK" ~ NA,
      T ~ vis
    )) %>%
    mutate(direction = case_when(
      direction == "BLANK" ~ NA,
      T ~ direction
    )) %>%
    mutate(photoquadrats = case_when(
      photoquadrats == "BLANK" ~ NA,
      T ~ photoquadrats
    ))

  if(is.character(df_in$time)){
    df_in <- df_in %>%
      mutate(time = case_when(
        time == "BLANK" ~ NA,
        T ~ time
      ))
  }

  fill_options <- marinegeo.utils::db_marinegeo_L2("reef-life-survey-data-marinegeo-v1") %>%
    filter(sample_event_id %in% ids) %>%
    select(all_of(columns)) %>%
    distinct() %>%
    mutate(vis = case_when(
      vis == "BLANK" ~ NA_character_,
      T ~ vis
    )) %>%
    mutate(direction = case_when(
      direction == "BLANK" ~ NA_character_,
      T ~ direction
    )) %>%
    mutate(photoquadrats = case_when(
      photoquadrats == "BLANK" ~ NA_character_,
      T ~ photoquadrats
    )) %>%
    mutate(time = case_when(
      time == "BLANK" ~ NA_character_,
      T ~ time
    )) %>%
    collect() %>%
    group_by(sample_event_id) %>%
    summarize(vis = first(vis, na_rm = T),
              direction = first(direction, na_rm = T),
              photoquadrats = first(photoquadrats, na_rm = T),
              time = first(time, na_rm = T))


  if(is.numeric(df_in$vis)){
    df_in <- df_in %>%
      mutate(vis = as.character(vis))
  }

  if(!is.character(df_in$time)){
    df_in <- df_in %>%
      mutate(time = as.character(time))
  }

  df_out <- df_in %>%
    rows_patch(fill_options, by = "sample_event_id")

  return(df_out)

}



