#' Load MarineGEO Excel data into R
#'
#' Load one or more sheets from a MarineGEO Excel file into R.The `output_table`
#' argument is used to select the correct sheet from the Excel file.
#'
#' @param filepath
#' @param output_table
#'
#' @returns a dataframe
#' @export
#'
#' @examples
utl_mg_load_excel <- function(filepath, output_table){

  # Check that the output table matches a table_id in the data structure

  # Check that the file exists at filepath

  tryCatch({

    ## Oyster Network Project 2025 ####

    #### Deployment Reef Metadata ####
    if(output_table == "oyster-2025-reef-metadata-deployment") {

      target_sheet <- "REEF METADATA"
      df_raw <- readxl::read_excel(filepath, sheet = target_sheet)

      # Rename some columns to match MarineGEO column standards
      df <- df_raw %>%
        rename(
          water_present = `Water Present Y N`,
          logger_deployed = `Logger Deployed Y N`,
          spat_stick = `Spat Stick PVC or biobox`,
          personnel = `Sampling Personnel`,
          notes = `Site Notes (including recent perturbations and weather conditions)`
        ) %>%
        mutate(input_filename = basename(filepath))

      #### Deployment Rugosity and Cluster Height ####
    } else if(output_table == "oyster-2025-rugosity") {

      target_sheet <- "RUGOSITY & CLUSTER HEIGHT"
      df_raw <- readxl::read_excel(filepath, sheet = target_sheet)

      # Rename some columns to match MarineGEO column standards
      df <- df_raw %>%
        mutate(input_filename = basename(filepath))

      ## Reef Life Survey ####

      #### Standard RLS Template ####
    } else if(output_table == "reef-life-survey-data-marinegeo-v1") {

      df_raw <- readxl::read_excel(filepath, sheet = "DATA")

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
          site_code = `Site No.`,
          photoquadrats = `P-Qs`
        ) |>
        # Input filename is metadata used to track file curation
        dplyr::mutate(input_filename = basename(filepath)) |>
        dplyr::mutate(Method = as.numeric(Method),
                      Block = as.numeric(Block)) |>
        marinegeo.utils::utl_join_taxonomy_by_scientific_name(identification_column_name = "Species",
                                                              taxonomic_levels = "phylum") |>
        dplyr::relocate(taxonomic_id, .after = "Species")


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
