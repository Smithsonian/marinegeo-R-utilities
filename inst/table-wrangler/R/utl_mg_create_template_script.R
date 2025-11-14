
create_template_script <- function(script_filepath, target_table, input_filepath, excel_sheet = NULL){
 
  # Process input filepath
  first_slash <- regexpr("/", input_filepath)
  
  # Extract everything after the first "/"
  local_data_filepath_in <- substr(input_filepath, first_slash + 1, nchar(input_filepath))
  
  local_data_filepath_out <- assemble_output_filepath(input_filepath, target_table)
  
  # Script defaults ####
  default_introduction <- c(
    "## How to use this script ####",
    "# Code entered in the 'MarineGEO Table Wrangler Start' section can be ",
    "# run in the Table Wrangler application to assist processing data.",
    "# The application can create code that you can copy and paste into this script.",
    "# IMPORTANT: Any objects used in this script between the 'Start' and 'End' section",
    "# must also be available in the application or else it will crash:",
    "#   - Functions used in this section must be sourced from the `tidyverse` or `marinegeo.utils` packages,",
    "#     or from `base` R packages. ",
    "#   - `df` is the name of the dataframe used in the app and by default here, ",
    "#     do not change the name of the dataframe that feeds into the 'Start' section. ",
    "#   - `df_out` is the name of the dataframe created by the code in the application,",
    "#     do not change name of the `df_out` dataframe in this script.",
    "#   - You do not need to evaluate any code in the application. In that case, ",
    "#     simply keep the line `df_out <- df` and do not add any additional code.",
    "",
    "# `marinegeo.utils` is a package of functions and resources to assist",
    "# in common data management tasks. To install, run (requires `devtool` package):",
    "# devtools::install_github('https://github.com/Smithsonian/marinegeo-R-utilities')", 
    "",
    "# Use the MarineGEO Table Wrangler Shiny application to support processing",
    "# run: marinegeo.utils::shiny_launch_table_wrangler()",
    "",
    "# Load Packages",
    "library(tidyverse)",
    "library(readxl)",
    "library(marinegeo.utils)",
    "",
    paste0("input_file_path <- '", local_data_filepath_in, "'"),
    ""
  )
  
  default_output_to_directory <- c(
    "# Test Output and write data ####",
    "# The select(all_of(req_cols)) code will create an error if required columns are not present",
    "# It also reorders the columns to match the target table format",
    "# Both of these are very important to maintaining data workflows for the target table",
    "# and shouldn't be skipped.",
    "",
    "df_out %>%",
    "  select(all_of(req_cols)) %>%",
    "  marinegeo.utils::qc_mg_column_data_types(table_out) %>%",
    paste0("\twrite_csv('", local_data_filepath_out, "')")
  )
  
  # Assemble Scripts ####
  ## RLS ####
  if(target_table == "reef-life-survey-data-marinegeo-v1"){
    
    script_template <- c(
      
      "# Process Reef Life Survey Excel data",
      "# Steps include",
      "#   - remove empty rows",
      "#   - assign Aphia IDs and resolve taxonomic issues",
      "#   - correct inconsistant metadata",
      "#   - check for suspicious invert and fish counts",
      "",
      
      default_introduction,
      
      "## Destination table metadata",
      "table_out <- 'reef-life-survey-data-marinegeo-v1'",
      "req_cols <- marinegeo.utils::utl_mg_column_order(table_out)",
      "",
      "# Load data",
      "df <- marinegeo.utils::utl_mg_load_excel(input_file_path, table_out, 'DATA')",
      "",
      
      "## MarineGEO Table Wrangler Start ####",
      "df_out <- df # %>%",
      "",
      "",
      "",
      
      "## MarineGEO Table Wrangler End ##",
      "",
      
      default_output_to_directory
    ) 
    
    ## Seagrass Monitoring ####
  } else if(target_table %in% c("seagrass-cover-monitoring-v1", "seagrass-macroinvertebrates-monitoring-v1", 
                                "shoot-count-monitoring-v1", "seagrass-metadata-monitoring-v1",
                                "seagrass-leaf-monitoring-v1", "sheath-and-epibiont-monitoring-v1")){
    
    sheet_name <- switch(target_table,
                         "seagrass-cover-monitoring-v1" = "COVER",
                         "seagrass-macroinvertebrates-monitoring-v1" = "MACROINVERTS",
                         "shoot-count-monitoring-v1" = "DENSITY",
                         "seagrass-metadata-monitoring-v1" = "TRANSECTS",
                         "seagrass-leaf-monitoring-v1" = "LEAF MEASUREMENTS",
                         "sheath-and-epibiont-monitoring-v1" = "SHEATH AND EPIBIONTS")
    
    if(sheet_name == "COVER"){
      processing_string <- c(
        "df_out <- df %>%",
        "  mutate(partner_code = \"\",",
        "         sample_event_id = gsub(\" \", \"-\", paste(partner_code, site_name, year, sep = \"_\")),",
        "         sample_collection_date = ymd(paste(year, month, day, sep = \"-\"))) %>%",
        "  rename(cover_quadrat_dimensions = quadrat_dimensions)",
        "  #marinegeo.utils::utl_sav_backfill_cover()"
      )
    } else if(sheet_name == "DENSITY"){
      processing_string <- c(
        "df_out <- df %>%",
        "  mutate(partner_code = \"\",",
        "         sample_event_id = gsub(\" \", \"-\", paste(partner_code, site_name, year, sep = \"_\")),",
        "         sample_collection_date = ymd(paste(year, month, day, sep = \"-\"))) %>%",
        "  rename(density_quadrat_dimensions = quadrat_dimensions) %>%",
        "  mutate(shoot_density_m2 = case_when(",
        "            density_quadrat_dimensions == \"10x10cm\" ~ as.numeric(shoot_count) * 100,",
        "            density_quadrat_dimensions == \"25x25cm\" ~ as.numeric(shoot_count) * 16,",
        "            density_quadrat_dimensions == \"50x50cm\" ~ as.numeric(shoot_count) * 4,",
        "            density_quadrat_dimensions == \"75x75cm\" ~ as.numeric(shoot_count) * 1.78,",
        "            T ~ F)) %>%",
        "  mutate(flowers_present = case_when(",
        "            flowers_p_a == \"A\" ~ F,",
        "            flowers_p_a == \"P\" ~ T,",
        "            T ~ NA))"
      )
    } else if(sheet_name == "SHEATH AND EPIBIONTS"){
      processing_string <- c(
        "df_1 <- df %>%",
        "  mutate(partner_code = \"\",",
        "         sample_event_id = gsub(\" \", \"-\", paste(partner_code, site_name, year, sep = \"_\")),",
        "         sample_collection_date = ymd(paste(year, month, day, sep = \"-\"))) %>%",
        "  mutate(grazing_scars_present = case_when(",
        "            grazing_scars_present == \"A\" ~ F,",
        "            grazing_scars_present == \"P\" ~ T,",
        "            T ~ NA))",
        "",
        "sample_events <- unique(df_1$sample_event_id)",
        "",
        "leaves_df <- marinegeo.utils::db_marinegeo_L2(\"seagrass-leaf-monitoring-v1\") %>%",
        "  filter(sample_event_id  %in% sample_events) %>%",
        "  group_by(site_name, transect, quadrat, scientific_name, taxonomic_id) %>%",
        "  collect() %>%",
        "  summarize(median_leaf_length_mm = median(leaf_length_mm, na.rm = T))",
        "",
        "df_out <- df_1 %>%",
        "  left_join(leaves_df) %>%",
        "  mutate(shoot_length_mm = sheath_length_mm + median_leaf_length_mm)"
        
      )
    } else if(sheet_name == "TRANSECTS") {
      
      processing_string <- c(
        "df_1 <- df %>%",
        "  mutate(partner_code = \"\")",
        "",
        "sites <- unique(df_1$site_name)",
        "",
        "sample_events_df <- marinegeo.utils::db_marinegeo_L2(\"seagrass-cover-monitoring-v1\") %>%",
        "  filter(site_name %in% sites) %>%",
        "  select(sample_event_id, site_name, sample_collection_date) %>%",
        "  distinct() %>%",
        "  collect() # %>%",
        "#  filter(year(sample_collection_date) %in% c())",
        "",
        "df_out <- df_1 #%>%",
        "  #left_join(sample_events_df)"
      ) 
      
    } else {
      processing_string <- c(
        "df_out <- df %>%",
        "  mutate(partner_code = \"\",",
        "         sample_event_id = gsub(\" \", \"-\", paste(partner_code, site_name, year, sep = \"_\")),",
        "         sample_collection_date = ymd(paste(year, month, day, sep = \"-\")))"
      )
    }
    
    script_template <- c(
      
      "# Process Seagrass Monitoring Excel data",
      "",
      
      default_introduction,
      
      "",
      "## Destination table metadata",
      paste0("table_out <- '", target_table, "'"),
      "req_cols <- marinegeo.utils::utl_mg_column_order(table_out)",
      "",
      "# Load data",
      paste0("df <- marinegeo.utils::utl_mg_load_excel(input_file_path, table_out, '", sheet_name,"')"),
      "",
      
      "## MarineGEO Table Wrangler Start ####",
      processing_string,
      "",
      
      "## MarineGEO Table Wrangler End ##",
      "",
      default_output_to_directory      
    ) 
    
    
    ## Oyster Network Project 2025 ####
  } else if(str_starts(target_table, "oyster-2025")){
    
    sheet_name <- switch(target_table,
                         "oyster-2025-reef-metadata-deployment" = "REEF METADATA", 
                         "oyster-2025-rugosity" = "RUGOSITY & CLUSTER HEIGHT",
                         "oyster-2025-reef-metadata-retrieval" = "REEF METADATA",
                         "oyster-2025-count" = "OYSTER & BIVALVE COUNT",
                         "oyster-2025-oyster-height" = "OYSTER HEIGHT",
                         "oyster-2025-bivalve-height" = "BIVALVE HEIGHT",
                         "oyster-2025-weight" = "BIVALVE WEIGHT",
                         "oyster-2025-mobile-fauna" = "MOBILE FAUNA",
                         "oyster-2025-sessile-fauna" = "SESSILE FAUNA")
    
    # If deployment, needs to target deployment date
    # If retrieval, needs to target retrieval date
    # Otherwise, no date columns
    if(target_table == "oyster-2025-reef-metadata-deployment"){
      date_string <-c(
        "",
        "  mutate(deployment_date = ymd(paste(deployment_year, deployment_month, deployment_day, sep = \"-\"))) %>%",
        ""
      )
      
    } else if(target_table == "oyster-2025-reef-metadata-retrieval"){
      date_string <- c(
        "",
        "  mutate(retrieval_date = ymd(paste(retrieval_year, retrieval_month, retrieval_day, sep = \"-\"))) %>%",
        ""
      )
    } else {
      date_string <- ""
    }
    
    # If a deployment file but not metadata, include code to get deployment dates
    if(str_detect(basename(input_filepath), "deployment") & target_table != "oyster-2025-reef-metadata-deployment"){
      
      processing_script <- c(
        "## MarineGEO Table Wrangler Start ####",
        "df_clean_site_names <- df %>%",
        "",
        "  mutate(partner_code = \"\") %>%",
        "",
        "# Use the code generation tools in the app to conditonally update reef codes",
        "  mutate(reef_code = \"\") %>%",
        "",
        "# Use the code generation tools in the app to conditonally update site names",
        "  mutate(site_name = reef_name) %>%",
        date_string,
        "  mutate(sample_event_id = paste(partner_code, reef_code, \"oyster-2025\", sep = \"_\"))",
        "",
        
        "deployment_dates <- marinegeo.utils::db_marinegeo_L2(\"oyster-2025-reef-metadata-deployment\") %>%",
        "  filter(sample_event_id %in% unique(df_clean_site_names$sample_event_id)) %>%",
        "  select(sample_event_id, deployment_date) %>%",
        "  collect()",
        "",
        "df_out <- df_clean_site_names %>%",
        "  left_join(deployment_dates)",
        "",

        "## MarineGEO Table Wrangler End ##"
      )
      
    } else if(str_detect(basename(input_filepath), "retrieval") & target_table != "oyster-2025-reef-metadata-retrieval"){
      
      processing_script <- c(
        "## MarineGEO Table Wrangler Start ####",
        "df_clean_site_names <- df %>%",
        "",
        "  mutate(partner_code = \"\") %>%",
        "",
        "# Use the code generation tools in the app to conditonally update reef codes",
        "  mutate(reef_code = \"\") %>%",
        "",
        "# Use the code generation tools in the app to conditonally update site names",
        "  mutate(site_name = reef_name) %>%",
        date_string,
        "  mutate(sample_event_id = paste(partner_code, reef_code, \"oyster-2025\", sep = \"_\"))",
        "",
        
        "retrieval_dates <- marinegeo.utils::db_marinegeo_L2(\"oyster-2025-reef-metadata-retrieval\") %>%",
        "  filter(sample_event_id %in% unique(df_clean_site_names$sample_event_id)) %>%",
        "  select(sample_event_id, retrieval_date) %>%",
        "  collect()",
        "",
        "df_out <- df_clean_site_names %>%",
        "  left_join(retrieval_dates)",
        "",
        
        "## MarineGEO Table Wrangler End ##"
      )
      
    } else {
      processing_script <- c(
        "## MarineGEO Table Wrangler Start ####",
        "df_out <- df %>%",
        "",
        "  mutate(partner_code = \"\") %>%",
        "",
        "# Use the code generation tools in the app to conditonally update reef codes",
        "  mutate(reef_code = \"\") %>%",
        "",
        "# Use the code generation tools in the app to conditonally update site names",
        "  mutate(site_name = reef_name) %>%",
        date_string,
        "  mutate(sample_event_id = paste(partner_code, reef_code, \"oyster-2025\", sep = \"_\"))",
        
        "",

        "## MarineGEO Table Wrangler End ##"
      )
    }
    
    script_template <- c(
      
      "# Process Oyster Network Experiment Excel data",
      "",
      
      default_introduction,
      
      "",
      "## Destination table metadata",
      paste0("table_out <- '", target_table, "'"),
      "req_cols <- marinegeo.utils::utl_mg_column_order(table_out)",
      "",
      "# Load data",
      paste0("df <- marinegeo.utils::utl_mg_load_excel(input_file_path, table_out, '", sheet_name,"')"),
      "",
      
      processing_script,
      "",
      default_output_to_directory      
    ) 
    
  }
  
  writeLines(script_template, script_filepath)
  
}

assemble_output_filepath <- function(input_filepath, target_table){
 
  output_filename_end <- switch(target_table,
                                "oyster-2025-reef-metadata-deployment" = "_metadata", 
                                "oyster-2025-rugosity" = "_rugosity",
                                "oyster-2025-reef-metadata-retrieval" = "_metadata",
                                "oyster-2025-count" = "_count",
                                "oyster-2025-oyster-height" = "_oyster_height",
                                "oyster-2025-bivalve-height" = "_bivalve_height",
                                "oyster-2025-weight" = "_weight",
                                "oyster-2025-mobile-fauna" = "_mobile_fauna",
                                "oyster-2025-sessile-fauna" = "_sessile_fauna",
                                "")
  
  # Add new string to filename and potentially change file type 
  if(str_ends(basename(input_filepath), ".xlsx")){
    filename <- gsub(".xlsx", paste0(output_filename_end, ".csv"), basename(input_filepath))
    
  } else if(str_ends(basename(input_filepath), ".csv")) {
    filename <- gsub(".csv", paste0(output_filename_end, ".csv"), basename(input_filepath))
    
  }
  
  output_filepath <- marinegeo.utils::utl_mg_data_index() %>%
    filter(table_id == !!target_table) %>%
    mutate(filepath = paste0(directory, "/", filename)) %>%
    pull(filepath)
  
  first_slash <- regexpr("/", output_filepath)
  
  # Extract everything after the first "/"
  local_data_filepath_out <- substr(output_filepath, first_slash + 1, nchar(output_filepath))
  
  return(local_data_filepath_out)
}