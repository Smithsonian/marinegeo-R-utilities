create_template_script <- function(script_filepath, target_table, input_filepath){
  
  if(target_table == "reef-life-survey-data-marinegeo-v1"){
    
    # Process input filepath
    first_slash <- regexpr("/", input_filepath)
    
    # Extract everything after the first "/"
    local_data_filepath_in <- substr(input_filepath, first_slash + 1, nchar(input_filepath))
    
    # Process output filepath 
    if(str_ends(basename(input_filepath), ".xlsx")){
      filename <- gsub(".xlsx", ".csv", basename(input_filepath))
    } else {
      filename <- basename(input_filepath)
    }
    
    output_filepath <- marinegeo.utils::utl_mg_data_index() %>%
      filter(table_id == !!target_table) %>%
      mutate(filepath = paste0(directory, "/", filename)) %>%
      pull(filepath)
    
    first_slash <- regexpr("/", output_filepath)
    
    # Extract everything after the first "/"
    local_data_filepath_out <- substr(output_filepath, first_slash + 1, nchar(output_filepath))
    
    script_template <- c(
      
      "# Process Reef Life Survey Excel data",
      "# Steps include",
      "#   - remove empty rows",
      "#   - assign Aphia IDs and resolve taxonomic issues",
      "#   - correct inconsistant metadata",
      "#   - check for suspicious invert and fish counts",
      "",
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
      "",
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
      "# Test Output and write data ####",
      "# The select(all_of(req_cols)) code will create an error if required columns are not present",
      "# It also reorders the columns to match the target table format",
      "# Both of these are very important to maintaining data workflows for the target table",
      "# and shouldn't be skipped.",
      "",
      "df_out %>%",
      "  select(all_of(req_cols)) %>%",
      paste0("\twrite_csv('", local_data_filepath_out, "')")
      
    )
    
  }
  
  writeLines(script_template, script_filepath)
  
}