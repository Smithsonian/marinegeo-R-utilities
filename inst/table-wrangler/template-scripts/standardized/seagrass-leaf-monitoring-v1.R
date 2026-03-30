# Process Seagrass Monitoring data

## How to use this script ####
# Code entered in the 'MarineGEO Table Wrangler Start' section can be
# run in the Table Wrangler application to assist processing data.
# The application can create code that you can copy and paste into this script.
# IMPORTANT: Any objects used in this script between the 'Start' and 'End' section
# must also be available in the application or else it will crash:
#   - Functions used in this section must be sourced from the `tidyverse` or `marinegeo.utils` packages,
#     or from `base` R packages.
#   - `df` is the name of the dataframe used in the app and by default here,
#     do not change the name of the dataframe that feeds into the 'Start' section.
#   - `df_out` is the name of the dataframe created by the code in the application,
#     do not change name of the `df_out` dataframe in this script.
#   - You do not need to evaluate any code in the application. In that case,
#     simply keep the line `df_out <- df` and do not add any additional code.

# `marinegeo.utils` is a package of functions and resources to assist
# in common data management tasks. To install, run (requires `devtool` package):
# devtools::install_github('https://github.com/Smithsonian/marinegeo-R-utilities')

# Use the MarineGEO Table Wrangler Shiny application to support processing
# run: marinegeo.utils::shiny_launch_table_wrangler()

# Load Packages
library(tidyverse)
library(readxl)
library(marinegeo.utils)

input_file_path <- '__INPUT_FILE_PATH__'

## Destination table metadata
table_out <- 'seagrass-leaf-monitoring-v1'
req_cols <- marinegeo.utils::utl_mg_column_order(table_out)


## MarineGEO Table Wrangler Start ####

# Load data
__LOAD_DATA__
  select(-any_of(c("ID", "Site Code"))) %>%
  filter(if_any(everything(), ~ !is.na(.))) %>%
  rename(
    transect = any_of("Transect ID"),
    site_name = any_of("Location Name"),
    leaf_measurement_notes = any_of("Leaf Measurement Data Notes")
  ) |>
  mutate(input_filename = basename(input_file_path),
         table_id = table_out)

colnames(df) <- gsub(" ", "_", tolower(colnames(df)))

df_out <- df %>%
  left_join(
    marinegeo.utils::utl_mg_get_registry("site_codes") %>%
      select(partner_code, site_code, site_name), by = "site_name"
  ) %>%
  mutate(sample_event_id = paste(partner_code, site_code, year, sep = "_"),
         sample_collection_date = ymd(paste(year, month, day, sep = "-"))) %>%
  marinegeo.utils::utl_mg_generate_row_uuid(table_out) # %>%
  # select(any_of(req_cols))

## MarineGEO Table Wrangler End ##

# Test Output and write data ####
# The select(all_of(req_cols)) code will create an error if required columns are not present
# It also reorders the columns to match the target table format
# Both of these are very important to maintaining data workflows for the target table
# and shouldn't be skipped.

df_out %>%
  select(all_of(req_cols)) %>%
  marinegeo.utils::utl_mg_test_data_types(table_out) %>%
	write_csv('__OUTPUT_FILE_PATH__')
