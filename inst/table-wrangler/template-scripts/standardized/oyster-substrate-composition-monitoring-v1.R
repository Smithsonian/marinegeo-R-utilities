# Process Oyster Substrate Composition Monitoring Data 
## How to use this script ####
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
# or `R -e "marinegeo.utils::shiny_launch_table_wrangler()"` in your terminal

# Load Packages
library(tidyverse)
library(readxl)
library(marinegeo.utils)

input_file_path <- '__INPUT_FILE_PATH__'

## Destination table metadata
table_out <- 'oyster-substrate-composition-monitoring-v1'
req_cols <- marinegeo.utils::utl_mg_column_order(table_out)

## MarineGEO Table Wrangler Start ####
#Load all three sheets and pivot appropriately 
type_columns_high = c("Live Oyster", "Box Oyster", "Cultch", "Shell Hash", "Sediment", "Rock")
df_high <- readxl::read_excel(input_file_path, "OYSTER COMPOSITION - HIGH VIS", skip = 1)%>%
  pivot_longer(cols = type_columns_high, names_to = "cover_type", values_to = "point_count")%>%
  mutate(`Transect ID` = as.character(`Transect ID`))

type_columns_low = c("Large Shell Material", "Cultch", "Shell Hash","Sediment","Rock")
df_low <- readxl::read_excel(input_file_path, "OYSTER COMPOSITION - LOW VIS", skip = 1)%>%
  pivot_longer(cols = type_columns_low, names_to = "cover_type", values_to = "point_count")%>%
  mutate(`Transect ID` = as.character(`Transect ID`))

type_columns_other = c("Primary Point Count","Secondary Point Count")
df_other <- readxl::read_excel(input_file_path, "COMPOSITION - OTHER SPECIES")%>%
  pivot_longer(cols = type_columns_other, names_to = "cover_type", values_to = "point_count")%>%
  mutate(`Transect ID` = as.character(`Transect ID`))

df <- bind_rows(df_high, df_low, df_other)%>%
  filter(!is.na(`Site Name`)) %>%
  mutate(cover_type = tolower(cover_type))%>%
  mutate(cover_type = case_when(
    cover_type == "primary point count" ~ "other primary",
    cover_type == "secondary point count" ~ "canopy taxa",
    T ~ cover_type
  ))

colnames(df) <- gsub(" ", "_", tolower(colnames(df)))

#Configure columns and column names
df <- df |>
  mutate(input_filename = basename(input_file_path),
         table_id = table_out)|>
  select(-any_of(c("id", "site_code"))) |>
  filter(if_any(everything(), ~ !is.na(.))) |>
  rename(
    transect = any_of("transect_id"),
  )


#Add QC corrections here. 
df_out <- df%>%
  mutate(points_in_quadrat = 81)%>%
  mutate(percent_cover = round((point_count/points_in_quadrat) *100,2))%>%
  mutate(sample_collection_date = ymd(paste(year, month, day, sep = "-")))#%>%
# left_join(
#  marinegeo.utils::utl_mg_get_registry("site_codes") %>%
#    select(partner_code, site_code, site_name), by = "site_name"
# ) %>%
# mutate(sample_event_id = paste(partner_code, site_code, year(sample_collection_date), sep = "_")) %>%
# marinegeo.utils::utl_mg_generate_row_uuid(table_out) %>%
#select(any_of(req_cols))

## MarineGEO Table Wrangler End #

# Test Output and write data ####
# The select(all_of(req_cols)) code will create an error if required columns are not present
# It also reorders the columns to match the target table format
# Both of these are very important to maintaining data workflows for the target table
# and shouldn't be skipped.

df_out %>%
  select(all_of(req_cols)) %>%
  marinegeo.utils::utl_mg_test_data_types(table_out) %>%
  write_csv('__OUTPUT_FILE_PATH__')
