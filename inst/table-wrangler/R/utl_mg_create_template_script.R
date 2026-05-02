
create_template_script <- function(script_filepath, target_table, input_filepath, excel_sheet = NULL, template_type = "standardized") {

  browser()
  
  if(str_starts(input_filepath, "/")){
    input_filepath <- substr(input_filepath, 2, nchar(input_filepath))
  }

  # Process input filepath
  first_slash <- regexpr("/", input_filepath)

  # Extract everything after the first "/"
  local_data_filepath_in <- substr(input_filepath, first_slash + 1, nchar(input_filepath))

  local_data_filepath_out <- assemble_output_filepath(input_filepath, target_table)

  # Build the data loading call based on input file type
  if (str_ends(input_filepath, "\\.(xlsx|xls)")) {
    load_data_call <- paste0("df <- readxl::read_excel(input_file_path, '", excel_sheet, "') %>%")
  } else {
    load_data_call <- "df <- read_csv(input_file_path) %>%"
  }

  # Read the template file for this table ID
  template_path <- file.path("template-scripts", template_type, paste0(target_table, ".R"))

  if (!file.exists(template_path)) {
    stop(paste("No template script found for table ID:", target_table,
               "\nExpected path:", template_path))
  }

  template_text <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  # Substitute placeholders
  script_text <- template_text
  script_text <- gsub("__INPUT_FILE_PATH__",  local_data_filepath_in,  script_text, fixed = TRUE)
  script_text <- gsub("__OUTPUT_FILE_PATH__", local_data_filepath_out, script_text, fixed = TRUE)
  script_text <- gsub("__LOAD_DATA__",        load_data_call,          script_text, fixed = TRUE)

  writeLines(strsplit(script_text, "\n")[[1]], script_filepath)

}

get_short_name_for_table <- function(target_table) {
  index <- marinegeo.utils::utl_mg_get_registry("data_index")
  match <- index %>%
    filter(table_id == !!target_table) %>%
    pull(short_name)
  if (length(match) == 0 || is.na(match[1])) return(NULL)
  return(match[1])
}

assemble_output_filepath <- function(input_filepath, target_table){

  # Add new string to filename and potentially change file type
  if(str_ends(basename(input_filepath), ".xlsx")){
    filename <- gsub(".xlsx", paste0("_", get_short_name_for_table(target_table), ".csv"), basename(input_filepath))

  } else if(str_ends(basename(input_filepath), ".csv")) {
    filename <- basename(input_filepath)

  }

  output_filepath <- utl_mg_get_registry("data_index") %>%
    filter(table_id == !!target_table) %>%
    mutate(filepath = paste0(directory, "/", filename)) %>%
    pull(filepath)

  first_slash <- regexpr("/", output_filepath)

  # Extract everything after the first "/"
  local_data_filepath_out <- substr(output_filepath, first_slash + 1, nchar(output_filepath))

  return(local_data_filepath_out)
}
