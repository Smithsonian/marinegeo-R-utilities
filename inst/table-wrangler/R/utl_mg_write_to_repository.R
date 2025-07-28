
utl_mg_write_to_repository <- function(df, filename, table_id){

  if(str_ends(filename, ".xlsx")){
    filename <- gsub(".xlsx", ".csv", filename)
    
  }
  
  target_filepath <- marinegeo.utils::utl_mg_data_index() %>%
    filter(table_id == !!table_id) %>%
    mutate(filepath = paste0(Sys.getenv("repository_filepath"), directory, "/", filename)) %>%
    pull(filepath)
  
  if(file.exists(target_filepath)){
    message("Output file already exists. It will be overwritten by new version!")
    
  }
  
  readr::write_csv(df, target_filepath)
  
}