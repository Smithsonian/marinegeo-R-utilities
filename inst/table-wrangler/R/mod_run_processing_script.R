mod_run_processing_script_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reload"), "Reload R script")
  )
}

mod_run_processing_script_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$reload, {
        
        req(input_list$script_filepath)
        
        # The following code also is used in `mod_load_marinegeo_file_db.R`
        script_lines <- readLines(input_list$script_filepath)
        
        start_line <- grep("## MarineGEO Table Wrangler Start", script_lines)
        end_line <- grep("## MarineGEO Table Wrangler End", script_lines)
        
        script_excerpt <- script_lines[(start_line+1):(end_line - 1)]
        
        df <- input_list$in_df
        
        # Convert the escape sequences to their actual characters
        parsed_text <- parse(text = script_excerpt)
        
        # Should create a dataframe named `df_out`
        eval(parsed_text)
        
        # evaluate, script should run on a dataframe named `df`
        input_list$out_df <- df_out
        
      })
    }
  )
}