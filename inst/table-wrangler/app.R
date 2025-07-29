library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(lubridate)
library(purrr)
library(readxl)
library(DT) # Interactive Table
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(marinegeo.utils)

ui <- page_navbar(
  title = "MarineGEO Table Wrangler",
  useShinyjs(),
  # theme = bs_theme(bootswatch = "solar"),
  
  # Javascript to redirect tab to indentation within a text box
  # Instead of moving active cursor to  next element
  
  tags$head(
    tags$script(HTML("
      $(document).on('keydown', 'textarea', function(e) {
        if (e.key == 'Tab') {
          e.preventDefault();
          var start = this.selectionStart;
          var end = this.selectionEnd;
          
          // Insert tab character
          this.value = this.value.substring(0, start) + '\\t' + 
                      this.value.substring(end);
          
          // Set cursor position after the inserted tab
          this.selectionStart = this.selectionEnd = start + 1;
        }
      });
    "))
  ), 
  
  sidebar = sidebar(
    uiOutput("filename"),
    mod_run_processing_script_UI("reload_r"),
    qc_flag_UI("qc_sidebar")
  ),
  
  nav_panel(title = "Load Data",
  
              load_marinegeo_data_UI("load_data")
  ),
  
  nav_panel(title = "QA/QC Data",
            
             navset_card_tab(
               nav_panel(
                 title = "Interactive Table",
                 card(
                   DT_table_UI("rls_excel_table"),
                   full_screen = TRUE
                 )
               ),
               
               nav_panel(
                 "Generate Code",
                 suggest_code_UI("code_suggestions")
               ),
               
               nav_panel(
                 "Sample Event Summary",
                 sample_event_UI("sample_events")
               ),
               
               nav_panel(
                 title = "Data Structure",
                 table_structure_UI("table_structure")
               ),
               
               nav_panel(
                 title = "Visualizations",
                 visualizations_UI("viz")
               )
             )
           
  )
)

server <- function(input, output, session) {
  
  # The `input_list` gets passed to each module to share app variables
  input_list <- reactiveValues(
    
    in_df = data.frame(), # Loaded data
    out_df = data.frame(), # Data that has all code chain links applied and sent to table UI
    
    # Passed to table UI with maximum of one flag per row
    flag_df = tibble(
      flag = NA_character_,
      row_num = NA_integer_,
      .rows = 0
    ), 
    selected_flag = NULL,
    
    data_filepath = NULL, # Filepath to currently loaded data table
    data_filename = NULL, # Filename for currently loaded data table
    script_filepath = NULL, # File path to the script associated with the data table that is currently loaded
    input_table_id = NULL, # ID of table that data is drawn from
    output_table_id = NULL, # ID of table that data will be written to

    code_chain = list(),
    load_code_chain_flag = 0,
    flush_code_chain_ids = NULL, # Name of code chain IDs to be replaced by a new code chain when loading an R script
    
    table_selections = list() # Hold information on cell and column selections in the DT module
      # A matrix
      # selected_cells = input$table_cells_selected,
      # A vector of column names from `df`
      # selected_columns = column_names
  )
  
  load_marinegeo_data_server("load_data", input_list)
  DT_table_server("rls_excel_table", input_list)
  sample_event_server("sample_events", input_list)
  qc_flag_server("qc_sidebar", input_list)
  suggest_code_server("code_suggestions", input_list)
  mod_run_processing_script_server("reload_r", input_list)
  table_structure_server("table_structure", input_list)
  visualizations_server("viz", input_list)
  
  output$filename <- renderUI({
    req(input_list$data_filename)
    div(input_list$data_filename)
  })
}

shinyApp(ui, server)