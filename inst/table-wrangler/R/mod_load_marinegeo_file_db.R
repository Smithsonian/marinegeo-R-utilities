load_marinegeo_data_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    layout_columns(
      card(
        card_header("Load Data"),
        uiOutput(ns("select_data_type")),
        uiOutput(ns("select_input_data_table")),
        uiOutput(ns("select_file")),
        uiOutput(ns("select_excel_sheet")),
        uiOutput(ns("select_output_data_table")),
        
        actionButton(ns("load_selected_data"), "Load Data")
      ),
      
      card(
        card_header("Load QA/QC Resources"),
        
        card(
          card_header("Script Content"),
          card_body(
            uiOutput(ns("preview_r_script")),
            actionButton(ns("show_full_script"), "Show Full Script", class = "mt-2")
          )
        )
      )
      
    )
  )
}

load_marinegeo_data_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## Select and Load Data #### 
      # First check which directories exist on computer running the application
      directories <- marinegeo.utils::utl_mg_data_index() %>%
        filter(location %in% c("github")) %>%
        pull(directory)
      
      available_directories <- unlist(compact(
        lapply(directories, function(x){
          
          result <- dir.exists(paste0(Sys.getenv("repository_filepath"), x))
          
          if(result){
            x
          } else {
            NULL
          }
        })
      ))
      
      #### UI outputs ####
      output$select_data_type <- renderUI({
        
        data_types <- marinegeo.utils::utl_mg_data_index() %>%
          filter(location %in% c("github")) %>%
          filter(directory %in% available_directories) %>%
          count(protocol) %>%
          pull(protocol)
        
        pickerInput(session$ns("select_data_type"),
                    "Select Data Type",
                    choices = data_types,
                    options = pickerOptions(container = "body"),
                    width = "100%")
        
      })
      
      output$select_input_data_table <- renderUI({
        
        req(input$select_data_type)
        
        data_tables <- marinegeo.utils::utl_mg_data_index() %>%
          filter(protocol == input$select_data_type) %>%
          filter(!is.na(table_id)) %>%
          count(table_id, table_name) %>%
          pull(table_id, name = "table_name")
        
        pickerInput(session$ns("select_input_data_table"),
                    "Select table type to load",
                    choices = data_tables,
                    options = pickerOptions(container = "body"),
                    width = "100%")
        
      })
      
      get_data_dir_inventory <- reactive({
        
        req(input$select_input_data_table)
        
        target_repository_directory <- marinegeo.utils::utl_mg_data_index() %>%
          filter(protocol == input$select_data_type,
                 table_id == input$select_input_data_table) %>%
          pull(directory)
        
        filepath_directory <- list.files(
          paste0(Sys.getenv("repository_filepath"), target_repository_directory),
          full.names = T,
          recursive = T
        )
        
        file_directory <- list.files(
          paste0(Sys.getenv("repository_filepath"), target_repository_directory),
          recursive = T
        )
        
        file_table <- tibble(filepath = filepath_directory,
                             filename = basename(filepath_directory),
                             local_filepath = file_directory) %>%
          mutate(local_filepath = case_when(
            str_remove(local_filepath, filename) == "" ~ local_filepath,
            T ~ str_remove(local_filepath, filename)
          ))
        
        return(file_table)
        
      })
      
      output$select_file <- renderUI({
        
        file_table <- get_data_dir_inventory()
        
        file_list <- file_table %>% 
          select(local_filepath, filename) %>%
          split(., .[, "local_filepath"]) %>%  
          lapply(., function(x) x %>% pull(filename)) %>%
          lapply(., as.list) # the key is to use `as.list` instead of `list` here
        
        pickerInput(session$ns("select_file"),
                    "Select File to Load",
                    choices = file_list,
                    options = pickerOptions(container = "body", 
                                            liveSearch = TRUE),
                    width = "100%")
        
      })
      
      output$select_excel_sheet <- renderUI({
        
        req(input$select_file)
        
        if(str_ends(input$select_file, ".xlsx")){
          
          filepath <- get_data_dir_inventory() %>%
            filter(filename == input$select_file) %>%
            pull(filepath)
          
          sheet_names <- readxl::excel_sheets(filepath)
          
          default_option <- case_when(
            input$select_data_type == "Reef Life Survey" ~ "DATA",
            input$select_data_type == "Oyster Experiment 2025" ~ "INSTRUCTIONS",
            T ~ sheet_names[1]
          )
          
          pickerInput(session$ns("select_excel_sheet"),
                      "Select Excel Sheet to Load",
                      choices = sheet_names,
                      selected = default_option,
                      options = pickerOptions(container = "body", 
                                              liveSearch = TRUE),
                      width = "100%")
          
        }
        
      })
      
      output$select_output_data_table <- renderUI({
        
        req(input$select_file)
        
        data_table_out <- marinegeo.utils::utl_mg_data_index() %>%
          filter(table_id == input$select_input_data_table) %>%
          left_join(
            marinegeo.utils::utl_mg_table_relationships() %>%
              rename(table_id = table_id_in),
            by = "table_id"
          ) %>%
          filter(!is.na(table_id_out)) %>%
          count(table_id_out) %>%
          pull(table_id_out)
        
        data_tables <- marinegeo.utils::utl_mg_data_index() %>%
          filter(table_id %in% data_table_out) %>%
          count(table_id, table_name) %>%
          pull(table_id, name = table_name)
        
        pickerInput(session$ns("select_output_data_table"),
                    "Select Output Table",
                    choices = data_tables,
                    options = pickerOptions(container = "body"),
                    width = "100%")
        
      })
      
      #### Load data into application ####
      observeEvent(input$load_selected_data, {
        
        # Load filepath of data
        data_filename <- input$select_file
        
        filepath <- get_data_dir_inventory() %>%
          filter(filename == data_filename) %>%
          pull(filepath)
        
        if(str_ends(data_filename, ".xlsx")){
          # Load excel data
          df <- marinegeo.utils::utl_mg_load_excel(filepath, 
                                                   input$select_output_data_table,
                                                   input$select_excel_sheet)
          
        } else if(str_ends(data_filename, ".csv")){
          df <- read_csv(filepath)
        }
        
        input_list$data_filepath <- filepath
        input_list$data_filename <- data_filename
        
        input_list$project_directory <- get_data_dir_inventory() %>%
          filter(filename == data_filename) %>%
          mutate(local_filepath = case_when(
            local_filepath == filename ~ NA,
            T ~ local_filepath
          )) %>%
          pull(local_filepath)
        
        input_list$input_table_id <- input$select_input_data_table
        input_list$output_table_id <- input$select_output_data_table
        input_list$in_df <- df
        input_list$out_df <- df
        
        # Set up the code chain
        script_filename <- get_script_filepath()
        
        if(length(script_filename) == 0){
          
          script_repository_directory <- marinegeo.utils::utl_mg_data_index() %>%
            filter(protocol == input$select_data_type,
                   table_id== input$select_output_data_table) %>%
            pull(script_directory)
          
          # This needs to point toward the script directory
          input_list$script_filepath <- paste0(
            Sys.getenv("repository_filepath"),
            script_repository_directory, "/",
            gsub(".xlsx", ".R", data_filename)
          )
          
          # Create a template script
          if(!file.exists(input_list$script_filepath)){
            
            local_data_filepath <- gsub(Sys.getenv("repository_filepath"),
                                        "",
                                        input_list$data_filepath)
            
            script_lines <- create_template_script(
              script_filepath = input_list$script_filepath,
              target_table = input_list$output_table_id,
              input_filepath = local_data_filepath
            )
            
          }
          
        } else {
          # Save the name of the script's filepath for other modules to use 
          # Particularly to update the script after any changes
          input_list$script_filepath <- script_filename
          
          
          i_am_error <- FALSE
          
          tryCatch({
            
            # The following code also is used in `mod_run_processing_script.R`
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
            
          }, error = function(e){
            
            i_am_error <<- TRUE
            error_message <<- e$message
            
          })
          
          if(i_am_error){
            
            showModal(modalDialog(
              title = "Error running R script! Only unprocessed data loaded!",
              error_message
            ))
            
          }
          
        }
        
      })
      
      ## Load R script associated with data ####
      get_script_filepath <- reactive({
        
        req(input$select_output_data_table)
        
        target_repository_directory <- marinegeo.utils::utl_mg_data_index() %>%
          filter(protocol == input$select_data_type,
                 table_id== input$select_output_data_table) %>%
          pull(script_directory)
        
        filepath_directory <- list.files(
          paste0(Sys.getenv("repository_filepath"), target_repository_directory),
          full.names = T,
          recursive = T
        )
        
        file_directory <- list.files(
          paste0(Sys.getenv("repository_filepath"), target_repository_directory),
          recursive = T
        )
        
        script_dir <- tibble(filepath = filepath_directory,
                             filename = basename(filepath_directory),
                             local_filepath = file_directory) %>%
          mutate(local_filepath = str_remove(local_filepath, filename))
        
        
        if(str_ends(input$select_file, ".xlsx")){
          script_filename <- gsub(".xlsx", ".R", input$select_file)
          
        } else if(str_ends(input$select_file, ".csv")){
          script_filename <- gsub(".csv", ".R", input$select_file)
          
        }
        
        script_filepath <- script_dir %>%
          filter(filename == script_filename) %>%
          pull(filepath)
        
        return(script_filepath)
        
      })
      

      # Track if showing full script or preview
      show_full <- reactiveVal(FALSE)
      
      # Toggle between full view and preview
      observeEvent(input$show_full_script, {
        current <- show_full()
        show_full(!current)
        updateActionButton(
          session, 
          "show_full_script", 
          label = if (!current) "Show Preview" else "Show Full Script"
        )
      })
      
      output$preview_r_script <- renderUI({
        
        req(input$select_file)
        
        if(length(get_script_filepath()) == 0){
          div("No R Script exists for this file")
          
        } else {
          
          script_lines <- readLines(get_script_filepath())
          
          if (length(script_lines) == 0) {
            return(p("Empty file"))
          }
          
          if (show_full()) {
            # Show full script
            full_text <- paste(script_lines, collapse = "\n")
            pre(code(full_text), class = "r")
          } else {
            # Show preview (first 10 lines)
            preview_lines <- min(10, length(script_lines))
            preview_text <- paste(script_lines[1:preview_lines], collapse = "\n")
            
            if (length(script_lines) > preview_lines) {
              preview_text <- paste0(preview_text, "\n...[", length(script_lines) - preview_lines, " more lines]")
            }
            
            pre(code(preview_text), class = "r")
          }
          
        }
      })
      
    }
  )
}