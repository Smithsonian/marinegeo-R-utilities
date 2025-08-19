suggest_code_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("all_process_options")),
    uiOutput(ns("data_type_specific_options")),
    
    shinyjs::disabled(
      textAreaInput(ns("code_suggestion"), 
                    label = NULL,
                    height = "160px",
                    width = "600px")
    )
    
  )
}

suggest_code_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## Dynamic UI for Mutate/Filter Action Buttons ####
      output$all_process_options <- renderUI({
        
        req(input_list$table_selections$selected_cells)
        
        if(nrow(input_list$table_selections$selected_cells) == 1){
          
          div(
            actionButton(session$ns("mutate_all_values"), "Mutate all values within column"),
            actionButton(session$ns("mutate_selected_values"), "Mutate selected values"),
            actionButton(session$ns("filter"), "Filter based on selected values")
          )
          
        } else if(nrow(input_list$table_selections$selected_cells) > 1){
          
          div(
            actionButton(session$ns("mutate_selected_values"), "Mutate selected values"),
            actionButton(session$ns("mutate_x_for_all_y"), "Mutate X by all unique Y values"),
            actionButton(session$ns("filter"), "Filter based on selected values")
          )
        }
        
      })
      
      ## Data Type-specific dynamic UI for Mutate/Filter Action ####
      output$data_type_specific_options <- renderUI({
        
        req(input_list$input_table_id)
        
        if(input_list$input_table_id == "reef-life-survey-data-marinegeo-input"){
          
          div(
            actionButton(session$ns("add_rls_metadata"), "Add sample event ID column"),
            actionButton(session$ns("fill_metadata"), "Fill missing RLS metadata")
          )
          
        }
      })
      
      ## Create template code ####
      #### Mutate ####
      shiny::observeEvent(input$mutate_all_values, {
        
        shinyjs::enable(session$ns("code_suggestion"), asis = TRUE)
        
        req(input_list$selected_flag)
        
        if(input_list$selected_flag %in% c("all", "no_flags")){
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            select(-row_num, -flag)
          
        } else {
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            filter(!is.na(flag)) %>%
            select(-row_num, -flag)
          
        }
        
        mutate_template_text <- build_mutate_case_when_template(input_list$table_selections$selected_cells, 
                                                                input_list$table_selections$selected_columns,
                                                                expand_selected_values = TRUE,
                                                                df)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "code_suggestion",
                                   value = mutate_template_text)
        
      })
      
      shiny::observeEvent(input$mutate_selected_values, {
        
        shinyjs::enable(session$ns("code_suggestion"), asis = TRUE)
        
        req(input_list$selected_flag)
        
        if(input_list$selected_flag %in% c("all", "no_flags")){
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            select(-row_num, -flag)
          
        } else {
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            filter(!is.na(flag)) %>%
            select(-row_num, -flag)
          
        }
        
        mutate_template_text <- build_mutate_case_when_template(input_list$table_selections$selected_cells, 
                                                                input_list$table_selections$selected_columns,
                                                                expand_selected_values = FALSE,
                                                                df)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "code_suggestion",
                                   value = mutate_template_text)
        
      })
      
      shiny::observeEvent(input$mutate_x_for_all_y, {
        
        shinyjs::enable(session$ns("code_suggestion"), asis = TRUE)
        
        req(input_list$selected_flag)
        
        if(input_list$selected_flag %in% c("all", "no_flags")){
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            select(-row_num, -flag)
          
        } else {
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            filter(!is.na(flag)) %>%
            select(-row_num, -flag)
          
        }
        
        mutate_template_text <- mutate_x_for_all_y_case_when_template(input_list$table_selections$selected_cells, 
                                                                      input_list$table_selections$selected_columns,
                                                                      df)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "code_suggestion",
                                   value = mutate_template_text)
        
      })
      
      
      #### Filter ####
      shiny::observeEvent(input$filter, {
        
        shinyjs::enable(session$ns("code_suggestion"), asis = TRUE)
        
        req(input_list$selected_flag)
        
        if(input_list$selected_flag %in% c("all", "no_flags")){
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            select(-row_num, -flag)
          
        } else {
          
          df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            filter(!is.na(flag)) %>%
            select(-row_num, -flag)
          
        }
        
        filter_template_text <- build_filter_template(input_list$table_selections$selected_cells, 
                                                      df)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "code_suggestion",
                                   value = filter_template_text)
        
      })
      
      #### Data Type Specific Observers ####
      
      ###### Reef Life Survey ####
      observeEvent(input$add_rls_metadata, {
        
        mutate_template_text <- paste("df %>%",
                                      paste0("\tmutate(sample_event_id = paste(site_code, \"RLS\", date, depth, sep = \"_\"))"),
                                      sep = "\n")
        
        # Don't want users to modify these templates
        # shinyjs::enable(session$ns("code_suggestion"), asis = TRUE)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "code_suggestion",
                                   value = mutate_template_text)
        
      })
      
      observeEvent(input$fill_metadata, {
        
        mutate_template_text <- paste("df %>%",
                                      paste0("\tmarinegeo.utils::utl_rls_fill_missing_metadata()"),
                                      sep = "\n")
        
        # Don't want users to modify these templates
        # shinyjs::enable(session$ns("code_suggestion"), asis = TRUE)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "code_suggestion",
                                   value = mutate_template_text)
        
      })
      
    }
  )
}