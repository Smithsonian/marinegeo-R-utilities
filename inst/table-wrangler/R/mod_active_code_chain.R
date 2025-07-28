# The code chain includes both the code and related attributes stored in input_list as a list()
# The order of the code_chain sub-items reflects the order of execution of the code chunks. 
# It can be re-ordered by the user. 
# input_list
# ... code_chain
# ... ... [[#]] ID name ("code_chain_x") assigned when code chunk is created, where x is an integer
# ... ... ... $code - a character vector to be evaluated as R code
# ... ... ... $attr - a list of attributes associated with the code chunk
# ... ... ... ... $active - T/F, specifying whether or not the code link is active and should be evaluated
# ... ... ... ... $type - DPLYR/CUSTOMFUNCTION
# ... ... ... ... $foo - additional attributes can be defined

# The code chain can be converted to an R script and vice versa. 
# When a dataset is loaded that has an associated R script,
# the existing code chain should be flushed (if it exists) and be replaced. 
# The conversion from R script text to the input_list$code_chain occur in "mod_load_code_chain.R"

code_chain_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("all_process_options")),
    uiOutput(ns("data_type_specific_options")),
    
    shinyjs::disabled(
      textAreaInput(ns("enter_code"), 
                    label = NULL,
                    height = "160px",
                    width = "600px")
    ),
    
    div(
      actionButton(ns("create_code_link"), "Confirm new code"),
      actionButton(ns("save_code"), "Save code changes"),
      actionButton(ns("save_data"), "Save code & data"),
    ),
    
    # Create a div to place code chunks (links) under
    tags$div(id = ns("input_container"), class = "mb-3")
    
  )
}

code_chain_server <- function(id, input_list) {
  shiny::moduleServer(
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
            actionButton(session$ns("filter"), "Filter based on selected values")
          )
        }
        
      })
      
      ## Data Type-specific dynamic UI for Mutate/Filter Action ####
      output$data_type_specific_options <- renderUI({
        
        req(input_list$input_table_id)
        
        if(input_list$input_table_id == "reef-life-survey-data-marinegeo-input"){
          
          div(
            actionButton(session$ns("add_rls_metadata"), "Add RLS metadata columns")
          )
          
        }
      })
      
      ## Create template code ####
      #### Mutate ####
      shiny::observeEvent(input$mutate_all_values, {
        
        shinyjs::enable(session$ns("enter_code"), asis = TRUE)
        
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
                                   inputId = "enter_code",
                                   value = mutate_template_text)
        
      })
      
      shiny::observeEvent(input$mutate_selected_values, {
        
        shinyjs::enable(session$ns("enter_code"), asis = TRUE)
        
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
                                   inputId = "enter_code",
                                   value = mutate_template_text)
        
      })
      
      #### Filter ####
      shiny::observeEvent(input$filter, {
        
        shinyjs::enable(session$ns("enter_code"), asis = TRUE)
        
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
                                   inputId = "enter_code",
                                   value = filter_template_text)
        
      })
      
      #### Data Type Specific Observers ####
      
      ###### Reef Life Survey ####
      observeEvent(input$add_rls_metadata, {
        
        mutate_template_text <- build_reef_life_survey_metadata_template(input_list$data_filename)
        
        # Don't want users to modify these templates
        # shinyjs::enable(session$ns("enter_code"), asis = TRUE)
        
        shiny::updateTextAreaInput(session,
                                   inputId = "enter_code",
                                   value = mutate_template_text)
        
      })
      
      ## Create Code Chain ####
      # Counter for unique IDs created for code chain links
      code_chain_link_counter <- reactiveVal(1)
      
      #### Replace existing code chain with loaded one from R script ####
      observeEvent(input_list$load_code_chain_flag, {
        
        if(!is.null(input_list$flush_code_chain_ids)){
          
          for(input_id in input_list$flush_code_chain_ids){
            # Clear the container
            removeUI(selector =  paste0("#", 
                                        session$ns(paste0("container_", input_id))))
          }
        }
        
        code_chain_link_counter(length(input_list$code_chain) + 1)
        
        lapply(names(input_list$code_chain), function(input_id){
          create_code_link_reactives(input_id)
        })
        
        rerender_chain()
        
        
      })
      
      
      ####  Add links to input_list$code_chain ####
      observeEvent(input$create_code_link, {
        
        if(nchar(input$enter_code) > 0){
          
          # Get and update counter
          current_counter <- code_chain_link_counter()
          code_chain_link_counter(current_counter + 1)
          
          # Get new input ID and update all input IDs
          input_id <- paste0("chain_link_", current_counter)
          
          new_link_text <- input$enter_code
          
          height_link_text <- calculate_textbox_height(new_link_text)
          
          # Assign code link text to a reactiveValues list sub-item
          # Assign order in chain - by default, last position
          # Assign code link as "active"
          input_list$code_chain <- append(input_list$code_chain, 
                                          setNames(
                                            list(
                                              list(code = new_link_text, 
                                                   attr = list(active = TRUE,
                                                               type = "DPLYR"))),
                                            input_id)
          )
          
          # Run the code chain link 
          # All previous existing links do not need to be rerun here because
          # the new link is added to the end of the chain.
          result <- execute_code_chain_link(input_id)
          
          # If the result is not NULL, then an error occurred. The outline of the div will be red
          if(!is.null(result)){
            div_style <- "border-style: solid; border-color: red;"
          } else  {
            div_style <- ""
          }
          
          # Create a container for the code chain link with a remove button
          insertUI(
            selector = paste0("#", session$ns("input_container")),
            ui = tags$div(
              id = session$ns(paste0("container_", input_id)),
              class = "d-flex align-items-center mb-2",
              style = div_style,
              textAreaInput(session$ns(input_id), label = NULL,
                            height = paste0(height_link_text, "px"),
                            width = "600px",
                            value = new_link_text),
              
              div(
                class = "d-flex ms-2",
                actionButton(session$ns(paste0("up_", input_id)), 
                             label = icon("arrow-up"), 
                             class = "btn-sm btn-outline-secondary me-1"
                ),
                actionButton(session$ns(paste0("down_", input_id)), 
                             label = icon("arrow-down"), 
                             class = "btn-sm btn-outline-secondary me-1"
                ),
                actionButton(session$ns(paste0("refresh_", input_id)), 
                             label = icon("refresh"), 
                             class = "btn-sm btn-outline-secondary me-1"
                ),
                actionButton(session$ns(paste0("remove_", input_id)), 
                             label = "Remove", 
                             class = "btn-sm btn-danger"),
                prettyToggle(session$ns(paste0("activate_", input_id)),
                             label_on = "Active", label_off = "Inactive",
                             value = TRUE, inline = TRUE, outline = TRUE, 
                             bigger = TRUE, shape = "curve",
                             status_off = NULL, status_on = "warning",
                             icon_on = icon("lightbulb"), icon_off = icon("lightbulb")
                )
              )
            )
          )
          
          create_code_link_reactives(input_id)
          
          shiny::updateTextAreaInput(session,
                                     inputId = "enter_code",
                                     value = "")
          
          shinyjs::disable(session$ns("enter_code"), asis = TRUE)
          
        }
        
      }, ignoreInit = T)
      
      create_code_link_reactives <- function(input_id){
        
        # Create observers for up and down buttons
        observeEvent(input[[paste0("up_", input_id)]], {
          move_input_up(input_id)
        })
        
        observeEvent(input[[paste0("down_", input_id)]], {
          move_input_down(input_id)
        })
        
        observeEvent(input[[paste0("refresh_", input_id)]], {
          # Update the code chain with the updates in the textAreaBox
          input_list$code_chain[[input_id]]$code <- input[[input_id]]
          rerender_chain()
          
        })
        
        # Set up an observer for the remove button
        observeEvent(input[[paste0("remove_", input_id)]], {
          
          current_ids <- names(input_list$code_chain)
          
          # Remove the link from the code chain
          input_list$code_chain <- input_list$code_chain[setdiff(current_ids, input_id)]
          
          removeUI(selector =  paste0("#", 
                                      session$ns(paste0("container_", input_id))))
          
          # Update UI by removing and reinserting all elements in the code_chain
          rerender_chain()
          
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("activate_", input_id)]], {
          
          # Update the code chain with a new active status
          input_list$code_chain[[input_id]]$attr$active <- input[[paste0("activate_", input_id)]]
          rerender_chain()
          
        }, ignoreInit = TRUE)
        
      }
      
      ## Edit Code Chain ####
      
      # Function to move an input up in the order
      move_input_up <- function(id) {
        
        current_position <- which(names(input_list$code_chain) == id)
        new_position <- current_position - 1
        
        # Only move inputs if they are not in the first position already
        if(current_position > 1){
          
          updated_order <- get_code_chain_order(current_position, new_position)
          
          input_list$code_chain <- input_list$code_chain[updated_order]
          
          # Rearrange in the UI by removing and reinserting all elements
          rerender_chain()
          
        }
        
      }
      
      # Function to move an input down in the order
      move_input_down <- function(id) {
        
        current_position <- which(names(input_list$code_chain) == id)
        new_position <- current_position + 1
        
        # Only move inputs if they are not in the last position already
        if(current_position != length(input_list$code_chain)){
          
          updated_order <- get_code_chain_order(current_position, new_position)
          
          input_list$code_chain <- input_list$code_chain[updated_order]
          
          # Rearrange in the UI by removing and reinserting all elements
          rerender_chain()
          
        }
      }
      
      # Returns a named vector reflecting the updated order of code chain links
      get_code_chain_order <- function(current_position, new_position){
        
        new_order <- 1:length(input_list$code_chain)
        
        # Shift elements between from_pos and to_pos
        if(current_position < new_position) {
          new_order[current_position:new_position] <- c(new_order[(current_position+1):new_position], current_position)
        } else {
          new_order[new_position:current_position] <- c(current_position, new_order[new_position:(current_position-1)])
        }
        
        return(new_order)
        
      }
      
      # Function to rerender all inputs based on the current order
      rerender_chain <- function() {
        
        # Reset the data frame in the table
        # Each code link will be run against it in the order of the chain
        input_list$out_df <- input_list$in_df
        
        # Create new code links in the UI
        lapply(names(input_list$code_chain), function(input_id){
          
          # Clear the container
          removeUI(selector =  paste0("#", 
                                      session$ns(paste0("container_", input_id))))
          
          height_link_text <- calculate_textbox_height(input_list$code_chain[[input_id]]$code)
          
          # If the code link is active, run the code
          if(input_list$code_chain[[input_id]]$attr$active){
            
            # Set the new input to TRUE for active
            active_status <- TRUE
            # Run the code chain link 
            result <- execute_code_chain_link(input_id)
            
            # If the result is not NULL, then an error occurred. The outline of the div will be red
            if(!is.null(result)){
              div_style <- "border-style: solid; border-color: red;"
            } else  {
              div_style <- ""
            }
            
          } else {
            # Make sure new active input inits as FALSE
            active_status <- FALSE
            div_style <- ""
          }
          
          # Create a container for the code chain link with a remove button
          insertUI(
            selector = paste0("#", session$ns("input_container")),
            ui = tags$div(
              id = session$ns(paste0("container_", input_id)),
              class = "d-flex align-items-center mb-2",
              style = div_style,
              textAreaInput(session$ns(input_id), label = NULL, 
                            height = paste0(height_link_text, "px"),
                            width = "600px",
                            value = input_list$code_chain[[input_id]]$code),
              div(
                class = "d-flex ms-2",
                actionButton(session$ns(paste0("up_", input_id)), 
                             label = icon("arrow-up"), 
                             class = "btn-sm btn-outline-secondary me-1"
                ),
                actionButton(session$ns(paste0("down_", input_id)), 
                             label = icon("arrow-down"), 
                             class = "btn-sm btn-outline-secondary me-1"
                ),
                actionButton(session$ns(paste0("refresh_", input_id)), 
                             label = icon("refresh"), 
                             class = "btn-sm btn-outline-secondary me-1"
                ),
                actionButton(session$ns(paste0("remove_", input_id)), 
                             label = "Remove", 
                             class = "btn-sm btn-danger"),
                prettyToggle(session$ns(paste0("activate_", input_id)),
                             label_on = "Active", label_off = "Inactive",
                             value = active_status, inline = TRUE, outline = TRUE, 
                             bigger = TRUE, shape = "curve",
                             status_off = NULL, status_on = "warning",
                             icon_on = icon("lightbulb"), icon_off = icon("lightbulb")
                )
              )
            )
          )
          
        })
        
      }
      
      ## Convert code chain to or load from R Script ####
      ### Initiate Loaded R script to code chain ####
      
      ### Save Code Chain to R Script ####
      observeEvent(input$save_code, {
        
        write_code_chain_to_R_script(input_list$code_chain,
                                     input_list$script_filepath)
        
        showModal(modalDialog(
          title = "Code saved!",
          paste("Code saved as", input_list$script_filepath, sep = " ")
        ))
        
      })
      
      ### Save Data & Code ####
      
      observeEvent(input$save_data, {
        
        i_am_error <- FALSE
        
        tryCatch({
          
          write_code_chain_to_R_script(input_list$code_chain,
                                       input_list$script_filepath)
          
          df <- input_list$out_df %>%
            select(all_of(utl_mg_column_order(input_list$output_table_id)))
          
          utl_mg_write_to_repository(
            df,
            basename(input_list$data_filepath),
            input_list$output_table_id
          )

        }, error = function(e){
          
          i_am_error <<- TRUE
          error_message <<- e$message
          
        })
        
        if(i_am_error){
          
          showModal(modalDialog(
            title = "Error saving code and data!",
            error_message
          ))
          
          return(error_message)
          
        } else {
          
          showModal(modalDialog(
            title = "Code and data saved!",
            paste0("Data saved to ", input_list$data_filepath, ".\n",
                   "Code saved to ", input_list$script_filepath, ".")
          ))
          
          
        }
      })
    
      
      ## Helper functions ####
      calculate_textbox_height <- function(link_text){
        
        # Count the number of lines
        num_lines <- length(strsplit(link_text, "\n")[[1]])
        
        # Calculate an appropriate height (e.g., 20px per line plus 40px padding)
        height_link_text <- 20 * num_lines + 40
        
        return(height_link_text)
        
      }
      
      # This function is always running on input_list$out_df
      # It returns either NULL or a character vector representing an error message
      # If an error message is returned, the UI code link gets a red outline
      execute_code_chain_link <- function(link_id){
        
        code_text <- input_list$code_chain[[link_id]]$code
        df_pre_execution <- input_list$out_df
        
        i_am_error <- FALSE
        
        tryCatch({
          
          result <- evalute_in_safe_environment(code_text, df_pre_execution)
          
        }, error = function(e){
          
          i_am_error <<- TRUE
          error_message <<- e$message
          
        })
        
        if(i_am_error){
          
          showModal(modalDialog(
            title = "Error!",
            error_message
          ))
          
          return(error_message)
          
        } else {
          input_list$out_df <- result
          
          return(NULL)
        }
        
      }
      
    }
  )
}