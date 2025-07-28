qc_flag_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("select_flag"))
  )
}

qc_flag_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$select_flag <- renderUI({
        
        req(input_list$out_df)
        
        # UI only rendered if data is loaded
        if(nrow(input_list$out_df) > 0) {
          flags_list <- utl_rls_run_all_qc(input_list$out_df, input_list$output_table_id)
          
          test_names <- unlist(
            purrr::compact(
              lapply(names(flags_list), function(i){
                if(is.null(flags_list[[i]]$results)){
                  return(NULL)
                } else if (length(flags_list[[i]]$results) == 0){
                  return(NULL)
                } else return(i)
              })
            )
          )
          
          if(nrow(result_list_to_table(flags_list)) == 0){
            flag_choices = setNames(
              "no_flags",
              "No flags present in data"
            )
          } else {
            flag_choices = setNames(
              c("all", test_names),
              c("Show all flags", gsub("_", " ", test_names))
            )
          }
          
          selectInput(session$ns("select_flag"), 
                      "Subset data by flag",
                      choices = flag_choices)
          
        }
      })
      
      observeEvent(input$select_flag, {
        
        input_list$selected_flag <- input$select_flag
        
        # If there are no flags, create an empty flag df
        if(input$select_flag == "no_flags"){
          
          input_list$flag_df <- tibble(
            flag = NA_character_,
            row_num = NA_integer_,
            .rows = 0
          )
          
        } else {
          
          results_list <- utl_rls_run_all_qc(input_list$out_df, input_list$output_table_id)
          
          # Convert the QC flag results list into a dataframe
          # Drop any tests that have no flags
          full_flag_table <- result_list_to_table(results_list)
          
          # If a single test is selected, only those flagged rows
          # need to be passed to the DT module
          if(input$select_flag != "all"){
            
            input_list$flag_df <- full_flag_table %>%
              filter(test_name == input$select_flag) %>%
              select(test_id, row_num) %>%
              rename(flag = test_id)
            
            # If all tests are selected, then the test ID number is used to set
            # priority, in case a row has > 1 flag. 
          } else {
            
            input_list$flag_df <- full_flag_table %>%
              group_by(row_num) %>%
              summarize(test_id = min(test_id)) %>%
              ungroup() %>%
              select(test_id, row_num) %>%
              rename(flag = test_id)
          }
          
        }
        
      })
      
      result_list_to_table <- function(results_list){
        
        # Convert the QC flag results list into a dataframe
        # Drop any tests that have no flags
        full_flag_table <- dplyr::bind_rows(
          purrr::compact(
            lapply(names(results_list), function(test_name){
              
              if(!is.null(results_list[[test_name]]$results)){
                
                tibble(
                  test_name = !!test_name,
                  test_id = results_list[[test_name]]$id,
                  row_num = results_list[[test_name]]$results
                )
                
              } else {
                return(NULL)
              }
            })
          )
        )
        
        return(full_flag_table)
        
      }
      
    }
  )
}