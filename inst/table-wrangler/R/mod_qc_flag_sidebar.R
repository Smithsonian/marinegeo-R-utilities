# Row-based QC tests are communicated in the application sidebar,
# and by assigning a unique color per flag type in the DT table.

# Assign tests to particular table types in utl_run_all_qc.R

# Module overview
# 1. The QC tests should only run when the input data frame is updated:
#     observeEvent run QC tests when input_list$out_df is updated
#     when num rows of input_list$out_df is 0 or is item is NULL, 
#     qc test list is set to NULL.
#
#    The existing flags dataframe is cleared to ensure that no rows are mistakenly flagged
#     when the new dataframe is displayed based on the previous table's results.
#
# 2. The "select_flag" sidebar UI will update to refresh which flags can be 
#     selected - this select input allows user to only code rows by color for 
#     all or a single QC test. The default is to assign a color for all QC results. 
#
# 3. An observer refreshes the flags dataframe that is passed to the DT module based on updates
#     at (1) and (2). Sometimes, (1) may update but not (2) in situations where the selected flag
#     did not change
#     (i.e., when "all flags" is selected and the data is changed and all flags remains selected).

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
      
      qc_flags_list <- reactiveVal(NULL)
      reassign_flags_counter <- reactiveVal(0)
      
      observeEvent(input_list$out_df, {
        
        # Reset the flag dataframe
        input_list$flag_df = tibble(
          flag = NA_character_,
          row_num = NA_integer_,
          .rows = 0
        )
        
        if(is.null(input_list$out_df)){
          qc_flags_list(NULL)
          
        } else if(nrow(input_list$out_df) > 0){
          qc_flags_list(utl_run_all_qc(input_list$out_df, input_list$output_table_id))
          
        } else {
          qc_flags_list(NULL)
        }
        
      })
      
      output$select_flag <- renderUI({
        
        req(qc_flags_list())
        
        flags_list <- qc_flags_list()
        
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
        
      })
      
      observe({
        
        req(input$select_flag)
        
        if(input$select_flag != "no_flags"){
          
          results_list <- qc_flags_list()
          
          # Convert the QC flag results list into a dataframe
          # Drop any tests that have no flags
          full_flag_table <- result_list_to_table(results_list)
          
          # if there are no flags, then remove existing flag dataframe
          # Necessary when you load a second table with no flags, but the 
          # first had flags
          if(nrow(full_flag_table) == 0){          
            input_list$selected_flag <- "no_flags"
            
          # If a single test is selected, only those flagged rows
          # need to be passed to the DT module
          } else if(input$select_flag != "all"){
            
            input_list$flag_df <- full_flag_table %>%
              filter(test_name == input$select_flag) %>%
              select(test_id, row_num) %>%
              rename(flag = test_id)
            
            input_list$selected_flag <- input$select_flag
            
            # If all tests are selected, then the test ID number is used to set
            # priority, in case a row has > 1 flag.
          } else {
            
            input_list$flag_df <- full_flag_table %>%
              group_by(row_num) %>%
              summarize(test_id = min(test_id)) %>%
              ungroup() %>%
              select(test_id, row_num) %>%
              rename(flag = test_id)
            
            input_list$selected_flag <- input$select_flag
          }
        } else {
          input_list$selected_flag <- "no_flags"
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