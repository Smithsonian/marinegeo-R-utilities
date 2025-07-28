DT_table_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    actionButton(ns("clear_cells"), "Clear Selected Cells"),
    DTOutput(ns("table")),
    
  )
}

DT_table_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$table <- renderDT({
        
        req(input_list$selected_flag)
        
        if(input_list$selected_flag %in% c("all", "no_flags")){
          
          plot_df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            select(-row_num, flag, everything())
          
        } else {
          
          plot_df <- input_list$out_df %>%
            rownames_to_column("row_num") %>%
            mutate(row_num = as.numeric(row_num)) %>%
            left_join(input_list$flag_df, by = "row_num") %>%
            filter(!is.na(flag)) %>%
            select(-row_num, flag, everything())
        }
        
        plot_df %>%
          DT::datatable(
            style = "default",
            selection = list(target = 'cell'),
            # Hide the flag column, only used for color
            options = list(columnDefs = list(list(visible=FALSE, targets=c("flag"))),
                           pageLength = 50)
          ) %>%
          # Color rows based on flag
          DT::formatStyle("flag", target = 'row', 
                          backgroundColor = DT::styleEqual(1:5, c("#e5c3c6", "#e1e9b7", "#f96161", "#bcd2d0", "#d0b783")))
        
      })
      
      # Create a proxy DT object, which allows dynamic updates to table without regenerating entire table
      # Such as when selected cells are cleared
      proxy <- DT::dataTableProxy("table")
      
      # When clear cells button is clicked, remove selections
      observeEvent(input$clear_cells, {
        proxy %>% selectCells(NULL)
      })
      
      # Update `input_list` with the latest selected cells, including column names of selected cells
      observeEvent(input$table_cells_selected, {
        
        if(nrow(input$table_cells_selected) > 0){
          column_indices <- unique(input$table_cells_selected[,2])
          column_names <- colnames(input_list$out_df)[column_indices]
        } else {
          column_names <- NULL
        }
        
        input_list$table_selections <- list(
          
          # A matrix
          selected_cells = input$table_cells_selected,
          # A vector of column names from `df`
          selected_columns = column_names
          
        )
        
      })
      
    }
  )
}