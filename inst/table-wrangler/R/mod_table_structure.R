table_structure_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(
      card_header("Selected Column Summary"),
      card_body(
        uiOutput(ns("column_summary"))
      ),
      full_screen = T
    ),
    
    card(
      card_header("Table Summary"),
      card_body(
        DTOutput(ns("table_summary"))
      ),
      full_screen = T
    )
    
  )
}

table_structure_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Column Summary Output
      output$column_summary <- renderUI({

        req(input_list$table_selections$selected_columns)

        summary_list <- lapply(input_list$table_selections$selected_columns, function(col_name) {

          col_data <- input_list$out_df[[col_name]]
          col_type <- class(col_data)[1]

          # Create summary based on data type
          if (is.numeric(col_data)) {
            summary_content <- div(
              h5(strong(col_name)),
              p("Data Type: ", col_type),
              p("Min: ", min(col_data, na.rm = TRUE)),
              p("Max: ", max(col_data, na.rm = TRUE)),
              p("NA Values: ", sum(is.na(col_data))),
              hr()
            )
          } else {
            unique_vals <- unique(col_data)
            unique_display <- if(length(unique_vals) <= 10) {
              paste(unique_vals, collapse = ", ")
            } else {
              paste(paste(unique_vals[1:10], collapse = ", "), "... (showing first 10)")
            }

            summary_content <- div(
              h5(strong(col_name)),
              p("Data Type: ", col_type),
              p("Unique Values (", length(unique_vals), "): ", unique_display),
              p("NA Values: ", sum(is.na(col_data))),
              hr()
            )
          }

          return(summary_content)
        })

        do.call(tagList, summary_list)
      })
      
      # All Columns Table Output
      output$table_summary <- renderDT({
        
        req(input_list$out_df)
        
        output_column_order <- utl_mg_column_order(input_list$output_table_id)
        
        missing_columns <- tibble(
          Column = setdiff(output_column_order, colnames(input_list$out_df)),
          Status = "Missing Column",
          .rows = length(setdiff(output_column_order, colnames(input_list$out_df)))
        )
        
        invalid_columns <- case_when(
          colnames(input_list$out_df) %in% setdiff(colnames(input_list$out_df), output_column_order) ~ "column not in target table",
          T ~ ""
        )
        
        # Create a dataframe with column information
        columns_info <- data.frame(
          Column = names(input_list$out_df),
          Status = invalid_columns,
          `Data Type` = sapply(input_list$out_df, function(x) class(x)[1]),
          `Non-NA Count` = sapply(input_list$out_df, function(x) sum(!is.na(x))),
          `NA Count` = sapply(input_list$out_df, function(x) sum(is.na(x))),
          `Unique Values` = sapply(input_list$out_df, function(x) length(unique(x))),
          stringsAsFactors = FALSE
        )
        
        columns_info <- bind_rows(
          missing_columns,
          columns_info
        )
        
        datatable(
          columns_info,
          options = list(
            pageLength = nrow(columns_info),
            scrollX = TRUE,
            dom = 't'  # Only show table, no search/pagination controls for cleaner look
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = 1:ncol(columns_info),
            backgroundColor = '#f8f9fa',
            fontSize = '14px'
          )
      })
      
    }
  )
}