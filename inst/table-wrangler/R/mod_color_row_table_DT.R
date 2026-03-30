DT_table_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("table"))
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

        if (!is.null(input_list$display_col_order)) {
          # ordered_data_cols <- c(
          #   intersect(input_list$display_col_order, colnames(plot_df)),
          #   setdiff(colnames(plot_df), c("flag", input_list$display_col_order))
          # )
          # plot_df <- plot_df %>% select(flag, all_of(ordered_data_cols))
          plot_df <- plot_df %>% 
            select(any_of(input_list$display_col_order), everything())
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
                          backgroundColor = DT::styleEqual(
                            1:2,
                            c("#f96161", "#fff3cd")
                            #  fail       warn
                          ))

      })

      # Update `input_list` with the latest selected cells, including column names of selected cells
      observeEvent(input$table_cells_selected, {

        if(nrow(input$table_cells_selected) > 0){
          column_indices <- unique(input$table_cells_selected[,2])
          display_cols <- if (!is.null(input_list$display_col_order)) {
            colnames(
              input_list$out_df %>% 
                select(any_of(input_list$display_col_order), everything())
            )
          } else {
            colnames(input_list$out_df)
          }
          column_names <- display_cols[column_indices]
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
