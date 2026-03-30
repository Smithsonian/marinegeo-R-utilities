# Contains all module UI and server code used on the application sidebar
# General settings (loaded filename, reorder column flag)
general_sidebar_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("filename")),
    uiOutput(ns("reorder_columns"))
  )
}

general_sidebar_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {

      output$filename <- renderUI({
        req(input_list$data_filename)
        div(input_list$data_filename)
      })

      output$reorder_columns <- renderUI({
        req(input_list$output_table_id)
        checkboxInput(session$ns("reorder_columns"), "Reorder columns", value = FALSE)
      })

      observeEvent(input$reorder_columns, {

        if (isTRUE(input$reorder_columns)) {
          req_cols <- marinegeo.utils::utl_mg_column_order(input_list$output_table_id)
          input_list$display_col_order <- c(
            intersect(req_cols, colnames(input_list$out_df)),
            setdiff(colnames(input_list$out_df), req_cols)
          )
        } else {
          input_list$display_col_order <- NULL
        }

      }, ignoreInit = TRUE)

    }
  )
}

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
    uiOutput(ns("structural_warnings")),  # structural test alerts
    uiOutput(ns("select_flag")),
    uiOutput(ns("hide_warn_toggle")),     # hide-warns checkbox
    uiOutput(ns("details_button"))
  )
}

qc_flag_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {

      qc_flags_list <- reactiveVal(NULL)

      QC_TEST_LABELS <- c(
        qc_check_columns = "Missing columns",
        qc_check_data_types = "Data type mismatches",
        qc_check_categorical_values = "Invalid categorical values",
        qc_check_missing_values = "Missing required values",
        qc_check_numeric_ranges = "Out-of-range numeric values",
        qc_check_lookup_values = "Unrecognized lookup values"
      )
      
      STRUCTURAL_TESTS <- c("qc_check_columns", "qc_check_data_types")
      
      utl_run_all_qc <- function(df, output_table) {
        qc_result <- marinegeo.utils::qc_run(df, output_table)
        marinegeo.utils::utl_qc_summarize(qc_result)
        # Returns list(summary = tibble(...), failures = tibble(...))
      }
      
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

      structural_tests <- reactive({
        req(qc_flags_list())
        qc_flags_list()$summary %>%
          filter(status %in% c("fail", "warn"), test %in% STRUCTURAL_TESTS)
      })

      row_level_tests <- reactive({
        req(qc_flags_list())
        qc_flags_list()$summary %>%
          filter(status %in% c("fail", "warn"), !test %in% STRUCTURAL_TESTS) %>%
          pull(test)
      })

      failures_to_flag_table <- function(qc_result) {
        
        # Build a lookup of test-level status for fallback
        test_status <- qc_result$summary %>%
          filter(!test %in% STRUCTURAL_TESTS) %>%
          select(test, status)

        qc_result$failures %>%
          filter(!test %in% STRUCTURAL_TESTS, !is.na(row_index)) %>%
          left_join(test_status, by = "test") %>%
          mutate(
            resolved_severity = dplyr::coalesce(severity, status),
            color_id = dplyr::if_else(resolved_severity == "warn", 2L, 1L),
            test_name = test,
            row_num   = as.integer(row_index)
          ) %>%
          select(test_name, color_id, row_num) %>%
          distinct()
      }

      output$select_flag <- renderUI({
        req(qc_flags_list())
        
        
        
        tests <- row_level_tests()

        if (length(tests) == 0) {
          flag_choices <- setNames("no_flags", "No row-level flags present")
        } else {
          labels <- dplyr::coalesce(QC_TEST_LABELS[tests], gsub("_", " ", tests))
          flag_choices <- setNames(c("all", tests), c("Show all flags", labels))
        }

        selectInput(session$ns("select_flag"), "Subset data by flag", choices = flag_choices)
      })

      output$hide_warn_toggle <- renderUI({
        req(qc_flags_list())

        if("row_index" %in% colnames(qc_flags_list()$failures)){
          
          has_warns <- any(
            failures_to_flag_table(qc_flags_list())$color_id == 2L
          )
          if (!has_warns) return(NULL)
          checkboxInput(session$ns("hide_warns"), "Hide warning-level flags", value = FALSE)
          
        } else return(NULL)
          
      })

      observe({

        req(input$select_flag)

        if (input$select_flag == "no_flags") {
          input_list$selected_flag <- "no_flags"
          return()
        }

        full_flag_table <- failures_to_flag_table(qc_flags_list())

        # Optionally hide warns
        hide_warns <- isTRUE(input$hide_warns)
        if (hide_warns) {
          full_flag_table <- full_flag_table %>% filter(color_id != 2L)
        }

        if (nrow(full_flag_table) == 0) {
          input_list$selected_flag <- "no_flags"
          return()
        }

        if (input$select_flag == "all") {
          input_list$flag_df <- full_flag_table %>%
            group_by(row_num) %>%
            summarize(flag = min(color_id), .groups = "drop")
          input_list$selected_flag <- "all"
        } else {
          input_list$flag_df <- full_flag_table %>%
            filter(test_name == input$select_flag) %>%
            select(flag = color_id, row_num)
          input_list$selected_flag <- input$select_flag
        }

      })

      output$structural_warnings <- renderUI({
        req(qc_flags_list())
        
        
        tests <- structural_tests()
        if (nrow(tests) == 0) return(NULL)

        tags$div(
          purrr::map(seq_len(nrow(tests)), function(i) {
            tags$div(
              class = "alert alert-warning",
              style = "font-size: 0.85em; padding: 6px 10px; margin-bottom: 4px;",
              tags$strong(QC_TEST_LABELS[tests$test[i]]), ": ", tests$message[i]
            )
          })
        )
      })

      ## Code to provide detail on flag violations
      output$details_button <- renderUI({
        req(input_list$selected_flag)

        # enforce conditions
        if (!is.null(input_list$selected_flag) &&
            input_list$selected_flag != "no_flags" &&
            input_list$selected_flag != "all") {

          actionButton(session$ns("show_details"), "Show Flag Details")
        }
      })

      # Functionality of the Button
      observeEvent(input$show_details, {

        #get the table with the QC test details
        table <- utl_get_qc_details(input_list$selected_flag, qc_flags_list())
        #Render the table in shiny format
        output$flag_table <- renderTable({
          table
        })

        #Create the pop-up.
        showModal(
          modalDialog(
            title = paste("Flag:", input_list$selected_flag),
            tableOutput(session$ns("flag_table")),
            easyClose = TRUE
          )
        )
      })

      utl_get_qc_details <- function(selected_flag, qc_result) {
        detail <- qc_result$failures %>%
          filter(test == selected_flag) %>%
          select(-row_index, -col_index) %>%
          distinct() %>%
          select(where(~ !all(is.na(.x))))

        if (nrow(detail) == 0) {
          return(tibble(issue = qc_result$summary %>%
                          filter(test == selected_flag) %>%
                          pull(message)))
        }
        detail
      }

    }
  )
}
