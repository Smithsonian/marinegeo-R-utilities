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
        checkboxInput(
          session$ns("reorder_columns"),
          "Reorder columns",
          value = FALSE
        )
      })

      observeEvent(
        input$reorder_columns,
        {
          if (isTRUE(input$reorder_columns)) {
            req_cols <- marinegeo.utils::utl_mg_column_order(
              input_list$output_table_id
            )
            input_list$display_col_order <- c(
              intersect(req_cols, colnames(input_list$out_df)),
              setdiff(colnames(input_list$out_df), req_cols)
            )
          } else {
            input_list$display_col_order <- NULL
          }
        },
        ignoreInit = TRUE
      )
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
    uiOutput(ns("structural_warnings")), # structural test alerts
    uiOutput(ns("select_flag")),
    uiOutput(ns("hide_warn_toggle")), # hide-warns checkbox
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
        # Returns a qc_issues tibble: one row per problem, with columns
        # check / severity / row / column / col_index / value / issue.
        marinegeo.utils::qc_run(df, output_table)
      }

      observeEvent(input_list$out_df, {
        # Reset the flag dataframe
        input_list$flag_df <- tibble(
          flag = NA_character_,
          row_num = NA_integer_,
          .rows = 0
        )

        if (is.null(input_list$out_df)) {
          qc_flags_list(NULL)
        } else if (nrow(input_list$out_df) > 0) {
          qc_flags_list(utl_run_all_qc(
            input_list$out_df,
            input_list$output_table_id
          ))
        } else {
          qc_flags_list(NULL)
        }
      })

      # Structural problems (missing columns, type mismatches): one row each.
      structural_issues <- reactive({
        req(qc_flags_list())
        qc_flags_list() %>%
          filter(check %in% STRUCTURAL_TESTS)
      })

      # Distinct row-level checks that produced at least one flag.
      row_level_tests <- reactive({
        req(qc_flags_list())
        qc_flags_list() %>%
          filter(!check %in% STRUCTURAL_TESTS, !is.na(row)) %>%
          pull(check) %>%
          unique()
      })

      failures_to_flag_table <- function(issues) {
        # severity is always present on issue rows, so colour directly.
        issues %>%
          filter(!check %in% STRUCTURAL_TESTS, !is.na(row)) %>%
          mutate(
            color_id = dplyr::if_else(severity == "warn", 2L, 1L),
            test_name = check,
            row_num = as.integer(row)
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
          labels <- dplyr::coalesce(
            QC_TEST_LABELS[tests],
            gsub("_", " ", tests)
          )
          flag_choices <- setNames(c("all", tests), c("Show all flags", labels))
        }

        selectInput(
          session$ns("select_flag"),
          "Subset data by flag",
          choices = flag_choices
        )
      })

      output$hide_warn_toggle <- renderUI({
        req(qc_flags_list())

        flag_table <- failures_to_flag_table(qc_flags_list())
        if (nrow(flag_table) == 0) {
          return(NULL)
        }

        has_warns <- any(flag_table$color_id == 2L)
        if (!has_warns) {
          return(NULL)
        }
        checkboxInput(
          session$ns("hide_warns"),
          "Hide warning-level flags",
          value = FALSE
        )
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

        issues <- structural_issues()
        if (nrow(issues) == 0) {
          return(NULL)
        }

        issue_labels <- c(
          missing_column = "Missing column",
          unexpected_column = "Unexpected column",
          wrong_order = "Wrong column order",
          type_mismatch = "Type mismatch"
        )

        # One alert box per check; each bullet describes a flagged column so the
        # sidebar stays compact when a check flags many columns.
        tags$div(
          purrr::map(unique(issues$check), function(chk) {
            sub <- issues[issues$check == chk, , drop = FALSE]
            labels <- unname(issue_labels[sub$issue])
            labels[is.na(labels)] <- sub$issue[is.na(labels)]
            bullets <- paste0(labels, ": ", sub$column)
            tm <- sub$issue == "type_mismatch" & !is.na(sub$value)
            bullets[tm] <- paste0(bullets[tm], " (found ", sub$value[tm], ")")

            tags$div(
              class = "alert alert-warning",
              style = "font-size: 0.85em; padding: 6px 10px; margin-bottom: 4px;",
              tags$strong(QC_TEST_LABELS[chk]),
              tags$ul(
                style = "margin: 4px 0 0 0; padding-left: 18px;",
                purrr::map(bullets, tags$li)
              )
            )
          })
        )
      })

      ## Code to provide detail on flag violations
      output$details_button <- renderUI({
        req(input_list$selected_flag)

        # enforce conditions
        if (
          !is.null(input_list$selected_flag) &&
            input_list$selected_flag != "no_flags" &&
            input_list$selected_flag != "all"
        ) {
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

      utl_get_qc_details <- function(selected_flag, issues) {
        detail <- issues %>%
          filter(check == selected_flag) %>%
          select(column, value, issue) %>%
          distinct() %>%
          select(where(~ !all(is.na(.x))))

        if (nrow(detail) == 0) {
          return(tibble(issue = "No additional details available."))
        }
        detail
      }
    }
  )
}
