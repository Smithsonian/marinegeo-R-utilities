load_marinegeo_data_UI <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      card(
        card_header("Load Data"),
        uiOutput(ns("select_data_type")),
        uiOutput(ns("select_grouping")),
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

load_marinegeo_data_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      loaded_bundle <- reactiveVal(NULL)

      ## Load data sources from JSON ####
      data_sources_config <- jsonlite::read_json("data_sources.json")$data_sources

      ## Filter to data sources that have at least one available directory ####
      available_data_sources <- Filter(function(ds) {
        any(sapply(ds$groupings, function(group) {
          dir.exists(file.path(Sys.getenv("repository_filepath"), group$location_path))
        }))
      }, data_sources_config)

      ## Helpers ####
      # Multiple groupings of data may exist per data type
      # Only return groupings that users have access to
      get_groupings_for_type <- function(data_type) {

        ds <- Filter(function(x) x$data_type == data_type, available_data_sources)
        if (length(ds) == 0) return(list())
        groupings <- ds[[1]]$groupings
        # Keep only groupings whose directories exist, sorted by grouping_order
        available <- Filter(function(g) {
          dir.exists(file.path(Sys.getenv("repository_filepath"), g$location_path))
        }, groupings)
        
        return(
          available[order(sapply(available, function(g) g$grouping_order))]
        )
      }

      # Return the list of parameters for the selected data grouping
      get_grouping_obj <- function(data_type, grouping_name) {

        groupings <- get_groupings_for_type(data_type)
        matches <- Filter(function(g) g$grouping_name == grouping_name, groupings)
        
        if (length(matches) == 0){
          return(NULL)
        } else {
          return(matches[[1]])
        }
      }

      ## Selected grouping reactive ####
      selected_grouping <- reactive({
        
        req(input$select_data_type, input$select_grouping)
        
        grouping_obj <- get_grouping_obj(input$select_data_type, input$select_grouping)
        return(grouping_obj)
      })

      #### UI outputs ####
      output$select_data_type <- renderUI({

        data_types <- sapply(available_data_sources, function(x) x$data_type)

        pickerInput(session$ns("select_data_type"),
                    "Select Data Type",
                    choices = data_types,
                    options = pickerOptions(container = "body"),
                    width = "100%")
      })

      output$select_grouping <- renderUI({
        req(input$select_data_type)

        groupings <- get_groupings_for_type(input$select_data_type)
        grouping_names <- sapply(groupings, function(g) g$grouping_name)

        pickerInput(session$ns("select_grouping"),
                    "Select Data Source",
                    choices = grouping_names,
                    options = pickerOptions(container = "body"),
                    width = "100%")
      })

      get_data_dir_inventory <- reactive({
        req(input$select_grouping)

        grouping <- selected_grouping()
        req(grouping)

        dir_path <- file.path(Sys.getenv("repository_filepath"), grouping$location_path)

        filepath_directory <- list.files(dir_path, full.names = TRUE, recursive = TRUE)
        file_directory     <- list.files(dir_path, recursive = TRUE)

        # Filter to accepted file types from JSON
        file_types <- unlist(grouping$file_types)
        if (length(file_types) > 0) {
          pattern <- paste0("\\.(", paste(file_types, collapse = "|"), ")$")
          keep <- grepl(pattern, filepath_directory, ignore.case = TRUE)
          filepath_directory <- filepath_directory[keep]
          file_directory     <- file_directory[keep]
        }

        files_df <- tibble(
          filepath       = filepath_directory,
          filename       = basename(filepath_directory),
          local_filepath = file_directory
        ) %>%
          mutate(local_filepath = case_when(
            str_remove(local_filepath, filename) == "" ~ local_filepath,
            TRUE ~ str_remove(local_filepath, filename)
          ))
        
        return(files_df)
      })

      output$select_file <- renderUI({

        req(input$select_grouping)
        
        files_df <- get_data_dir_inventory()
        grouping <- selected_grouping()
        label_strategy <- grouping$label_strategy$type

        if (label_strategy == "subdirectory") {
          # Group files under their subdirectory folder names as labels
          file_list <- files_df %>%
            select(local_filepath, filename) %>%
            split(., .[, "local_filepath"]) %>%
            lapply(function(x) x %>% pull(filename)) %>%
            lapply(as.list)
        } else {
          file_list <- files_df$filename
        }

        pickerInput(session$ns("select_file"),
                    "Select File to Load",
                    choices = file_list,
                    options = pickerOptions(container = "body",
                                            liveSearch = TRUE),
                    width = "100%")
      })

      output$select_excel_sheet <- renderUI({
        req(input$select_file)

        if (str_ends(input$select_file, "\\.(xlsx|xls)")) {
          filepath <- get_data_dir_inventory() %>%
            filter(filename == input$select_file) %>%
            pull(filepath)

          sheet_names <- readxl::excel_sheets(filepath)

          # Default to the first sheet in default_sheet_mappings, fall back to sheet_names[1]
          grouping <- selected_grouping()
          default_sheet_mappings <- grouping$mappings$default_sheet_mappings

          default_option <- if (!is.null(default_sheet_mappings) && length(default_sheet_mappings) > 0) {
            first_mapped_sheet <- default_sheet_mappings[[1]]$sheet_name
            if (first_mapped_sheet %in% sheet_names) first_mapped_sheet else sheet_names[1]
          } else {
            sheet_names[1]
          }

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

        grouping <- selected_grouping()
        table_ids <- unlist(grouping$mappings$table_ids)

        pickerInput(session$ns("select_output_data_table"),
                    "Select Output Table",
                    choices = table_ids,
                    options = pickerOptions(container = "body"),
                    width = "100%")
      })

      # When user changes the Excel sheet, auto-select the matching output table
      observeEvent(input$select_excel_sheet, {
        req(input$select_excel_sheet)

        grouping <- selected_grouping()
        default_sheet_mappings <- grouping$mappings$default_sheet_mappings

        if (!is.null(default_sheet_mappings)) {
          match <- Filter(
            function(m) m$sheet_name == input$select_excel_sheet,
            default_sheet_mappings
          )
          if (length(match) > 0) {
            updatePickerInput(session, "select_output_data_table",
                              selected = match[[1]]$table_id)
          }
        }
      })

      #### Load data into application ####
      observeEvent(input$load_selected_data, {

        #browser()
        
        data_filename <- input$select_file

        filepath <- get_data_dir_inventory() %>%
          filter(filename == data_filename) %>%
          pull(filepath)

        filepath <- normalizePath(filepath, winslash = "/")
        
        if (str_ends(data_filename, ".xlsx") | str_ends(data_filename, ".xls")) {
          df <- readxl::read_excel(filepath, sheet = input$select_excel_sheet)
          target_excel_sheet <- input$select_excel_sheet
          
        } else if (str_ends(data_filename, ".csv")) {
          df <- readr::read_csv(filepath)
          target_excel_sheet <- NULL
        }

        project_dir_value <- get_data_dir_inventory() %>%
          filter(filename == data_filename) %>%
          mutate(local_filepath = case_when(
            local_filepath == filename ~ NA,
            TRUE ~ local_filepath
          )) %>%
          pull(local_filepath)

        script_filepath_value <- NULL

        # Set up the code chain
        script_filename <- get_script_filepath()

        if(length(script_filename) == 0) {

          grouping <- selected_grouping()

          short_name  <- get_short_name_for_table(input$select_output_data_table)
          script_base <- gsub("\\.(xlsx|xls|csv)$", ".R", data_filename)
          if (!is.null(short_name)) script_base <- paste0(short_name, "_", script_base)

          script_filepath_value <- file.path(
            Sys.getenv("repository_filepath"),
            grouping$script_path,
            input$select_output_data_table,
            script_base
          )

          if (!file.exists(script_filepath_value)) {

            local_data_filepath <- gsub(Sys.getenv("repository_filepath"), "", filepath)

            create_template_script(
              script_filepath = script_filepath_value,
              target_table    = input$select_output_data_table,
              input_filepath  = local_data_filepath,
              excel_sheet = target_excel_sheet,
              template_type   = grouping$template_type
            )
            
          } else {

            # A filename exists but does not contain the correct filepath or table ID!
            showModal(modalDialog(
              title = "Error loading R script!",
              paste0(script_filepath_value, " already exists but does not have the correct input filepath and/or table ID variables set.")
            ))

          }

        } else {
          
          script_filepath_value <- script_filename

        }
          
        i_am_error <- FALSE

        tryCatch({

          script_lines <- readLines(script_filepath_value)

          start_line <- grep("## MarineGEO Table Wrangler Start", script_lines)
          end_line   <- grep("## MarineGEO Table Wrangler End", script_lines)

          script_excerpt <- script_lines[(start_line + 1):(end_line - 1)]

          input_file_path <- filepath
          req_cols <- marinegeo.utils::utl_mg_column_order(input$select_output_data_table)
          table_out <- input$select_output_data_table
            
          parsed_text <- parse(text = script_excerpt)
          eval(parsed_text)

        }, error = function(e) {

          i_am_error    <<- TRUE
          error_message <<- e$message

        })

        if (i_am_error) {

          showModal(modalDialog(
            title = "Error running R script! Only unprocessed data loaded!",
            error_message
          ))
            
          df_out <- df
        }
        
        loaded_bundle(list(
          in_df = df,
          out_df = df_out,
          data_filepath = filepath,
          data_filename = data_filename,
          project_directory = project_dir_value,
          output_table_id = input$select_output_data_table,
          script_filepath = script_filepath_value,
          output_req_cols = marinegeo.utils::utl_mg_column_order(input$select_output_data_table)
        ))

      })

      ## Load R script associated with data ####
      # Searches recursively through the grouping's script_path directory for an R script
      # that contains both an input_file_path line matching the selected file and a
      # table_out line matching the selected output table ID.
      get_script_filepath <- reactive({

        req(input$select_output_data_table, input$select_file)

        grouping <- selected_grouping()
        req(grouping, grouping$script_path)

        scripts_dir <- file.path(Sys.getenv("repository_filepath"), grouping$script_path)

        if (!dir.exists(scripts_dir)) return(character(0))

        all_scripts <- list.files(scripts_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

        if (length(all_scripts) == 0) return(character(0))

        selected_file  <- input$select_file
        selected_table <- input$select_output_data_table
        table_pattern  <- paste0('table_out\\s*<-\\s*["\']', selected_table, '["\']')

        matching_script <- NULL
        for (script in all_scripts) {
          lines     <- tryCatch(readLines(script, warn = FALSE), error = function(e) character(0))
          has_file  <- any(grepl("input_file_path", lines, fixed = TRUE) &
                           grepl(selected_file, lines, fixed = TRUE))
          has_table <- any(grepl(table_pattern, lines))
          if (has_file && has_table) {
            matching_script <- script
            break
          }
        }

        if (is.null(matching_script)) return(character(0))
        return(matching_script)

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

        if (length(get_script_filepath()) == 0) {
          div("No R Script exists for this file")

        } else {

          script_lines <- readLines(get_script_filepath())

          if (length(script_lines) == 0) {
            return(p("Empty file"))
          }

          if (show_full()) {
            full_text <- paste(script_lines, collapse = "\n")
            pre(code(full_text), class = "r")
          } else {
            preview_lines <- min(10, length(script_lines))
            preview_text  <- paste(script_lines[1:preview_lines], collapse = "\n")

            if (length(script_lines) > preview_lines) {
              preview_text <- paste0(preview_text, "\n...[", length(script_lines) - preview_lines, " more lines]")
            }

            pre(code(preview_text), class = "r")
          }

        }
      })

      return(loaded_bundle)

    }
  )
}
