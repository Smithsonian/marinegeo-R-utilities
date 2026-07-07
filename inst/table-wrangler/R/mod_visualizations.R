# Steps to add new visualizations
# 
# 1. If adding visualizations to a table that doesn't have any pre-existing visualizations:
#   - Add the table name under the "Table plot list" header
#   - Add a new function to orchestrate table functions
#     - Some tables may share a function (see Oyster 2025 experiment)
#     - Arguments passed to the function can vary across table types

visualizations_UI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("vizualization_page"))
}

visualizations_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {

    initialized_ids <- reactiveVal(character(0))

    vis_fns <- reactive({
      req(input_list$output_table_id)

      base <- gsub("-", "_", input_list$output_table_id)

      ui_fn <- tryCatch(get(paste0(base, "_vis_UI"), mode = "function"), error = function(e) NULL)
      server_fn <- tryCatch(get(paste0(base, "_vis_server"), mode = "function"), error = function(e) NULL)

      list(base = base, ui = ui_fn, server = server_fn)
    })

    output$vizualization_page <- renderUI({
      fns <- vis_fns()
      shiny::validate(
        shiny::need(!is.null(fns$ui), "No visualization is available for this data table.")
      )
      fns$ui(session$ns(fns$base))
    })

    observeEvent(vis_fns(), {
      fns <- vis_fns()
      if (is.null(fns$server)) return()
      if (fns$base %in% initialized_ids()) return()
      fns$server(fns$base, input_list)
      initialized_ids(c(initialized_ids(), fns$base))
    })

  })
}



