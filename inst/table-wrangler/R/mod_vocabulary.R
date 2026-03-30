vocabulary_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("search"), "Fuzzy search", placeholder = "Search across all columns..."),
    navset_card_tab(
      id = ns("vocab_tabs"),
      nav_panel("Partner Codes", DTOutput(ns("partner_codes_tbl"))),
      nav_panel("Site Codes", DTOutput(ns("site_codes_tbl"))),
      nav_panel("Observation Lookup", DTOutput(ns("obs_lookup_tbl"))),
      nav_panel("Categorical Vocab", DTOutput(ns("cat_vals_tbl"))),
      nav_panel("Numeric Ranges", DTOutput(ns("num_range_tbl"))),
      
    )
  )
}

vocabulary_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    partner_codes <- utl_mg_get_registry("partner_codes")
    site_codes <- utl_mg_get_registry("site_codes")
    obs_lookup <- utl_mg_get_registry("observation_lookup")
    categorical_vocab <- utl_mg_get_registry("categorical_values")
    numeric_ranges <- utl_mg_get_registry("numeric_ranges")

    active_registry <- reactive({
      switch(input$vocab_tabs,
        "Partner Codes" = partner_codes,
        "Site Codes" = site_codes,
        "Observation Lookup" = obs_lookup,
        "Categorical Vocab" = categorical_vocab,
        "Numeric Ranges" = numeric_ranges,
        partner_codes
      )
    })

    search_debounced <- debounce(reactive(input$search), 300)

    filtered_registry <- reactive({
      df <- active_registry()
      term <- trimws(search_debounced())

      if (is.null(term) || nchar(term) == 0) return(df)

      row_matches <- apply(df, 1, function(row) {
        any(lengths(agrep(term, as.character(row),
                          ignore.case = TRUE,
                          max.distance = 0.3,
                          value = FALSE)) > 0)
      })

      df[row_matches, , drop = FALSE]
    })

    render_tbl <- function(registry_name) {
      renderDT({
        req(input$vocab_tabs == registry_name)
        filtered_registry()
      },
      rownames = FALSE,
      options = list(
        dom = "tip",
        scrollX = TRUE,
        pageLength = 25
      ))
    }

    output$partner_codes_tbl <- render_tbl("Partner Codes")
    output$site_codes_tbl <- render_tbl("Site Codes")
    output$obs_lookup_tbl <- render_tbl("Observation Lookup")
    output$cat_vals_tbl <- render_tbl("Categorical Vocab")
    output$num_range_tbl <- render_tbl("Numeric Ranges")
    
  })
}
