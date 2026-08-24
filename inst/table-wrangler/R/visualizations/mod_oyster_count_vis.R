#seagrass shoot cont monitoring visualizations


oyster_count_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("sample_event_select_ui")),
    layout_column_wrap(
      card(
        card_header("Mollusk Count Barplot"),
        full_screen = TRUE,
        plotOutput(ns("oyster_density_barplot"))
      )
    )
  )
}



oyster_count_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
    # load_additional_oyster <- reactive({
    #   df <- bind_rows(
    #     marinegeo.utils::db_arrow_marinegeo(input_list$output_table_id) %>%
    #       filter(input_filename != input_list$data_filename,
    #              partner_code %in% unique(input_list$out_df$partner_code)) %>%
    #       collect(),
    #     input_list$out_df
    #   ) %>%
    #     arrange(year(sample_collection_date)) %>%
    #     mutate(year = year(sample_collection_date))
    #   
    #   df
    # })
    
    output$sample_event_select_ui <- renderUI({
      req(input_list$out_df)
      events <- sort(unique(input_list$out_df$sample_event_id))
      req(length(events) > 1)
      selectInput(
        session$ns("sample_event_select"),
        label = "Sample Event",
        choices = events
      )
    })
    
    barplot_data <- reactive({
      req(input_list$out_df)
      events <- sort(unique(input_list$out_df$sample_event_id))
      selected <- if (length(events) <= 1) events[[1]] else input$sample_event_select
      req(selected)
      input_list$out_df %>% 
        mutate(bivalve_id = paste(scientific_name, live_or_box))%>%
        filter(sample_event_id == selected)
    })
    
    output$oyster_density_barplot <- renderPlot({
      barplot_data() %>%
        ggplot(aes(transect, bivalve_density_m2, fill = bivalve_id)) +
        geom_col() 
      })
    
  })
}
