#seagrass sheath and epibiont monitoring visualizations

oyster_habitat_complexity_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("sample_event_select_ui")),
    layout_column_wrap(
      card(card_header("Rugosity Barplot"),
           full_screen = T,
           plotOutput(ns("rugosity_barplot"))
      ),
      # card(card_header("Rugosity Timeseries"),
      #      full_screen = T,
      #      plotOutput(ns("rugosity_timeseries"))
      # ),
      card(card_header("Max Reef Height Barplot"),
           full_screen = T,
           plotOutput(ns("max_reef_height_barplot"))
      )#,
      # card(card_header("Max Reef Height Timeseries"),
      #      full_screen = T,
      #      plotOutput(ns("max_reef_height_timeseries"))
      # )
    )
  )
}


oyster_habitat_complexity_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
    load_additional_oyster <- reactive({
      df <- bind_rows(
        marinegeo.utils::db_arrow_marinegeo(input_list$output_table_id) %>%
          filter(input_filename != input_list$data_filename,
                 partner_code %in% unique(input_list$out_df$partner_code)) %>%
          collect(),
        input_list$out_df
      ) %>%
        arrange(year(sample_collection_date)) %>%
        mutate(year = year(sample_collection_date))
      
      df
    })
    
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
      input_list$out_df %>% filter(sample_event_id == selected)
    })
    
    output$rugosity_barplot <- renderPlot({
      barplot_data() %>%
        mutate(quadrat = as.factor(quadrat)) %>%
        select(site_name, transect, quadrat, rugosity, max_cluster_height_cm) %>%
        ggplot(aes(quadrat, rugosity)) +
        geom_col() +
        facet_wrap(site_name ~ transect)
    })
    
    # output$rugosity_timeseries <- renderPlot({
    #   load_additional_oyster() %>%
    #     group_by(transect)%>%
    #     mutate(average_rugosity = mean(rugosity, na.rm = TRUE))%>%
    #     ungroup()%>%
    #     ggplot(aes(as.factor(year), average_rugosity)) 
    # })
    
    output$max_reef_height_barplot <- renderPlot({
      barplot_data() %>%
        mutate(quadrat = as.factor(quadrat)) %>%
        select(site_name, transect, quadrat, rugosity, max_cluster_height_cm) %>%
        ggplot(aes(quadrat, max_cluster_height_cm)) +
        geom_col() +
        facet_wrap(site_name ~ transect)
    })
    
    # output$max_reef_height_timeseries <- renderPlot({
    #   load_additional_oyster() %>%
    #     group_by(transect)%>%
    #     mutate(average_max_cluster_height_cm = mean(max_cluster_height_cm, na.rm = TRUE))%>%
    #     ungroup()%>%
    #     ggplot(aes(as.factor(year), average_max_cluster_height_cm)) 
    # })
    
  })
}
