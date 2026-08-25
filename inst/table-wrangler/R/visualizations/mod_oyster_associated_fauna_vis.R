### template UI and server functions for a visualization of a specific data type


oyster_biobox_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("sample_event_select_ui")),
    layout_column_wrap(
      card(
        card_header("Associated Fauna Composition Barplot"),
        full_screen = TRUE,
        plotOutput(ns("associated_fauna_barplot"))
      )
    )
  )
}


oyster_biobox_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {

    
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
      input_list$out_df%>% filter(sample_event_id == selected)
    })
    
    output$associated_fauna_barplot <- renderPlot({
      barplot_data() %>%
        ggplot(aes(site_name, biobox_count, fill = scientific_name)) +
        geom_col()
    })
      
    })
  }

