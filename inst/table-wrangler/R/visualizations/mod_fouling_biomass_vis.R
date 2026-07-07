### template UI and server functions for a visualization of a specific data type


fouling_biomass_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("year_select_ui")),
    layout_column_wrap(
      card(
        card_header("Community Biomass Barplot"),
        full_screen = TRUE,
        plotOutput(ns("fouling_biomass_barplot"))
      ),
      card(
        card_header("Biomass Timeseries Line Plot"),
        full_screen = TRUE,
        plotlyOutput(ns("fouling_biomass_timeseries_lineplot"))
      )
    )
  )
}


fouling_biomass_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
    load_additional_fouling <- reactive({
      df <- bind_rows(
        marinegeo.utils::db_arrow_marinegeo(input_list$output_table_id) %>%
          filter(input_filename != input_list$data_filename,
                 partner_code %in% unique(input_list$out_df$partner_code)) %>%
          collect(),
        input_list$out_df
      ) %>%
        arrange(year(retrieval_date)) %>%
        mutate(year = year(retrieval_date))
      
      df
    })
    
    output$year_select_ui <- renderUI({
      req(input_list$out_df)
      years <- sort(unique(year(input_list$out_df$retrieval_date)))
      req(length(years) > 1)
      selectInput(
        session$ns("year_select"),
        label = "Year Sampled",
        choices = years
      )
    })
    
    barplot_data <- reactive({
      req(input_list$out_df)
      years <- sort(unique(year(input_list$out_df$retrieval_date)))
      selected <- if (length(years) <= 1) years[[1]] else input$year_select
      req(selected)
      input_list$out_df %>% filter(year(retrieval_date) == selected)
    })
    
    output$fouling_biomass_barplot <- renderPlot({
      barplot_data() %>%
        ggplot(aes(panel_id, community_biomass_g, fill = site_name)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    output$fouling_biomass_timeseries_lineplot <- renderPlotly({
      functional_groups <- c("Algae", "Seagrass")

      df <- load_additional_fouling() 
      
      df_viz <- load_additional_fouling() %>%
        group_by(year, site_name) %>%
        summarize(community_biomass_g = mean(community_biomass_g, na.rm = T))

      plot <- marinegeo.utils::viz_mg_timeseries_annual(
        df = df_viz,
        y_var = "community_biomass_g",
        x_var = "year",
        y_label = "Mean Community Biomass",
        y_grouping_var = "site_name"
      )

      marinegeo.utils::viz_mg_ggplotly(plot)

    })
  })
}
