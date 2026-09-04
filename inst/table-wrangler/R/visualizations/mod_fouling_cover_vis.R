### template UI and server functions for a visualization of a specific data type


fouling_cover_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("year_select_ui")),
    layout_column_wrap(
      card(
        card_header("Morpho-functional Group Cover Barplot"),
        full_screen = TRUE,
        plotOutput(ns("fouling_cover_barplot"))
      ),
      card(
        card_header("Morpho-functional Group Cover Timeseries"),
        full_screen = TRUE,
        plotlyOutput(ns("fouling_cover_timeseries_lineplot"))
      )
    )
  )
}



fouling_cover_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
    groups <- c("Amphipod tubes", "Anemone", "Arborescent bryozoan", "Barnacles", 
                "Colonial ascidian", "Crepidula", "Encrusting bryozoan", "Fish eggs", 
                "Hydroid", "Kamptozoa", "Mobile", "Mussel", "Other", "Other bivalves",
                "Other polychaetes", "Oyster", "Sabellid", "Serpulidae",
                "Solitary ascidian", "Sponge", "Terebellid", "Turf algae", "Vermetid", "n/a")
    
    species_lookup <- read_csv("fouling_sp_lookup.csv")
    
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
    
    output$fouling_cover_barplot <- renderPlot({
      
      barplot_data() %>%
        left_join(species_lookup, by = "scientific_name") %>%
        ggplot(aes(panel_id, point_count, fill = group)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    output$fouling_cover_timeseries_lineplot <- renderPlotly({
      
      df_viz <- load_additional_fouling() %>%
        distinct() %>%
        left_join(species_lookup, by = "scientific_name") %>%
        filter(!is.na(group)) %>%
        group_by(site_name, year, group) %>%
        summarize(percent_cover = mean(percent_cover, na.rm = T))
      
      top_groups <- df_viz %>%
        ungroup() %>%
        group_by(group) %>%
        summarize(percent_cover = max(percent_cover, na.rm = T)) %>%
        arrange(desc(percent_cover)) %>%
        pull(group)
      
      top_groups <- top_groups[1:5]
      
      plot <- marinegeo.utils::viz_mg_timeseries_annual(
        df = df_viz,
        y_var = "percent_cover",
        x_var = "year",
        y_label = "Mean Percent Cover",
        y_grouping_var = "group",
        facet_var = "site_name",
        facet_num_cols = 1
      )
      
      marinegeo.utils::viz_mg_ggplotly(plot, plotly_visible_traces = top_groups)
      
    })
  })
}