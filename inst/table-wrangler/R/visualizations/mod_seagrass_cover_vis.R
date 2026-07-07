### template UI and server functions for a visualization of a specific data type


seagrass_cover_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("sample_event_select_ui")),
    layout_column_wrap(
      card(
        card_header("Percent Cover Barplot"),
        full_screen = TRUE,
        plotOutput(ns("seagrass_cover_barplot"))
      ),
      card(
        card_header("Percent Cover Timeseries Line Plot"),
        full_screen = TRUE,
        plotlyOutput(ns("seagrass_cover_timeseries_lineplot"))
      )
    )
  )
}


seagrass_cover_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {

    load_additional_seagrass <- reactive({
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

    output$seagrass_cover_barplot <- renderPlot({
      barplot_data() %>%
        ggplot(aes(quadrat, percent_cover, fill = scientific_name)) +
        geom_col() +
        facet_wrap(site_name ~ transect)
    })

    output$seagrass_cover_timeseries_lineplot <- renderPlotly({
      functional_groups <- c("Algae", "Seagrass")
      
      df <- load_additional_seagrass() %>%
        mutate(functional_group = utl_mg_assign_functional_groups("vegetation",
                                                                  functional_groups,
                                                                  scientific_name))
      
      df_fg <- df %>%
        filter(!is.na(functional_group)) %>%
        group_by(year, site_name, transect, quadrat, functional_group) %>%
        summarize(percent_cover = sum(percent_cover)) %>%
        mutate(percent_cover = case_when(
          percent_cover > 100 ~ 100,
          T ~ percent_cover
        )) %>%
        ungroup() %>%
        rename(cover_type = functional_group)
      
      df_sp <- df %>%
        select(year, site_name, transect, quadrat, scientific_name, percent_cover) %>%
        rename(cover_type = scientific_name) %>%
        mutate(percent_cover = case_when(
          percent_cover > 100 ~ 100,
          T ~ percent_cover
        ))
      
      df_viz <- bind_rows(df_fg, df_sp) %>%
        group_by(year, site_name, cover_type) %>%
        summarize(percent_cover = mean(percent_cover, na.rm = T))
      
      plot <- marinegeo.utils::viz_mg_timeseries_annual(
          df = df_viz,
          y_var = "percent_cover",
          x_var = "year",
          y_label = "Mean Percent Cover",
          y_limits = c(0,100),
          y_grouping_var = "cover_type",
          facet_var = "site_name",
          facet_num_cols = 1
      )
    
      marinegeo.utils::viz_mg_ggplotly(plot, plotly_visible_traces = functional_groups)
      
    })
  })
}
