#seagrass shoot cont monitoring visualizations


shoot_count_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("sample_event_select_ui")),
    layout_column_wrap(
      card(
        card_header("Density Barplot"),
        full_screen = TRUE,
        plotOutput(ns("seagrass_density_barplot"))
      ),
      card(
        card_header("Density Timeseries Site plot"),
        full_screen = TRUE,
        plotlyOutput(ns("seagrass_density_site_timeseries_lineplot"))
      ),
      card(
        card_header("Density Timeseries Patch plot"),
        full_screen = TRUE,
        plotlyOutput(ns("seagrass_density_patch_timeseries_lineplot"))
      )
    )
  )
}



shoot_count_monitoring_v1_vis_server <- function(id, input_list) {
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

    output$seagrass_density_barplot <- renderPlot({
      barplot_data() %>%
        ggplot(aes(quadrat, shoot_density_m2, fill = scientific_name)) +
        geom_col() +
        facet_wrap(site_name ~ transect)
    })

    output$seagrass_density_site_timeseries_lineplot <- renderPlotly({
      functional_groups <- "Seagrass"
      
      df <- load_additional_seagrass() %>%
        mutate(functional_group = utl_mg_assign_functional_groups("vegetation",
                                                                  functional_groups,
                                                                  scientific_name)) %>%
        filter(!is.na(shoot_density_m2))
      
      df_fg <- df %>%
        filter(!is.na(functional_group)) %>%
        group_by(year, site_name, transect, quadrat, functional_group) %>%
        summarize(shoot_density_m2 = sum(shoot_density_m2, na.rm = T)) %>%
        ungroup() %>%
        rename(density_type = functional_group)
      
      df_sp <- df %>%
        select(year, site_name, transect, quadrat, scientific_name, shoot_density_m2) %>%
        rename(density_type = scientific_name)
      
      df_viz <- bind_rows(df_fg, df_sp) %>%
        group_by(year, site_name, density_type) %>%
        summarize(shoot_density_m2 = mean(shoot_density_m2, na.rm = T))
      
      plot <- marinegeo.utils::viz_mg_timeseries_annual(
        df = df_viz,
        y_var = "shoot_density_m2",
        x_var = "year",
        y_label = "Mean Shoot Density m2",
        y_grouping_var = "density_type",
        facet_var = "site_name",
        facet_num_cols = 1
      )
      
      marinegeo.utils::viz_mg_ggplotly(plot, plotly_visible_traces = functional_groups)
      
    })
    
    output$seagrass_density_patch_timeseries_lineplot <- renderPlotly({
      functional_groups <- "Seagrass"
      
      df <- load_additional_seagrass() %>%
        mutate(functional_group = utl_mg_assign_functional_groups("vegetation",
                                                                  functional_groups,
                                                                  scientific_name)) %>%
        filter(!is.na(shoot_density_m2)) %>%
        filter(shoot_density_m2 > 0)
      
      df_fg <- df %>%
        filter(!is.na(functional_group)) %>%
        group_by(year, site_name, transect, quadrat, functional_group) %>%
        summarize(shoot_density_m2 = sum(shoot_density_m2, na.rm = T)) %>%
        ungroup() %>%
        rename(density_type = functional_group)
      
      df_sp <- df %>%
        select(year, site_name, transect, quadrat, scientific_name, shoot_density_m2) %>%
        rename(density_type = scientific_name)
      
      df_viz <- bind_rows(df_fg, df_sp) %>%
        group_by(year, site_name, density_type) %>%
        summarize(shoot_density_m2 = mean(shoot_density_m2, na.rm = T))
      
      plot <- marinegeo.utils::viz_mg_timeseries_annual(
        df = df_viz,
        y_var = "shoot_density_m2",
        x_var = "year",
        y_label = "Mean Shoot Density m2",
        y_grouping_var = "density_type",
        facet_var = "site_name",
        facet_num_cols = 1
      )
      
      marinegeo.utils::viz_mg_ggplotly(plot, plotly_visible_traces = functional_groups)
      
    })
    
  })
}
