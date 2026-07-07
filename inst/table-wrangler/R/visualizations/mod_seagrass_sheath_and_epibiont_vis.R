#seagrass sheath and epibiont monitoring visualizations

sheath_and_epibiont_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("sample_event_select_ui")),
    layout_column_wrap(
      card(card_header("Shoot Length Barplot"),
           full_screen = T,
           plotOutput(ns("shoot_length_barplot"))
      ),
      card(card_header("Shoot Length Timeseries Boxplot"),
           full_screen = T,
           plotOutput(ns("shoot_length_timeseries_boxplot"))
      ),
      card(card_header("Shoot Length Timeseries Lineplot"),
           full_screen = T,
           plotlyOutput(ns("shoot_length_timeseries_lineplot"))
      ),
      
      card(card_header("Leaf and Epibiont Mass Histogram"),
           full_screen = T,
           plotOutput(ns("leaf_and_epibiont_histograms"))
      ),
      card(card_header("Leaf Mass Timeseries Boxplot"),
           full_screen = T,
           plotOutput(ns("leaf_mass_timeseries_boxplot"))
      ),
      card(card_header("Epibiont Mass Timeseries Boxplot"),
           full_screen = T,
           plotOutput(ns("epibiont_mass_timeseries_boxplot"))
      )

    )
  )
}


sheath_and_epibiont_monitoring_v1_vis_server <- function(id, input_list) {
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

    output$shoot_length_barplot <- renderPlot({
      barplot_data() %>%
        mutate(site_name = paste0(site_name, " - ", scientific_name)) %>%
        mutate(quadrat = as.factor(quadrat)) %>%
        select(site_name, transect, quadrat, scientific_name, sheath_length_mm, median_leaf_length_mm) %>%
        pivot_longer(all_of(c("sheath_length_mm", "median_leaf_length_mm")), names_to = "measurement_type", values_to = "measurement") %>%
        ggplot(aes(quadrat, measurement, fill = measurement_type)) +
        geom_col() +
        facet_wrap(site_name ~ transect)
    })

    output$shoot_length_timeseries_boxplot <- renderPlot({
      load_additional_seagrass() %>%
        #filter(taxonomic_id != 0) %>%
        ggplot(aes(as.factor(year), shoot_length_mm)) +
        geom_boxplot() +
        facet_grid(site_name ~ scientific_name)
    })

    output$leaf_mass_timeseries_boxplot <- renderPlot({
      load_additional_seagrass() %>%
        #filter(taxonomic_id != 0) %>%
        ggplot(aes(as.factor(year), blades_dry_mass_g)) +
        geom_boxplot() +
        facet_grid(site_name ~ scientific_name)
    })

    output$epibiont_mass_timeseries_boxplot <- renderPlot({
      load_additional_seagrass() %>%
        #filter(taxonomic_id != 0) %>%
        ggplot(aes(as.factor(year), epibiont_dry_mass_g)) +
        geom_boxplot() +
        facet_grid(site_name ~ scientific_name)
    })

    output$leaf_and_epibiont_histograms <- renderPlot({
      input_list$out_df %>%
        select(site_name, transect, quadrat, scientific_name, blades_dry_mass_g, epibiont_dry_mass_g) %>%
        pivot_longer(all_of(c("blades_dry_mass_g", "epibiont_dry_mass_g")), names_to = "measurement_type", values_to = "measurement") %>%
        ggplot(aes(measurement)) +
        geom_histogram() +
        facet_wrap(scientific_name ~ measurement_type)
    })
    
    output$shoot_length_timeseries_lineplot <- renderPlotly({
      functional_groups <- "Seagrass"
      
      df <- load_additional_seagrass() %>%
        mutate(functional_group = utl_mg_assign_functional_groups("vegetation",
                                                                  functional_groups,
                                                                  scientific_name)) %>%
        filter(!is.na(shoot_length_mm))
      
      df_fg <- df %>%
        filter(!is.na(functional_group)) %>%
        group_by(year, site_name, transect, quadrat, functional_group) %>%
        summarize(shoot_length_mm = mean(shoot_length_mm, na.rm = T)) %>%
        ungroup() %>%
        rename(type = functional_group)
      
      df_sp <- df %>%
        select(year, site_name, transect, quadrat, scientific_name, shoot_length_mm) %>%
        rename(type = scientific_name)
      
      df_viz <- bind_rows(df_fg, df_sp) %>%
        group_by(year, site_name, type) %>%
        summarize(shoot_length_mm = mean(shoot_length_mm, na.rm = T))
      
      plot <- marinegeo.utils::viz_mg_timeseries_annual(
        df = df_viz,
        y_var = "shoot_length_mm",
        x_var = "year",
        y_label = "Mean Shoot Length mm",
        y_grouping_var = "type",
        facet_var = "site_name",
        facet_num_cols = 1
      )
      
      marinegeo.utils::viz_mg_ggplotly(plot, plotly_visible_traces = functional_groups)
      
    })
    
  })
}
