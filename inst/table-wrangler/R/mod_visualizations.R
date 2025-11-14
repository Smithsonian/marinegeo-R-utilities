# Steps to add new visualizations
# 
# 1. If adding visualizations to a table that doesn't have any pre-existing visualizations:
#   - Add the table name under the "Table plot list" header
#   - Add a new function to orchestrate table functions
#     - Some tables may share a function (see Oyster 2025 experiment)
#     - Arguments passed to the function can vary across table types

visualizations_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("vizualization_page"))
  )
}

visualizations_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Set the theme for all plots
      theme_set(theme_minimal() +
                  theme(
                    # Plot titles and labels
                    # plot.title = element_text(size = 16, face = "bold"),
                    # plot.subtitle = element_text(size = 14),
                    # plot.caption = element_text(size = 12),
                    
                    # Axis text and labels
                    axis.title = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    
                    # Legend text
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 12),
                    
                    # Strip text (for faceted plots)
                    strip.text = element_text(size = 13, face = "bold"),
                    
                    # Optional: Remove minor grid lines for cleaner look
                    panel.grid.minor = element_blank()
                  ))
      
      
      output$vizualization_page <- renderUI({
        
        if(input_list$output_table_id == "reef-life-survey-data-marinegeo-v1"){
          ## UI: RLS - Reef Life Survey ####
          
          layout_column_wrap(
            card(card_header("Abundance"), 
                 full_screen = T,
                 plotOutput(session$ns("rls_abundance"))),
            card(card_header("Richness"), 
                 full_screen = T,
                 plotOutput(session$ns("rls_richness")))
          )
          
          ## UI: 2025 Oyster Network Project ####
        } else if(str_starts(input_list$output_table_id, "oyster-2025")){
          
          card(card_header("Biobox Coordinates"),
               full_screen = T,
               leafletOutput(session$ns("oyster2025_bioboxcoords"))
          )
          
          ## UI: Seagrass Monitoring ####
         
        } else if(input_list$output_table_id == "seagrass-cover-monitoring-v1"){
          
          layout_column_wrap(
            card(card_header("Percent Cover Barplot"),
                 full_screen = T,
                 plotOutput(session$ns("seagrass_cover_barplot"))
            ),
            card(card_header("Percent Cover Timeseries Boxplot"),
                 full_screen = T,
                 plotOutput(session$ns("seagrass_cover_timeseries_boxplot"))
            )
          )
          
        } else if(input_list$output_table_id == "shoot-count-monitoring-v1"){
          
          layout_column_wrap(
            card(card_header("Density Barplot"),
                 full_screen = T,
                 plotOutput(session$ns("seagrass_density_barplot"))
            ),
            card(card_header("Density Timeseries Boxplot"),
                 full_screen = T,
                 plotOutput(session$ns("seagrass_density_timeseries_boxplot"))
            )
          )
          
        } else if(input_list$output_table_id == "seagrass-leaf-monitoring-v1"){
          
          layout_column_wrap(
            card(card_header("Seagrass Leaf Length"),
                 full_screen = T,
                 plotOutput(session$ns("seagrass_leaf_length_boxplot"))
            ),
            card(card_header("Seagrass Leaf Width"),
                 full_screen = T,
                 plotOutput(session$ns("seagrass_leaf_width_boxplot"))
            )
          )
          
        } else if(input_list$output_table_id == "sheath-and-epibiont-monitoring-v1"){
          
          layout_column_wrap(
            card(card_header("Shoot Length Barplot"),
                 full_screen = T,
                 plotOutput(session$ns("shoot_length_barplot"))
            ),
            card(card_header("Shoot Length Timeseries Boxplot"),
                 full_screen = T,
                 plotOutput(session$ns("shoot_length_timeseries_boxplot"))
            ),
            
            card(card_header("Leaf and Epibiont Mass Histogram"),
                 full_screen = T,
                 plotOutput(session$ns("leaf_and_epibiont_histograms"))
            ),
            card(card_header("Leaf Mass Timeseries Boxplot"),
                 full_screen = T,
                 plotOutput(session$ns("leaf_mass_timeseries_boxplot"))
            ),
            card(card_header("Epibiont Mass Timeseries Boxplot"),
                 full_screen = T,
                 plotOutput(session$ns("epibiont_mass_timeseries_boxplot"))
            )
            
          )
          
        } else if(input_list$output_table_id == "seagrass-metadata-monitoring-v1"){
          card(card_header("Transect Map "),
               full_screen = T,
               leafletOutput(session$ns("transect_map"))
          )
          
        }
        
      })
      
      ## Plots: Multi Protocol ####
      output$transect_map <- renderLeaflet({
        
        input_list$out_df %>%
          pivot_longer(c(transect_begin_decimal_latitude, transect_end_decimal_latitude,
                         transect_begin_decimal_longitude, transect_end_decimal_longitude),
                       names_to = "coordinate_type", values_to = "coordinate") %>%
          mutate(lat_long = case_when(
            coordinate_type %in% c("transect_begin_decimal_latitude", "transect_end_decimal_latitude") ~ "latitude",
            coordinate_type %in% c("transect_begin_decimal_longitude", "transect_end_decimal_longitude") ~ "longitude",
            T ~ NA_character_
          )) %>%
          mutate(coordinate_type = case_when(
            grepl("transect_begin", coordinate_type) ~ "transect begin",
            grepl("transect_end", coordinate_type) ~ "transect end",
            T ~ NA_character_
          )) %>%
          pivot_wider(names_from = lat_long,
                      values_from = coordinate) %>%
          mutate(id = paste0(site_name, " ", transect)) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs = 25832) %>%
          group_by(id) %>%
          dplyr::summarize(do_union=FALSE) %>%  
          st_cast("LINESTRING") %>%
          leaflet() %>%
          addTiles() %>%
          addPolylines()
      })

      ## Plots: Reef Life Survey ####
      output$rls_abundance <- renderPlot({
        
        abundance_richness_df <- input_list$out_df %>%
          mutate(method = as.factor(method),
                 block = as.factor(block)) %>%
          pivot_longer(inverts:`400`,
                       names_to = "size_class",
                       values_to = "size_count") %>%
          group_by(sample_event_id, method, block) %>%
          summarize(abundance = sum(size_count, na.rm = T),
                    richness = n_distinct(taxonomic_id)) 
        
        abundance_richness_df %>%
          ggplot(aes(x = block, y = abundance, fill = method)) + 
          geom_bar(stat = "identity") +
          scale_fill_viridis_d() +
          facet_wrap(vars(sample_event_id), scales = "free_y", ncol = 2)
      })
      
      output$rls_richness <- renderPlot({
        
        abundance_richness_df <- input_list$out_df %>%
          mutate(method = as.factor(method),
                 block = as.factor(block)) %>%
          pivot_longer(inverts:`400`,
                       names_to = "size_class",
                       values_to = "size_count") %>%
          group_by(sample_event_id, method, block) %>%
          summarize(abundance = sum(size_count, na.rm = T),
                    richness = n_distinct(taxonomic_id)) 
        
        abundance_richness_df %>%
          ggplot(aes(x = block, y = richness, fill = method)) + 
          geom_bar(stat = "identity") +
          scale_fill_viridis_d() +
          facet_wrap(vars(sample_event_id), scales = "free_y", ncol = 2)
        
      })
      
      ## Plots: Seagrass Monitoring ####
      
      load_additional_seagrass <- reactive({
        
        df <- bind_rows(
          marinegeo.utils::db_marinegeo_L2(input_list$output_table_id) %>%
            filter(input_filename != input_list$data_filename,
                   partner_code %in% unique(input_list$out_df$partner_code)) %>%
            collect(),
          input_list$out_df
        ) %>%
          arrange(year(sample_collection_date)) %>%
          mutate(year = year(sample_collection_date)) %>%
          
        
        return(df)
        
      })
      
      output$seagrass_cover_barplot <- renderPlot({
        input_list$out_df %>%
          ggplot(aes(quadrat, percent_cover, fill = scientific_name)) + 
          geom_col() +
          facet_wrap(site_name ~ transect)
      })

      output$seagrass_cover_timeseries_boxplot <- renderPlot({
        load_additional_seagrass() %>%
          filter(taxonomic_id != 0) %>%
          ggplot(aes(as.factor(year), percent_cover)) + 
          geom_boxplot() +
          facet_grid(site_name ~ scientific_name)
      })
      
      output$seagrass_density_barplot <- renderPlot({
        input_list$out_df %>%
          ggplot(aes(quadrat, shoot_density_m2, fill = scientific_name)) + 
          geom_col() +
          facet_wrap(site_name ~ transect)
      })
      
      output$seagrass_density_timeseries_boxplot <- renderPlot({
        load_additional_seagrass() %>%
          filter(taxonomic_id != 0) %>%
          ggplot(aes(as.factor(year), shoot_density_m2)) + 
          geom_boxplot() +
          facet_grid(site_name ~ scientific_name)
      })
      
      output$seagrass_leaf_length_boxplot <- renderPlot({
        input_list$out_df %>%
          ggplot(aes(site_name, leaf_length_mm)) + 
          geom_boxplot() +
          facet_wrap(vars(scientific_name))
      })
      
      output$seagrass_leaf_width_boxplot <- renderPlot({
        input_list$out_df %>%
          ggplot(aes(site_name, leaf_width_mm)) + 
          geom_boxplot() +
          facet_wrap(vars(scientific_name))
      })
      
      output$shoot_length_barplot <- renderPlot({
        input_list$out_df %>%
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
          filter(taxonomic_id != 0) %>%
          ggplot(aes(as.factor(year), shoot_length_mm)) + 
          geom_boxplot() +
          facet_grid(site_name ~ scientific_name)
      })
      
      output$leaf_mass_timeseries_boxplot <- renderPlot({
        load_additional_seagrass() %>%
          filter(taxonomic_id != 0) %>%
          ggplot(aes(as.factor(year), blades_dry_mass_g)) + 
          geom_boxplot() +
          facet_grid(site_name ~ scientific_name)
      })
      
      output$epibiont_mass_timeseries_boxplot <- renderPlot({
        load_additional_seagrass() %>%
          filter(taxonomic_id != 0) %>%
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
      
      ## Plots: Oyster 2025 Network Project ####
      
      output$oyster2025_bioboxcoords <- renderLeaflet({
        input_list$out_df %>%
          leaflet() %>%
          addTiles() %>%
          addMarkers(lng = ~biobox_longitude, 
                     lat = ~biobox_latitude,
                     label = ~reef_code)
      })
      
    }
  )}