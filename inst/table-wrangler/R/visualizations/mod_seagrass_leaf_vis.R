#seagrass leaf length visualizations 

seagrass_leaf_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  layout_column_wrap(
    card(card_header("Seagrass Leaf Length"),
         full_screen = T,
         plotOutput(ns("seagrass_leaf_length_boxplot"))
    ),
    card(card_header("Seagrass Leaf Width"),
         full_screen = T,
         plotOutput(ns("seagrass_leaf_width_boxplot"))
    )
  )
}


seagrass_leaf_monitoring_v1_vis_server <- function(id, input_list) {
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
  })
}
