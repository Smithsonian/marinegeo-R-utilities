#seagrass shoot cont monitoring visualizations


oyster_count_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
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
    
    output$oyster_density_barplot <- renderPlot({
      
      req(input_list$out_df)
      
      input_list$out_df %>%
        mutate(
          functional_group = utl_mg_assign_ancestor_labels(
            fg_tree = "oyster_density",
            scientific_names = scientific_name,
            type = "primary"
          )
        ) %>%
        ggplot(aes(transect, density_m2, fill = functional_group)) +
        geom_col() +
        facet_wrap(vars(site_name))
      })
    
  })
}
