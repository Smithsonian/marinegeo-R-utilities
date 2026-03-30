## Reef Lif survey visualizations 

reef_life_survey_data_marinegeo_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  layout_column_wrap(
    card(card_header("Abundance"), 
         full_screen = T,
         plotOutput(ns("rls_abundance"))),
    card(card_header("Richness"), 
         full_screen = T,
         plotOutput(ns("rls_richness")))
  )
}


reef_life_survey_data_marinegeo_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
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
  }
)}
