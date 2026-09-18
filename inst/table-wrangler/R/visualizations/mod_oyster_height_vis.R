oyster_height_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  layout_column_wrap(
    card(card_header("Mollusk Height Boxplot"),
         full_screen = T,
         plotOutput(ns("mollusk_length_boxplot"))
    )
  )
}


oyster_height_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    

    output$mollusk_length_boxplot <- renderPlot({
      input_list$out_df %>%
        ggplot(aes(site_name, height_mm, fill = live_or_box)) + 
        geom_boxplot() +
        facet_wrap(vars(scientific_name))
    })
    
  })
}
