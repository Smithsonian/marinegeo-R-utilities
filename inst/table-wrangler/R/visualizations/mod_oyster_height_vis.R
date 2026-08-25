oyster_height_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  layout_column_wrap(
    card(card_header("Live Mollusk Height"),
         full_screen = T,
         plotOutput(ns("live_mollusk_length_boxplot"))
    ),
    card(card_header("Box Mollusk Height"),
         full_screen = T,
         plotOutput(ns("box_mollusk_length_boxplot"))
    )
  )
}


oyster_height_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    

    output$live_mollusk_length_boxplot <- renderPlot({
      input_list$out_df %>%
        filter(live_or_box == "live")%>%
        ggplot(aes(site_name, height_mm)) + 
        geom_boxplot() +
        facet_wrap(vars(scientific_name))
    })
    
    output$box_mollusk_length_boxplot <- renderPlot({
      input_list$out_df %>%
        filter(live_or_box == "box")%>%
        ggplot(aes(site_name, height_mm)) + 
        geom_boxplot() +
        facet_wrap(vars(scientific_name))
    })
  })
}
