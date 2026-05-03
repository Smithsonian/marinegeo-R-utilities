# Fouling Panel metadata
fouling_panel_metadata_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  card(card_header("Panel Map "),
       full_screen = T,
       leafletOutput(ns("panel_map"))
  )
}


fouling_panel_metadata_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
    output$panel_map <- renderLeaflet({
      
      input_list$out_df %>%
        group_by(sample_event_id, site_name, latitude, longitude) %>%
        summarize(n_panel_retrievals = n()) %>%
        distinct() %>%
        leaflet() %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude,
                   popup = ~paste0(
                     site_name, " : ", n_panel_retrievals, " retrievals"
                   ))
    })
  })
}
