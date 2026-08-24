#seagrass transect maps 
oyster_transect_metadata_monitoring_v1_vis_UI <- function(id) {
  ns <- NS(id)
  
  card(card_header("Transect Map "),
       full_screen = T,
       leafletOutput(ns("transect_map"))
  )
}


oyster_transect_metadata_monitoring_v1_vis_server <- function(id, input_list) {
  moduleServer(id, function(input, output, session) {
    
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
  })
}
