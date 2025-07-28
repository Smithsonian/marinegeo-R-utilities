visualizations_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("viz_card"))
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
      
      output$viz_card <- renderUI({
        
        plot_list <- get_plot_list()
        
        if(is.null(plot_list)){
          
          card(
            "No visualizations have been defined for this output data type."
          )
          
        } else {
          
          plot_options <- names(plot_list)
          
          ns <- NS(id)
          
          card(
            selectInput(ns("plot_choices"), 
                        "Select a plot",
                        plot_options,
                        multiple = F),
            
            plotOutput(ns("plot_selection")),
            
            full_screen = T
          )
          
        }
      })
      
      get_plot_list <- reactive({
        
        if(input_list$output_table_id == "reef-life-survey-data-marinegeo-v1"){
          plot_list <- viz_rls_plots(input_list$out_df)
        } else {
          plot_list <- NULL
        }
        
        return(plot_list)
      })
      
      output$plot_selection <- renderPlot({
        
        req(input$plot_choices)
        
        plot_list <- get_plot_list()
        
        plot_list[[input$plot_choices]]
        
      })
    }
  )
}

viz_rls_plots <- function(df){
  
  abundance_richness_df <- df %>%
    mutate(method = as.factor(method),
           block = as.factor(block)) %>%
    pivot_longer(inverts:`400`,
                 names_to = "size_class",
                 values_to = "size_count") %>%
    group_by(sample_event_id, method, block) %>%
    summarize(abundance = sum(size_count, na.rm = T),
              richness = n_distinct(taxonomic_id)) 
  
  plot_list <- list(
    
    "Abundance" = abundance_richness_df %>%
      ggplot(aes(x = block, y = abundance, fill = method)) + 
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      facet_wrap(vars(sample_event_id), scales = "free_y", ncol = 2) ,
    
    "Richness" = abundance_richness_df %>%
      ggplot(aes(x = block, y = richness, fill = method)) + 
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      facet_wrap(vars(sample_event_id), scales = "free_y", ncol = 2)
    
  )
  
  return(plot_list)

}