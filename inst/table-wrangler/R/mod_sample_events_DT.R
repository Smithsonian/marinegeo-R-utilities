sample_event_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("sample_event_summary"))
    
  )
}

sample_event_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$sample_event_summary <- renderUI({
        
        ns <- NS(id)
        
        if(input_list$output_table_id == "reef-life-survey-data-marinegeo-v1"){
          
          div(
            card("Check that sample events (dives) in this file are defined in the roster. If one or more divers are listed in the Initials or Diver column (depending on project), then the dive in the data matches the roster",
                 DTOutput(ns("file_sample_events")),
                 full_screen = TRUE),
            
            card("Check that all Method - Block combinations are represented. This table pulls in L2 data derived from other input files, if it exists. You can also review all metadata values for visibility, direction, time, and photoquadrat status across all files.",
                 DTOutput(ns("all_sample_events")),
                 full_screen = TRUE)
          )
        } else if(str_starts(input_list$output_table_id, "oyster-2025")){
          div(
            card("Check that sample events in this file are defined in the roster. If the partner code, site name and reef code match the roster, then values for the status columns will be present",
                 DTOutput(ns("oyster_2025_roster")),
                 full_screen = TRUE),
          )
        } else if(input_list$output_table_id %in%  c("seagrass-biomass-monitoring-v1",
                                                     "seagrass-cover-monitoring-v1",
                                                     "seagrass-macroinvertebrates-monitoring-v1",
                                                     "shoot-count-monitoring-v1",
                                                     # "seagrass-metadata-monitoring-v1",
                                                     "seagrass-macrophyte-monitoring-v1",
                                                     "seagrass-epifauna-monitoring-v1",
                                                     "seagrass-macroalgae-monitoring-v1",
                                                     "seagrass-leaf-monitoring-v1",
                                                     "sheath-and-epibiont-monitoring-v1")){
          
          layout_column_wrap(
            width = "500px",
            card("Check that sample events in this file are defined in the roster. If the partner code, site name and sample collection date match the roster, then other column values will be present",
                 DTOutput(ns("seagrass_monitoring_roster")),
                 full_screen = TRUE),
            
            card("Check that number of unique quadrats per transect",
                 DTOutput(ns("num_uniq_quadrats")),
                 full_screen = TRUE), 
            
            card("Check quadrat - transect relationships between tables",
                 DTOutput(ns("quadrat_relationships")),
                 full_screen = TRUE)
            
          )
        }
      })
      
      ## Cross-Habitats ####
      output$num_uniq_quadrats <- renderDT({
        
        if(!("quadrat" %in% colnames(input_list$out_df))){
          df <- tibble(status = "Quadrat column not in table")
          
        } else {
          
          df <- input_list$out_df |>
            dplyr::group_by(site_name, transect) |>
            dplyr::summarize(number_quadrats_per_transect = dplyr::n_distinct(quadrat)) 
          
        }
        
        df %>%
          DT::datatable(
            style = "default"
          )
      })
      
      output$quadrat_relationships <- renderDT({
        
        if(!("quadrat" %in% colnames(input_list$out_df))){
          df <- tibble(status = "Quadrat column not in table")
          
        } else if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df <- tibble(status = "add sample event ID column!")
          
        } else {
          
          quadrat_tables <- c(
            "seagrass-cover-monitoring-v1",
            "shoot-count-monitoring-v1",
            "seagrass-macroinvertebrates-monitoring-v1",
            "seagrass-macrophyte-monitoring-v1",
            "seagrass-epifauna-monitoring-v1",
            "seagrass-leaf-monitoring-v1",
            "sheath-and-epibiont-monitoring-v1",
            "seagrass-biomass-monitoring-v1",
            "seagrass-macroalgae-monitoring-v1"
          )
          
          sample_events <- unique(input_list$out_df$sample_event_id)
          
          df <- compact(
            lapply(quadrat_tables, function(x){
              
              tryCatch({
                # Don't load L2 table for the current target table, 
                # use the loaded table instead
                if(x == input_list$output_table_id){
                  input_list$out_df %>%
                    select(site_name, transect, quadrat) %>%
                    distinct() %>%
                    mutate(table = x,
                           status = T)
                  
                } else {
                  marinegeo.utils::db_marinegeo_L2(x) %>%
                    filter(sample_event_id %in% sample_events) %>%
                    select(site_name, transect, quadrat) %>%
                    distinct() %>%
                    collect() %>%
                    mutate(table = x,
                           status = T)
                  
                }
              }, error = function(e){
                NULL
                
              })
            })
          ) %>%
            bind_rows() %>%
            pivot_wider(names_from = table, 
                        values_from = status) %>%
            arrange(site_name, transect, quadrat)
          
        }
        
        df %>%
          DT::datatable(
            style = "default",
            options = list(pageLength = 50)
          )
        
      })
      
      ## Reef Life Survey ####
      
      load_rls_roster <- function(){
        
        # South Florida roster
        rls_roster_filepath <- paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L1-data/dive-roster/EPA-2024/EPA_project_dive_roster.xlsx")
        
        epa_roster <- readxl::read_excel(rls_roster_filepath) %>%
          filter(!is.na(`T1 Year`) | !is.na(`T2 Year`)) %>%
          select(`Site Code`, `Site Name`,
                 `T1 Initials`, `T1 Depth`, `T1 Year`, `T1 Month`, `T1 Day`,
                 `T2 Initials`, `T2 Depth`, `T2 Year`, `T2 Month`, `T2 Day`) %>%
          mutate(across(everything(), as.character)) %>% 
          pivot_longer(-all_of(c("Site Code", "Site Name")), names_to = "column_name", values_to = "value") %>%
          mutate(transect = substr(column_name, 1,2),
                 column_name = gsub("T1 ", "", 
                                    gsub("T2 ", "", column_name))) %>%
          pivot_wider(names_from = column_name, values_from = value) %>%
          mutate(Date = ymd(paste(Year, Month, Day, sep ="-")),
                 Depth = as.numeric(Depth)) %>%
          rename(site_code = `Site Code`,
                 site_name = `Site Name`,
                 depth = Depth,
                 date = Date,
                 initials = Initials) %>%
          dplyr::mutate(sample_event_id = paste(gsub(" ", "_", site_name), 
                                                "RLS", date, depth, sep = "_")) %>%
          select(sample_event_id, site_name, initials)
        
        # PAFF 2025 rosters
        roster_dr <- paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L1-data/dive-roster/PAFF-2025/DR 2025 Transect Metadata.xlsx")
        roster_usvi <- paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L1-data/dive-roster/PAFF-2025/USVI Metadata.xlsx")
        roster_bra <- paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L1-data/dive-roster/PAFF-2025/Metadata Table_Brazil.xlsx")
        
        paff_roster <- readxl::read_excel(roster_bra) %>%
          mutate(Date = ymd(paste(Year, Month, Day, sep = "-"))) %>%
          bind_rows(
            readxl::read_excel(roster_dr),
            readxl::read_excel(roster_usvi)
          ) %>%
          rename(site_code = Code,
                 site_name = `SiteName`,
                 depth = Depth,
                 date = Date,
                 diver = Diver) %>%
          dplyr::mutate(sample_event_id = paste(gsub(" ", "_", site_name), 
                                                "RLS", date, depth, sep = "_")) %>%
          select(sample_event_id, site_name, diver) %>%
          distinct() %>% 
          group_by(sample_event_id, site_name) %>% 
          summarize(diver = paste(diver, collapse=","))
        
        rls_roster <- list(
          "EPA-South-Florida" = epa_roster,
          "PAFF-2025" = paff_roster
        )
        
        return(rls_roster)
      }
      
      load_rls_l2_data <- function(){
        
        req_cols <- c("sample_event_id", "site_name", "site_code", "date", "depth", "method", "block", "vis", "direction", "time", "photoquadrats","input_filename")
        
        # rls_l2_files <- list.files(paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L2-data/reef-life-survey-data-marinegeo-v1"),
        #                            recursive = T, full.names = T)
        
        rls_l2_data <- marinegeo.utils::db_marinegeo_L2("reef-life-survey-data-marinegeo-v1") %>%
          select(all_of(req_cols)) %>%
          distinct()
        
        return(rls_l2_data)
      }
      
      output$file_sample_events <- renderDT({
        
        req(input_list$selected_flag)
        
        req_cols <- c("sample_event_id", "site_name", "date", "depth", "method", "block", "input_filename")
        
        if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df <- tibble(status = "add sample event ID column!")
          
          # } else if(!is.Date(input_list$out_df$date) | !is.numeric(input_list$out_df$method) | !is.numeric(input_list$out_df$block)){
          #   df <- tibble(status = "check data type of date, method, or block to evaluate sample events")
          
        } else {
          
          rls_data <- input_list$out_df %>%
            select(any_of(req_cols)) %>%
            distinct()
          
          rls_sample_events <- marinegeo.utils::utl_rls_sample_event_summary(rls_data) %>%
            select(sample_event_id, site_name, date, depth)
          
          if(str_starts(input_list$project_directory, "EPA-2024/")){
            proj_roster <- "EPA-South-Florida"
          } else if(str_starts(input_list$project_directory, "PAFF-2025/")){
            proj_roster <- "PAFF-2025"
          }
          
          df <- left_join(rls_sample_events, 
                          load_rls_roster()[[proj_roster]], 
                          by = c("sample_event_id", "site_name"))
          
        }
        
        df %>%
          select(-any_of("sample_event_id")) %>%
          DT::datatable(
            style = "default"
          )
        
      })
      
      output$all_sample_events <- renderDT({
        
        req(input_list$selected_flag)
        
        req_cols <- c("sample_event_id", "site_code", "site_name", "date", "depth", "method", "block", "vis", "direction", "time", "photoquadrats","input_filename")
        
        if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df <- tibble(status = "add sample event ID column!")
          
          # } else if(!is.Date(input_list$out_df$date) | !is.numeric(input_list$out_df$method) | !is.numeric(input_list$out_df$block)){
          #   df <- tibble(status = "check data type of date, method, or block to evaluate sample events")
          
        } else {
          
          site_names <- unique(input_list$out_df$site_name)
          
          df_in <- input_list$out_df
          
          if(is.numeric(df_in$vis)){
            df_in <- df_in %>%
              mutate(vis = as.character(vis))
          }
          
          if(!is.character(df_in$time)){
            df_in <- df_in %>%
              mutate(time = as.character(time))
          }
          
          rls_data <- bind_rows(
            df_in %>%
              select(any_of(req_cols)) %>%
              distinct(), 
            
            load_rls_l2_data() %>%
              filter(input_filename != input_list$data_filename) %>%
              filter(site_name %in% site_names) %>%
              collect()
          )
          
          rls_sample_events <- marinegeo.utils::utl_rls_sample_event_summary(rls_data)
          
          rls_dive_metadata <- rls_data %>%
            group_by(sample_event_id, site_name) %>% 
            summarize(vis = paste(unique(vis), collapse = ","),
                      direction = paste(unique(direction), collapse = ","),
                      time = paste(unique(time), collapse = ","),
                      photoquadrats = paste(unique(photoquadrats), collapse = ",")
            )
          
          rls_summary <- left_join(rls_sample_events, rls_dive_metadata) %>%
            select(site_name, everything())
          
        }
        
        rls_summary %>%
          select(-any_of("sample_event_id")) %>%
          DT::datatable(
            style = "default"
          )
      })
      
      ## Seagrass Monitoring ####
      output$seagrass_monitoring_roster <- renderDT({
        
        req(input_list$selected_flag)
        
        if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df_out <- tibble(status = "add sample event ID column!")
          
        } else {
          
          roster_files <- list.files(
            paste0(Sys.getenv("repository_filepath"), "marinegeo-seagrass-monitoring/L1-data/seagrass-roster"), 
            full.names = T
          )
          
          roster <- readr::read_csv(roster_files)
          
          df <- input_list$out_df %>%
            select(partner_code, site_name, sample_collection_date) %>%
            distinct()
          
          roster_columns <- roster %>%
            mutate(sample_collection_date = ymd(paste(Year, Month, Day, sep = "-"))) %>%
            select(-`GitHub Tracker`, -Year, -Month, -Day, -method_id) %>%
            distinct()
          
          df_out <- left_join(
            df, roster_columns, by = c("partner_code", "site_name", "sample_collection_date")
          )
          
        }
        
        df_out %>%
          DT::datatable(
            style = "default"
          )
        
      })
      
      ## Oyster Network Project 2025 ####
      output$oyster_2025_roster <- renderDT({
        
        req(input_list$selected_flag)
        
        if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df_out <- tibble(status = "add sample event ID column!")
          
        } else {
          
          roster_filepath <- paste0(Sys.getenv("repository_filepath"), "oyster-network-project-2025/L1-data/oyster-2025-roster/oyster_network_project_2025_roster.csv")
          
          roster <- readr::read_csv(roster_filepath)
          
          df <- input_list$out_df %>%
            select(partner_code, site_name, reef_code) %>%
            distinct()

          roster_columns <- roster %>%
            select(partner, partner_code, site_name, reef_code, deployment_status, retrieval_status, logger_status) %>%
            distinct()
          
          df_out <- left_join(
            df, roster_columns, by = c("partner_code", "site_name", "reef_code")
          )
          
        }
        
        df_out %>%
          DT::datatable(
            style = "default"
          )
        
      })
      
    }
  )
}