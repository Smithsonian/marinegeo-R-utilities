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
            card("Check that sample events (dives) in this file are defined in the roster.",
                 DTOutput(ns("file_sample_events")),
                 full_screen = TRUE),
            
            card("Check Method - Block combination for all processed data",
                 DTOutput(ns("all_sample_events")),
                 full_screen = TRUE)
          )
        }
      })
      
      ## Reef Life Survey ####
      
      load_rls_roster <- function(){
        
        # South Florida roster
        rls_roster_filepath <- paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L1-data/dive-roster/EPA_project_dive_roster.xlsx")
        
        epa_roster <- readxl::read_excel(rls_roster_filepath) %>%
          filter(!is.na(`T1 Year`) | !is.na(`T2 Year`)) %>%
          select(`Site Code`, 
                 `T1 Initials`, `T1 Depth`, `T1 Year`, `T1 Month`, `T1 Day`,
                 `T2 Initials`, `T2 Depth`, `T2 Year`, `T2 Month`, `T2 Day`) %>%
          mutate(across(everything(), as.character)) %>% 
          pivot_longer(-`Site Code`, names_to = "column_name", values_to = "value") %>%
          mutate(transect = substr(column_name, 1,2),
                 column_name = gsub("T1 ", "", 
                                    gsub("T2 ", "", column_name))) %>%
          pivot_wider(names_from = column_name, values_from = value) %>%
          mutate(Date = ymd(paste(Year, Month, Day, sep ="-")),
                 Depth = as.numeric(Depth)) %>%
          rename(site_code = `Site Code`,
                 depth = Depth,
                 date = Date,
                 initials = Initials) %>%
          dplyr::mutate(sample_event_id = paste(site_code, "RLS", date, depth, sep = "_")) %>%
          select(sample_event_id, site_code, initials)
        
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
          dplyr::mutate(sample_event_id = paste(site_code, "RLS", date, depth, sep = "_")) %>%
          select(sample_event_id, site_code, site_name, diver) %>%
          distinct()
        
        rls_roster <- list(
          "EPA-South-Florida" = epa_roster,
          "PAFF-2025" = paff_roster
        )
        
        return(rls_roster)
      }
      
      load_rls_l2_data <- function(){
        
        req_cols <- c("sample_event_id", "site_code", "date", "depth", "method", "block", "input_filename")
        
        rls_l2_files <- list.files(paste0(Sys.getenv("repository_filepath"), "marinegeo-reef-life-survey/L2-data/reef-life-survey-data-marinegeo-v1"),
                                   recursive = T, full.names = T)
        
        rls_l2_data <- read_csv(rls_l2_files) %>%
          select(all_of(req_cols)) %>%
          distinct()
        
        return(rls_l2_data)
      }
      
      output$file_sample_events <- renderDT({
        
        req(input_list$selected_flag)
        
        req_cols <- c("sample_event_id", "site_code", "date", "depth", "method", "block", "input_filename")
        
        if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df <- tibble(status = "add sample event ID column!")
          
          # } else if(!is.Date(input_list$out_df$date) | !is.numeric(input_list$out_df$method) | !is.numeric(input_list$out_df$block)){
          #   df <- tibble(status = "check data type of date, method, or block to evaluate sample events")
          
        } else {
          
          rls_data <- input_list$out_df %>%
            select(any_of(req_cols)) %>%
            distinct()
          
          rls_sample_events <- marinegeo.utils::utl_rls_sample_event_summary(rls_data) %>%
            select(sample_event_id, site_code, date, depth)
          
          if(str_starts(input_list$project_directory, "EPA-project/")){
            proj_roster <- "EPA-South-Florida"
          } else if(str_starts(input_list$project_directory, "PAFF-2025/")){
            proj_roster <- "PAFF-2025"
          }
          
          df <- left_join(rls_sample_events, load_rls_roster()[[proj_roster]], 
                          by = c("sample_event_id", "site_code"))
          
        }
        
        df %>%
          select(-any_of("sample_event_id")) %>%
          DT::datatable(
            style = "default"
          )
        
      })
      
      output$all_sample_events <- renderDT({
        
        req(input_list$selected_flag)
        
        req_cols <- c("sample_event_id", "site_code", "date", "depth", "method", "block", "input_filename")
        
        if(!"sample_event_id" %in% colnames(input_list$out_df)){ 
          df <- tibble(status = "add sample event ID column!")
          
          # } else if(!is.Date(input_list$out_df$date) | !is.numeric(input_list$out_df$method) | !is.numeric(input_list$out_df$block)){
          #   df <- tibble(status = "check data type of date, method, or block to evaluate sample events")
          
        } else {
          
          site_codes <- unique(input_list$out_df$site_code)
          
          rls_data <- bind_rows(
            input_list$out_df %>%
              select(any_of(req_cols)) %>%
              distinct(), 
            
            load_rls_l2_data() %>%
              filter(input_filename != input_list$data_filename) %>%
              filter(site_code %in% site_codes)
          )
          
          rls_sample_events <- marinegeo.utils::utl_rls_sample_event_summary(rls_data)
          
          if(str_starts(input_list$project_directory, "EPA-project/")){
            proj_roster <- "EPA-South-Florida"
          } else if(str_starts(input_list$project_directory, "PAFF-2025/")){
            proj_roster <- "PAFF-2025"
          }
          
          df <- left_join(rls_sample_events, 
                          load_rls_roster()[[proj_roster]],
                          by = c("sample_event_id", "site_code"))
        }
        
        df %>%
          select(-any_of("sample_event_id")) %>%
          DT::datatable(
            style = "default"
          )
      })
      
    }
  )
}