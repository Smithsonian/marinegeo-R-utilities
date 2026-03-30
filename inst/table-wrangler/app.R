library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(lubridate)
library(purrr)
library(readxl)
library(DT) # Interactive Table
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(marinegeo.utils)
library(leaflet)
library(sf)
library(jsonlite)

purrr::walk(
  list.files(file.path("R", "visualizations"), pattern = "\\.R$", full.names = TRUE),
  source
)

ui <- page_navbar(
  title = "MarineGEO Table Wrangler",
  id = "main_navbar",
  fillable = TRUE,
  useShinyjs(),
  # theme = bs_theme(bootswatch = "solar"),

  # Javascript to redirect tab to indentation within a text box
  # Instead of moving active cursor to  next element

  tags$head(
    tags$script(HTML("
      $(document).on('keydown', 'textarea', function(e) {
        if (e.key == 'Tab') {
          e.preventDefault();
          var start = this.selectionStart;
          var end = this.selectionEnd;

          // Insert tab character
          this.value = this.value.substring(0, start) + '\\t' +
                      this.value.substring(end);

          // Set cursor position after the inserted tab
          this.selectionStart = this.selectionEnd = start + 1;
        }
      });
    "))
  ),
  
  nav_panel(title = "Load Data",
            load_marinegeo_data_UI("load_data")
  ),
  
  nav_panel(title = "Vocabulary",
    vocabulary_UI("vocabulary")
  )
)

server <- function(input, output, session) {
  
  vocabulary_server("vocabulary")
  loaded_bundle <- load_marinegeo_data_server("load_data")
  tab_counter <- reactiveVal(0L)

  observeEvent(loaded_bundle(), {
    req(loaded_bundle())
    bundle <- loaded_bundle()

    n <- tab_counter() + 1L
    tab_counter(n)

    tab_id <- paste0("tab_", n)
    tab_label <- basename(bundle$script_filepath)
    if (nchar(tab_label) > 35) tab_label <- paste0(substr(tab_label, 1, 32), "...")

    nav_insert(
      id = "main_navbar",
      nav = nav_panel(title = tab_label, value = tab_id,
                      mod_table_instance_UI(tab_id)),
      target = "Load Data",
      position = "after",
      select = TRUE,
      session = session
    )

    mod_table_instance_server(tab_id, bundle)

  }, ignoreInit = TRUE)

}

shinyApp(ui, server)
