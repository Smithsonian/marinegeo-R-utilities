#Row- based test details popup from the application sidebar. (Design may change later pending review)


qc_flag_details_UI <- function(id) {
    ns <- NS(id)
    tagList(
      uiOutput(ns("details_button"))
    )
}


qc_flag_details_server <- function(id, input_list) {
  moduleServer(
    id,
    function(input, output, session) {
      #Appearance of the button only when a specific flag is selected
      output$details_button <- renderUI({
        req(input_list$selected_flag)

        # enforce conditions
        if (!is.null(input_list$selected_flag) &&
            input_list$selected_flag != "no_flags" &&
            input_list$selected_flag != "all") {

          actionButton(session$ns("show_details"), "Show Flag Details")
        }
      })


      # Functionality of the Button
      observeEvent(input$show_details, {

        #get the table with the QC test details
        table <- utl_get_qc_details(input_list)
        #Render the table in shiny format
        output$flag_table <- renderTable({
          table
        })

        #Create the pop-up.
        showModal(
          modalDialog(
            title = paste("Flag:", input_list$selected_flag),
            tableOutput(session$ns("flag_table")), #This is where we will put all the content later
            easyClose = TRUE
          )
        )
      })
    }
  )
}
