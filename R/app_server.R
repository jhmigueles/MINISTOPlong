#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic 
  
  mod_ilrs4_server("ilrs4_ui_1")
  # mod_piechart4_server("piechart4_ui_1", reactive(input$ilr1))
}
