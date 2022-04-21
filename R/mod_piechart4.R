#' piechart4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_piechart4_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      plotly::plotlyOutput(ns("plot"))
    )
  )
}

#' piechart4 Server Functions
#'
#' @noRd 
mod_piechart4_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # data for ilr1 changes ---------------------------------------------------
    m4 = c(10.12452, 54.14555, 347.96264, 513.26918, 514.49811)
    sbp <- matrix(c(  1, -1, -1, -1, -1,
                      0,  1, -1, -1, -1,
                      0,  0,  1, -1, -1,
                      0,  0,  0,  1, -1),
                  ncol = 5, byrow = TRUE)
    psi <- compositions::gsi.buildilrBase(t(sbp))
    
    ilr.m4 = compositions::ilr(m4, V = psi)
    a = reactive(ilrs4$ilr1())
    b = reactive((1440 - (m4[1]*a())) / (m4[2]+m4[3]+m4[4]+m4[5]))
    
    # Plot
    output$plot = plotly::renderPlotly({
      data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                      values = c(m4[1]*a(),
                                                 m4[2]*b(),
                                                 m4[3]*b(),
                                                 m4[4]*b(),
                                                 m4[5]*b())))
      
      p = plotly::plot_ly(data_plot(), labels = ~x, values = ~values, 
                  type = 'pie',
                  # textposition = 'outside',
                  # textinfo = 'values',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = ~paste0(compo_names, " (", ~round(values), " min/day)"),
                  # text = ~paste0(compo_names, " (", round(values), " min/day)"),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2)),
                  #The 'pull' attribute can also be used to create space between the sectors
                  showlegend = FALSE)
      
      p = plotly::layout(layout(title = 'Movement behaviors at 4 years old',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      )
      p
    })
    
  })
}

## To be copied in the UI
# mod_piechart4_ui("piechart4_ui_1")

## To be copied in the server
# mod_piechart4_server("piechart4_ui_1")
