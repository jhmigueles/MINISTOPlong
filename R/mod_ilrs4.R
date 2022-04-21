#' ilrs4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ilrs4_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    shiny::sidebarPanel(
      div(
        radioButtons(inputId = ns("select"), 
                     choices = c("ILR 1", "ILR 2", "ILR 3", "ILR 4"), 
                     label = "What ILR do you want to change?", 
                     selected = "ILR 1", inline = T),
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr1"),
                           label = "ILR 1: VPA vs MPA·LPA·SB·Sleep",
                           min = 0.01229853, max = 1.98770147,
                           value = 1, step = 0.1039686)
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr2"),
                           label = "ILR 2: MPA vs LPA·SB·Sleep",
                           min = 0.4459378, max = 1.5540622,
                           value = 1, step = 0.05832233)
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr3"),
                           label = "ILR 3: LPA vs SB·Sleep",
                           min = 0.7413515, max = 1.2586485,
                           value = 1, step = 0.02722615 )
      ),
      hr(),
      div(
        shiny::sliderInput(inputId = ns("ilr4"),
                           label = "ILR 4: SB vs Sleep",
                           min = 0.4155114, max = 1.5844886,
                           value = 1, step = 0.06152512)
      )
    ),
    shiny::mainPanel(
      div(
        col_6(
          plotly::plotlyOutput(ns("plot"))
        )
      )
      
    )
    
  )
}

#' ilrs4 Server Functions
#'
#' @noRd 
mod_ilrs4_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$select, {
      # Enable/disable sliders
      shinyjs::toggleState(id = "ilr1", condition = input$select == "ILR 1")
      shinyjs::toggleState(id = "ilr2", condition = input$select == "ILR 2")
      shinyjs::toggleState(id = "ilr3", condition = input$select == "ILR 3")
      shinyjs::toggleState(id = "ilr4", condition = input$select == "ILR 4")
      
      # if a different ilr is selected, slider = 1
      if(input$select != "ILR 1") updateSliderInput(session = session, inputId = "ilr1", value = 1)
      if(input$select != "ILR 2") updateSliderInput(session = session, inputId = "ilr2", value = 1)
      if(input$select != "ILR 3") updateSliderInput(session = session, inputId = "ilr3", value = 1)
      if(input$select != "ILR 4") updateSliderInput(session = session, inputId = "ilr4", value = 1)
    })
    
    # Plot -----------------------------------------------------------------
    m4 = c(10.12452, 54.14555, 347.96264, 513.26918, 514.49811)
    compo_names = c("VPA", "MPA", "LPA", "SB", "Sleep")
    sbp <- matrix(c(  1, -1, -1, -1, -1,
                      0,  1, -1, -1, -1,
                      0,  0,  1, -1, -1,
                      0,  0,  0,  1, -1),
                  ncol = 5, byrow = TRUE)
    psi <- compositions::gsi.buildilrBase(t(sbp))
    
    ilr.m4 = compositions::ilr(m4, V = psi)
    
    
    # data for plot if ILR 1 modified
    observeEvent(input$select, {
      if (input$select == "ILR 1") {
        a = reactive(input$ilr1)
        b = reactive((1440 - (m4[1]*a())) / (m4[2] + m4[3] + m4[4] + m4[5]))
        
        data_plot = reactive(data.frame(x = c("VPA", "MPA", "LPA", "SB", "Sleep"),
                                        values = c(m4[1]*a(),
                                                   m4[2]*b(),
                                                   m4[3]*b(),
                                                   m4[4]*b(),
                                                   m4[5]*b())))
      }
      
      output$plot = plotly::renderPlotly({
        p = plotly::plot_ly(data_plot(), labels = ~x, values = ~values, 
                            type = 'pie',
                            # textposition = 'outside',
                            textinfo = "text",
                            insidetextfont = list(color = '#FFFFFF'),
                            # hoverinfo = "text",
                            hovertemplate = "<b>%{label}:</b>  %{text} <br>%{percent}</br><extra></extra>",
                            text = ~paste0(round(values), " min/day"),
                            marker = list(colors = colors,
                                          line = list(color = '#FFFFFF', width = 2)),
                            showlegend = FALSE)
        p = plotly::layout(p, title = 'Movement behaviors at 4 years old')
        p
      })
      
    })
    
    
    
    
    
    # return for use in other modules -----------------------------------------
    return(list(
      ilr1 = reactive(input$ilr1),
      ilr2 = reactive(input$ilr2), 
      ilr3 = reactive(input$ilr3), 
      ilr4 = reactive(input$ilr4)
    ))
    
  })
}

## To be copied in the UI
# mod_ilrs4_ui("ilrs4_ui_1")

## To be copied in the server
# mod_ilrs4_server("ilrs4_ui_1")
