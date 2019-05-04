#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(s20x)

arterms = factor()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput(paste("ar1val"),paste("AR1"), min = 0, max = 1.0, value = 0.5),
         sliderInput(paste("ar2val"),paste("AR2"), min = 0, max = 1.0, value = 0.1),
         sliderInput(paste("ar3val"),paste("AR3"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ar4val"),paste("AR4"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma1val"),paste("MA1"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma2val"),paste("MA2"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma3val"),paste("MA3"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma4val"),paste("MA4"), min = 0, max = 1.0, value = 0)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("tsPlot"),
        plotOutput("acfPlot"),
        plotOutput("pacfPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  arsim <- reactive({
      arima.sim(model = list(
        ar = sapply(1:4, function(i) { input[[paste0("ar", i, "val")]]}),
        ma = sapply(1:4, function(i) { input[[paste0("ma", i, "val")]]})
        #ar = c(input$ar1val, input$ar2val)
        #ar = c(input$ar1val)
        #ar = if(input$arterms > 0) { sapply(1:input$arterms, function(i) { input$ar1val } ) } else { c(0) },
        #ma = if(input$materms > 0) { sapply(1:input$materms, function(i) { input$ma1val } ) } else { c(0) }
      ),
      n = 1000)
    #arima.sim(model = list(ar = c(input$ar1val, input$ar2val, input$ar3val, input$ar4val),
    #                       ma = c(input$ma1val, input$ma2val, input$ma3val, input$ma4val)),
    #          n = 1000)
    #arima.sim(model = list(ar = c(input$ar1val)), n = 1000)
  })
  
  output$arcomps <- renderUI({
    if(input$arterms > 0) {
      lapply(1:input$arterms, function(i) {
        sliderInput(paste("ar", i, "val"),paste("AR", i), min = 0, max = 1.0, value = 0.5)
      })
    }
  })
  output$macomps <- renderUI({
    if(input$materms > 0) {
      lapply(1:input$arterms, function(i) {
        sliderInput(paste("ma", i, "val"),paste("MA", i), min = 0, max = 1.0, value = 0.5)
      })
    }
  })
   output$tsPlot <- renderPlot({
      plot.ts(arsim(), main=paste("ARIMA Sim"))
   })
   output$acfPlot <- renderPlot({
     acf(arsim())
   })
   output$pacfPlot <- renderPlot({
     pacf(arsim())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

