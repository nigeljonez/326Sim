#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("326 ARIMA Simulator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput(paste("ar1val"),paste("AR1"), min = 0, max = 1.0, value = 0.5),
         sliderInput(paste("ar2val"),paste("AR2"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ar3val"),paste("AR3"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ar4val"),paste("AR4"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma1val"),paste("MA1"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma2val"),paste("MA2"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma3val"),paste("MA3"), min = 0, max = 1.0, value = 0),
         sliderInput(paste("ma4val"),paste("MA4"), min = 0, max = 1.0, value = 0)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("arWarning"),
        tabsetPanel(
          tabPanel("Plots",
                    plotOutput("arimaplots", height="750px")
                   ),
          tabPanel("ARIMA",
                   sliderInput("arimaar", "# of ARs", min = 0, max = 4, value = 1),
                   sliderInput("arimama", "# of MAs", min = 0, max = 4, value = 0),
                   verbatimTextOutput("arimacmd")
                   )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  arsim <- reactive({
    if ((input$ar1val + input$ar2val + input$ar3val + input$ar4val) < 1) {
      arima.sim(model = list(
        ar = sapply(1:4, function(i) { input[[paste0("ar", i, "val")]]}),
        ma = sapply(1:4, function(i) { input[[paste0("ma", i, "val")]]})
      ),
      n = 1000)
    } else {return()}
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
   
   output$arWarning <- renderText({
     if ((input$ar1val + input$ar2val + input$ar3val + input$ar4val) >= 1) {
       "AR terms MUST sum to less than 1 - non stationary otherwise."
     } else {
       ""
     }
   })
   
   output$arimaplots <- renderPlot({
     if (length(arsim()) == 1000) {
       par(mfrow=c(3,1))
       plot.ts(arsim(), main="ARIMA Sim")
       acf(arsim())
       pacf(arsim())
     }
   })
   
   output$arimacmd <- renderPrint({
     if (length(arsim()) == 1000) {
      arima(arsim(), order=c(input$arimaar, 0, input$arimama))
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

