# Shiny application for AR/MA/ARIMA generation
# MIT License - see LICENSE and About tab of running app
# for details

# Also contains a small amount of bootstrap CSS (also MIT)

library(shiny)
library(shinyjs)


opts <- seq(-0.9, 0.9, by = 0.05)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
   
   # Application title
   titlePanel("326 ARIMA Simulator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(div(id = "arimaapp",
      sidebarPanel(
        inputPanel(
          radioButtons("artype", "Type:", c("AR", "MA", "ARMA"), "AR", inline = TRUE)
        ),
        conditionalPanel(
          condition = "input.artype == 'AR' || input.artype == 'ARMA'",
          "AR Terms",
         sliderInput("ar1val",paste("AR1"), min = -0.9, max = 0.9, value = 0.5, step = 0.05),
         sliderInput("ar2val",paste("AR2"), min = 0, max = 0.45, value = 0, step = 0.025),
         sliderInput("ar3val",paste("AR3"), min = 0, max = 0.45, value = 0, step = 0.025),
         sliderInput("ar4val",paste("AR4"), min = 0, max = 0.45, value = 0, step = 0.025)
        ),
        conditionalPanel(
          condition = "input.artype == 'MA' || input.artype == 'ARMA'",
          "MA Terms",
         sliderInput(paste("ma1val"),paste("MA1"), min = -0.9, max = 0.9, value = 0, step = 0.05),
         sliderInput(paste("ma2val"),paste("MA2"), min = 0, max = 0.45, value = 0, step = 0.025),
         sliderInput(paste("ma3val"),paste("MA3"), min = 0, max = 0.45, value = 0, step = 0.025)
        ),
        actionButton("regenerate", "Re-generate!", icon = icon("sync"), style="color: #fff; background-color: #28a745; border-color: #28a745;"),
        actionButton("reset", "Reset", icon = icon("undo"), style="color: #fff; background-color: #dc3545; border-color: #dc3545;")
      )),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("arWarning"),
        tabsetPanel(
          tabPanel("Plots",
                    plotOutput("arimaplots", height="750px")
                   ),
          tabPanel("ARIMA",
                   sliderInput("arimaar", "# of ARs", min = 0, max = 4, value = 1),
                   sliderInput("arimama", "# of MAs", min = 0, max = 3, value = 0),
                   verbatimTextOutput("arimacmd")
                   ),
          tabPanel("About",
            div('Created with',
                a(href='https://shiny.rstudio.com/', 'Shiny'),
                'by N Jones,', 
                a(href='https://github.com/snjnz/stats326sim', 
                  'Open Source on GitHub'),
                'under the MIT License'))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  arimaval <- reactiveVal(list(ar=c(0.5)))
  update <- reactiveVal(TRUE)
  observe({
    update(FALSE)
    setSliderU <- function(inputname, inputtype, inputnum) {
      res <- logical(length(opts))
      terms = (-1)*sapply(1:(inputnum-1), function(i) { 
        input[[paste0(inputtype, i, "val")]] +
          ifelse((inputnum-1) == i, 0.0001, 0)
        })
      for (i in 1:length(opts))
        res[i] <- all(Mod(polyroot(c(1, terms, -opts[i]))) > 1.000001)
      if (all(res == TRUE)) {
        updateSliderInput(session, inputname, min = -0.9, max = 0.9)
        return(TRUE)
      } else if (all(res == FALSE)) {
        updateSliderInput(session, inputname, value = 0)
        return(FALSE)
      } else {
        updateSliderInput(session, inputname, min = min(opts[res]), max = max(opts[res]))
        return(TRUE)
      }
    }
    shinyjs::toggleState("ar2val", (input$ar1val != 0) && setSliderU("ar2val", "ar", 2))
    shinyjs::toggleState("ar3val", (input$ar2val != 0) && setSliderU("ar3val", "ar", 3))
    shinyjs::toggleState("ar4val", (input$ar3val != 0) && setSliderU("ar4val", "ar", 4))
    shinyjs::toggleState("ma2val", (input$ma1val != 0) && setSliderU("ma2val", "ma", 2))
    shinyjs::toggleState("ma3val", (input$ma2val != 0) && setSliderU("ma3val", "ma", 3))
    update(TRUE)
  })
  
  observeEvent(input$reset, {
    update(TRUE)
    shinyjs::reset("arimaapp")
  })
  
  arsim <- reactive({
    input$regenerate
    if (update())
      x = list()
      if (grepl("AR", input$artype)) {
        x$ar <- sapply(1:4, function(i) { input[[paste0("ar", i, "val")]]})
        if (any(x$ar == 0)) {
          x$ar[which.min(x$ar != 0):4] <- 0
        }
      }
      
      if (grepl("MA", input$artype)) {
        x$ma <- sapply(1:3, function(i) { input[[paste0("ma", i, "val")]]})
        if (any(x$ma == 0)) {
          x$ma[which.min(x$ma != 0):3] <- 0
        }
      }
      arima.sim(model = x, n = 1000)
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
     req((length(arsim()) == 1000))
     par(mfrow=c(3,1))
     plot.ts(arsim(), main="ARIMA Sim")
     acf(arsim())
     pacf(arsim())
   })
   
   output$arimacmd <- renderPrint({
     req(length(arsim()) == 1000)
     arima(arsim(), order=c(input$arimaar, 0, input$arimama))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

