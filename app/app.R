
# This app wraps around code for identifying user IDs and generating access tokens for ROAR v2 fitbit users.
# load code

source("R/authorization-flow.R")
library(rclipboard)



# Define UI 
ui <- fluidPage(
  tags$style(".butt{background:#000099;} .butt{color: #337ab7;}"),
  # App title ----
  titlePanel("FitBit user consenting"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      
      textInput(inputId = "authresponse",
                label = "Authorization redirect url",
                placeholder = 'Paste URL from "error" page here')


  ),

  mainPanel(
  rclipboardSetup(),
  p("Once the patient has logged in to fitbit, please copy this url into a new window in the browser. Once the patient has consented, copy the url from the redirected page into the box."),
  
  uiOutput("clip"),
  textInput("copytext", "Copy this:", test.auth.url),
  
  tableOutput("accessinfo"),
  shiny::downloadButton(outputId ="downloadData",
                 label = "Download access information", 
                 class = "butt")
  
  )
  
  
  )
  
  # Main panel for displaying outputs ----
  
)


# Define server 
server <- function(input, output) {
  
  output$clip <- renderUI({
    rclipButton(inputId = "clipbtn",
                label = "rclipButton Copy",
                clipText = input$copytext,
                icon = icon("clipboard"))
  })
  
  accessdat <- reactive({req(input$authresponse)
                        grabAccessInfo(input$authresponse)})

  
  # observeEvent(input$go, {
  output$accessinfo <- renderTable({
    accessdat()
  })
  # })
  output$downloadData <- downloadHandler(
    filename = paste0(accessdat()$user_id, ".csv"),
    content = function(file) {
      write.csv(accessdat(), file, row.names = FALSE)
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)