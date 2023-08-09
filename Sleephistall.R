
library(shiny)

mydat = read.csv(file = "Sleep_health_and_lifestyle_dataset.csv",header = TRUE)
data(mydat)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sleep Health"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30), #default step = 1 , change the value, if step is 2 make min 0 for even steps
      
      # Input: Select input to select the parameter for sleep health
      selectInput("parameter","Select a parameter",choices = names(mydat))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    bins <- seq(min(mydat[,input$parameter]), max(mydat[,input$parameter]), length.out = input$bins + 1)
    hist(mydat[,input$parameter],breaks= bins,col = "pink", border = "black",
    main = "Histogram of selected parameter in Sleep Health Data",
    xlab = input$parameter)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
