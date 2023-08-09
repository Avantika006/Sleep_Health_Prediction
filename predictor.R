# Import libraries
library(shiny)
library(randomForest)
library(caret)

# Read the data and preprocess it
mydat <- read.csv(file = "Sleep_health_and_lifestyle_dataset.csv", header = TRUE)
dat <- data.frame(
  Age = mydat$Age,
  Sleep.Duration = mydat$Sleep.Duration,
  Heart.Rate = mydat$Heart.Rate,
  Daily.Steps = mydat$Daily.Steps,
  Sleep.Disorder = mydat$Sleep.Disorder
)
dat$Sleep.Disorder <- ifelse(dat$Sleep.Disorder == "None", "No_Disorder", dat$Sleep.Disorder)
dat$Sleep.Disorder <- factor(dat$Sleep.Disorder, levels = c("No_Disorder", "Insomnia", "Sleep Apnea"))

# Performs stratified random split of the data set
set.seed(123) # For reproducibility
TrainingIndex <- createDataPartition(dat$Sleep.Disorder, p = 0.8, list = FALSE)
TrainingSet <- dat[TrainingIndex,] # Training Set
TestingSet <- dat[-TrainingIndex,] # Test Set

# Build the Random Forest model
# Specify the response variable and predictor variables
response_var <- "Sleep.Disorder"
predictor_vars <- setdiff(names(dat[, 1:4]), response_var)

model <- randomForest(
  formula = as.formula(paste(response_var, "~", paste(predictor_vars, collapse = "+"))),
  data = TrainingSet,
  ntree = 500,
  mtry = 4
)

# Save model to RDS file
saveRDS(model, "model.rds")

####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  # Page header
  headerPanel('Sleep Disorder Predictor'),
  
  # Input values
  sidebarPanel(
    tags$label(h3('Input parameters')),
    numericInput("mydat.Age", 
                 label = "Age:", 
                 value = 40),
    numericInput("mydat.Sleep.Duration", 
                 label = "Sleep Duration:", 
                 value = 6),
    numericInput("mydat.Heart.Rate", 
                 label = "Heart Rate:", 
                 value = 78),
    numericInput("mydat.Daily.Steps", 
                 label = "Daily Steps:", 
                 value = 8000),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Prepare input data and make predictions
  datasetInput <- reactive({  
    
    df <- data.frame(
      Age = input$mydat.Age,
      Sleep.Duration = input$mydat.Sleep.Duration,
      Heart.Rate = input$mydat.Heart.Rate,
      Daily.Steps = input$mydat.Daily.Steps
    )
    
    # Print the data frame for debugging
    print(df)
    
    # Make sure the input data columns match the training data columns
    predictor_vars <- setdiff(names(dat[, 1:4]), response_var)
    df <- df[, predictor_vars, drop = FALSE]
    
    # Make predictions using the model
    prediction <- predict(model, df)
    probabilities <- predict(model, df, type = "prob")
    
    # Combine predictions and probabilities into a data frame
    Output <- data.frame(Prediction = prediction, round(probabilities, 3))
    
    return(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
