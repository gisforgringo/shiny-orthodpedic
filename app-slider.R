# Import libraries
library(shiny)
library(data.table)
library(caret)

library(tidyverse)

library(data.table)

library(dplyr)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
url_file <- 'https://raw.github.com/gisforgringo/harvard/master/column_3C_weka.csv'

df <-read.csv(url_file)


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Orthopedic Disease Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("pelvic_incidence", label = "pelvic incidence", value = 5.0,
                min = min(df$Sepal.Length),
                max = max(df$Sepal.Length)
    ),
    sliderInput("pelvic_tilt", label = "pelvic tilt", value = 3.6,
                min = min(df$pelvic_tilt),
                max = max(df$pelvic_tilt)),
    sliderInput("lumbar_lordosis_angle", label = "lumbar lordosis angle", value = 1.4,
                min = min(df$lumbar_lordosis_angle),
                max = max(df$lumbar_lordosis_angle)),
    sliderInput("sacral_slope", label = "sacral slope", value = 0.2,
                min = min(df$sacral_slope),
                max = max(df$sacral_slope)),
    sliderInput("pelvic_radius", label = "pelvic radius", value = 0.2,
                min = min(df$pelvic_radius),
                max = max(df$pelvic_radiush)),
    sliderInput("degree_spondylolisthesis", label = "degree spondylolisthesis", value = 0.2,
                min = min(df$degree_spondylolisthesis),
                max = max(df$degree_spondylolisthesis)),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
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

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("pelvic incidence",
               "pelvic tilt",
               "lumbar lordosis angle",
               "sacral slope", 
               "pelvic radius", 
               "degree spondylolisthesis"),
      Value = as.character(c(input$pelvic_incidence,
                             input$pelvic_tilt,
                             input$lumbar_lordosis_angle,
                             input$sacral_slope,
                             input$pelvic_radius,
                             input$degree_spondylolisthesis)),
      stringsAsFactors = FALSE)
    
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)