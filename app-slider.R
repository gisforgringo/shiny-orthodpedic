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

library(tidyr)

library(shinythemes)

df <- df %>% drop_na()


####################################
# User interface                   #
####################################
print("hello ui")

ui <- fluidPage(theme = shinytheme("united"), 
                

  
  
  pageWithSidebar(
  
  
  # Page header
  headerPanel('Orthopedic Disease Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("pelvic_incidence", label = "pelvic incidence",  
                value = (min(df$pelvic_incidence)+max(df$pelvic_incidence))/2,
                min = min(df$pelvic_incidence),
                max = max(df$pelvic_incidence)
    ),
    sliderInput("pelvic_tilt", label = "pelvic tilt", 
                value = (max(df$pelvic_tilt)+min(df$pelvic_tilt))/2,
                min = min(df$pelvic_tilt),
                max = max(df$pelvic_tilt)),
    sliderInput("lumbar_lordosis_angle", label = "lumbar lordosis angle", 
                value = 74,
                min = min(df$lumbar_lordosis_angle),
                max = max(df$lumbar_lordosis_angle)),
    sliderInput("sacral_slope", label = "sacral slope", 
                value = 74,
                min = min(df$sacral_slope),
                max = max(df$sacral_slope)),
    sliderInput("pelvic_radius", label = "pelvic radius", 
                value = 120,
                min = min(df$pelvic_radius),
                max = max(df$pelvic_radius)),
    sliderInput("degree_spondylolisthesis", label = "degree spondylolisthesis", value = 200,
                min = min(df$degree_spondylolisthesis),
                max = max(df$degree_spondylolisthesis)),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
  
 
), # end slide bar panel




tabPanel("About", 
         titlePanel("About"), 
         div(includeMarkdown("project_two.html"), 
             align="justify")
) #tabPanel(), About


) # end fluid page

####################################
# Server                           #
####################################


print('hello server')
server<- function(input, output, session) {
  
  input
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("pelvic_incidence",
               "pelvic_tilt",
               "lumbar_lordosis_angle",
               "sacral_slope", 
               "pelvic_radius", 
               "degree_spondylolisthesis"),
      Value = as.character(c(input$pelvic_incidence,
                             input$pelvic_tilt,
                             input$lumbar_lordosis_angle,
                             input$sacral_slope,
                             input$pelvic_radius,
                             input$degree_spondylolisthesis)),
      stringsAsFactors = FALSE)
    
    df
    
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    print("test")
    print(test)
    print('end test')
    
   
    
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