library(shiny)
library(shinyBS)
library(leaflet)
library(dplyr)
library(tidyr)
library(caret)



head(data)

# Load model
load("/Users/stantaov/model.Rdata");

# Define UI for app 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Assignment 1"),


  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input:
      sliderInput(inputId = "precipitac",
                  label = "Precipitacion:",
                  min = 0,
                  max = 100,
                  value = 1),
      
      sliderInput(inputId = "tempmax",
                  label = "Max Temperature:",
                  min = 10,
                  max = 40,
                  value = 1),
      
      sliderInput(inputId = "tempmin",
                  label = "Min Temperature:",
                  min = 5,
                  max = 25,
                  value = 1),
      
      sliderInput(inputId = "tempmed",
                  label = "Average Temperature:",
                  min = 10,
                  max = 30,
                  value = 1),
      
      sliderInput(inputId = "umidade",
                  label = "Humidity:",
                  min = 40,
                  max = 100,
                  value = 1),
      
      sliderInput(inputId = "insolacao",
                  label = "Heatstroke:",
                  min = 0,
                  max = 12,
                  value = 1),
      
      selectInput(inputId = "day_number", "Day of The Week:",
                size = 7, selectize=FALSE,
                  c("Moday" = 1,
                    "Tuesday" = 2,
                    "Wednesday" = 3,
                    "Tuesday" = 4,
                    "Friday" = 5,
                    "Saturday" = 6,
                    "Sunday" = 0)),
      
      selectInput(inputId = "month", "Month of The Year:",
                size = 12, selectize=FALSE, 
                  c("January" = 1,
                    "February" = 2,
                    "March" = 3,
                    "April" = 4,
                    "May" = 5,
                    "June" = 6,
                    "July" = 7,
                    "August" = 8,
                    "September" = 9,
                    "October" = 10,
                    "Novemeber" = 11,
                    "December" = 12)) 
      )
    ))

server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  inputData <- reactive({
    data.frame(
      name = c("precipitac",  "tempmax", "tempmin", "tempmed", "umidade",
               "insolacao", "month", "day_number"),

      value = as.numeric(c(input$precipitac, input$tempmax, input$tempmin,
                           input$tempmed, input$umidade, input$insolacao, input$day_number,
                           input$month ))
      )
    })
  
  # Use model on input data
  output$prediction <- reactive({
    data <- spread(inputData(), name, value)
    #data <- as.integer(data)
    outcome <- predict(model, data = data)
  })
  
  
}


shinyApp(ui, server)
